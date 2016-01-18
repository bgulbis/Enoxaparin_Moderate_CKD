# exclude.R
# 
# determine which patients should be excluded

source("library.R")

exclude.dir <- "Exclusion"

# compress data files
gzip_files(exclude.dir)

# read in all data files needed to evaluate for exclusion
raw.excl.demograph <- list.files(exclude.dir, pattern="^demographics", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              age = as.numeric(Age..Years..Visit.),
              sex = factor(Sex, exclude = c("", "Unknown")),
              race = factor(Race, exclude = c("", "Unknown")))

raw.excl.diagnosis <- list.files(exclude.dir, pattern="^diagnosis", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              diag.code = ICD9.Diagnosis.Code,
              diag.type = factor(Diagnosis.Type, exclude = ""),
              diag.seq = factor(Diagnosis.Code.Sequence)) 

raw.excl.weight <- list.files(exclude.dir, pattern="^ht_wt", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event),
              result = as.numeric(Clinical.Event.Result),
              unit = factor(Clinical.Event.Result.Units)) 

raw.excl.labs <- list.files(exclude.dir, pattern="^labs", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event),
              result = Clinical.Event.Result)
              
raw.excl.anticoag <- list.files(exclude.dir, pattern="^anticoagulants", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event),
              dose = as.numeric(Clinical.Event.Result),
              dose.unit = factor(Clinical.Event.Result.Units, exclude = ""),
              rate = as.numeric(Infusion.Rate),
              rate.unit = factor(Infusion.Rate.Unit, exclude = ""),
              route = factor(Route.of.Administration...Short, exclude = "")) 

raw.excl.enox <- list.files(exclude.dir, pattern="^enoxaparin", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event),
              dose = as.numeric(Clinical.Event.Result),
              dose.unit = factor(Clinical.Event.Result.Units, exclude = ""),
              rate = as.numeric(Infusion.Rate),
              rate.unit = factor(Infusion.Rate.Unit, exclude = ""),
              route = factor(Route.of.Administration...Short, exclude = ""),
              frequency = factor(Parent.Order.Frequency.Description)) 

# evaluate for inclusion criteria: at least 3 doses of enoxaparin over 72 hours
end_eval <- function(dt) {
    max.stop <- first(dt) + days(5)
    
    if (last(dt) < max.stop) {
        last(dt)
    } else {
        max.stop
    }
}

# find those with >= 3 doses and >= 72 hours of treatment
tmp.enox.courses <- raw.excl.enox %>%
    filter(dose > 40) %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    mutate(freq = ifelse(str_detect(frequency, regex("(q12h|bid)", ignore_case = TRUE)) == TRUE, "twice", 
                         ifelse(str_detect(frequency, regex("(q24h|daily)", ignore_case = TRUE)) == TRUE, "once", "")),
           hours.prev.dose = difftime(event.datetime, lag(event.datetime), units = "hours"),
           course.start = ifelse(is.na(hours.prev.dose) | 
                                     (freq == "twice" & hours.prev.dose > 36) | 
                                     hours.prev.dose > 48, TRUE, FALSE),
           course.count = cumsum(course.start)) %>%
    ungroup %>%
    group_by(pie.id, course.count) %>%
    summarize(first.dose = first(event.datetime),
              last.dose = last(event.datetime),
              end.datetime = end_eval(event.datetime),
              dose.count = n()) %>%
    mutate(duration = difftime(last.dose, first.dose, units = "hours")) %>%
    ungroup %>%
    group_by(pie.id) %>%
    filter(course.count == 1,
           duration >= 72,
           dose.count >= 3)
    
pts.include <- tmp.enox.courses$pie.id
    
# exclusion: pregnancy, weight < 45 or > 150, >1 CrCl <30 during enoxaparin,
# coadmin of other anticoags except warfarin, enoxaparin dose change by >10%

# find any patients with a weight < 45 kg or > 150 kg
tmp.weight <- raw.excl.weight %>%
    filter(pie.id %in% pts.include,
           event == "Weight",
           unit == "kg") %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    left_join(tmp.enox.courses, by = "pie.id") %>%
    filter(event.datetime <= first.dose + hours(2)) %>%
    summarize(weight = last(result)) %>%
    filter(weight > 45 & weight < 150)

excl.weight <- pts.include[! pts.include %in% tmp.weight$pie.id]

pts.include <- pts.include[! pts.include %in% excl.weight]

# find patients with severe renal impairment: >1 CrCl <30 mL/min within 48h of
# enoxaparin start

# find SCr to use for CrCl calculation
tmp.height <- raw.excl.weight %>%
    filter(pie.id %in% pts.include,
           event == "Height",
           unit == "cm") %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    left_join(tmp.enox.courses, by = "pie.id") %>%
    filter(event.datetime <= first.dose + hours(2)) %>%
    summarize(height = last(result))

tmp.crcl <- raw.excl.labs %>%
    filter(pie.id %in% pts.include,
           event == "Creatinine Lvl") %>%
    mutate(result = as.numeric(result)) %>%
    filter(!is.na(result)) %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    left_join(tmp.enox.courses, by = "pie.id") %>%
    filter(event.datetime >= first.dose - days(2),
           event.datetime <= end.datetime) %>%
    select(pie.id:result) %>%
    inner_join(select(raw.excl.demograph, pie.id, age, sex), by = "pie.id") %>%
    inner_join(tmp.weight, by = "pie.id") %>%
    inner_join(tmp.height, by = "pie.id") %>%
    rowwise %>%
    mutate(crcl = calculate_crcl(age, as.character(sex), result, weight, height),
           crcl.lt30 = ifelse(crcl < 30, TRUE, FALSE),
           crcl.30_60 = ifelse(crcl >= 30 & crcl <= 60, TRUE, FALSE)) %>%
    ungroup %>%
    group_by(pie.id) %>%
    summarize(num.crcl.lt30 = sum(crcl.lt30),
              num.crcl.30_60 = sum(crcl.30_60)) %>%
    filter(num.crcl.lt30 <= 1)
    
# remove any patients crcl < 30
excl.crcl <- pts.include[! pts.include %in% tmp.crcl$pie.id]
    
pts.include <- pts.include[! pts.include %in% excl.crcl]
