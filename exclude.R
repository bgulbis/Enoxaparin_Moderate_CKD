# exclude.R
# 
# determine which patients should be excluded

source("library.R")

exclude.dir <- "Exclusion"

# compress data files
gzip_files(exclude.dir)

# raw data ---------------------------------------------------------------------
# read in all data files needed to evaluate for exclusion
raw.excl.demograph <- list.files(exclude.dir, pattern="^demographics", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              person.id = Person.ID,
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

raw.excl.warf <- list.files(exclude.dir, pattern="^warfarin", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              indication = Clinical.Event.Result)

raw.excl.procedure <- list.files(exclude.dir, pattern="^procedure", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              procedure.date = mdy_hms(Procedure.Date.and.Time),
              procedure.code = ICD9.Procedure.Code)

# inclusion --------------------------------------------------------------------
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
    select(-rate, -rate.unit, -route) %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    mutate(frequency = as.character(frequency),
           frequency = ifelse(frequency != "Not Defined", frequency, 
                              ifelse(!is.na(lead(frequency)), lead(frequency), 
                                     ifelse(!is.na(lag(frequency)), lag(frequency), frequency))),
           freq = ifelse(str_detect(frequency, regex("(q12h|bid)", ignore_case = TRUE)) == TRUE, "twice", 
                         ifelse(str_detect(frequency, regex("(q24h|daily)", ignore_case = TRUE)) == TRUE, "once", "")),
           hours.prev.dose = difftime(event.datetime, lag(event.datetime), units = "hours"),
           course.start = ifelse(is.na(hours.prev.dose) | 
                                     (freq == "twice" & hours.prev.dose > 36) | 
                                     hours.prev.dose > 48, TRUE, FALSE),
           course.count = cumsum(course.start)) %>%
    filter(dose > 40) %>%
    ungroup %>%
    group_by(pie.id, course.count) %>%
    summarize(first.datetime = first(event.datetime),
              last.datetime = last(event.datetime),
              end.datetime = end_eval(event.datetime),
              dose.count = n(),
              freq = first(freq)) %>%
    mutate(duration = difftime(last.datetime, first.datetime, units = "hours")) %>%
    ungroup %>%
    group_by(pie.id) %>%
    filter(course.count == 1, duration >= 72, dose.count >= 3) %>%
    # filter(duration >= 72, dose.count >= 3) %>%
    arrange(course.count) %>%
    summarize(course.count = first(course.count),
              first.datetime = first(first.datetime),
              last.datetime = first(last.datetime),
              end.datetime = first(end.datetime),
              dose.count = first(dose.count),
              duration = first(duration),
              freq = first(freq))
    
pts.include <- tmp.enox.courses$pie.id
    
# exclusion --------------------------------------------------------------------
# criteria: pregnancy, weight < 45 or > 150, >1 CrCl <30 during enoxaparin,
# coadmin of other anticoags except warfarin, enoxaparin dose change by >10%

# find patients with >10% dose change for q12h, >15% for q24h
tmp.dose.change <- raw.excl.enox %>%
    inner_join(tmp.enox.courses, by = "pie.id") %>%
    filter(dose > 40,
           event.datetime >= first.datetime,
           event.datetime <= end.datetime) %>%
    group_by(pie.id) %>%
    summarize(first.dose = first(dose),
              last.dose = last(dose),
              freq = first(freq)) %>%
    mutate(dose.diff = last.dose / first.dose) %>%
    filter((freq == "once" & dose.diff >= 0.85 & dose.diff <= 1.15) | 
               (dose.diff >= 0.9 & dose.diff <= 1.10)) 

excl.dose <- pts.include[! pts.include %in% tmp.dose.change$pie.id]

pts.include <- pts.include[! pts.include %in% excl.dose]

# find any patients with a weight < 45 kg or > 150 kg
tmp.weight <- raw.excl.weight %>%
    filter(pie.id %in% pts.include,
           event == "Weight",
           unit == "kg") %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    left_join(tmp.enox.courses, by = "pie.id") %>%
    filter(event.datetime <= first.datetime + hours(2)) %>%
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
    filter(event.datetime <= first.datetime + hours(2)) %>%
    summarize(height = last(result))

tmp.crcl <- raw.excl.labs %>%
    filter(pie.id %in% pts.include,
           event == "Creatinine Lvl") %>%
    mutate(result = as.numeric(result)) %>%
    filter(!is.na(result)) %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    left_join(tmp.enox.courses, by = "pie.id") %>%
    filter(event.datetime >= first.datetime - days(2),
           event.datetime <= end.datetime) %>%
    select(pie.id:result) %>%
    inner_join(select(raw.excl.demograph, pie.id, age, sex), by = "pie.id") %>%
    inner_join(tmp.weight, by = "pie.id") %>%
    inner_join(tmp.height, by = "pie.id") %>%
    filter(!is.na(sex)) %>%
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

# remove any pregnant patients
tmp.labs.preg <- raw.excl.labs %>%
    filter(pie.id %in% pts.include,
           event == "S Preg" | event == "U Preg",
           result == "Positive") %>%
    select(pie.id) %>%
    distinct

excl.preg <- tmp.labs.preg$pie.id

ref.preg.codes <- read.csv("Lookup/exclusion_codes.csv", colClasses = "character")

ref.preg.icd9 <- icd9_lookup(ref.preg.codes)

tmp.diag.preg <- raw.excl.diagnosis %>%
    filter(pie.id %in% pts.include,
           diag.code %in% ref.preg.icd9$icd9.code) %>%
    select(pie.id) %>%
    distinct

excl.preg <- c(excl.preg, tmp.diag.preg$pie.id)

pts.include <- pts.include[! pts.include %in% excl.preg]

# find patients receiving other anticoagulants while on enoxaparin
tmp.anticoag <- raw.excl.anticoag %>%
    filter(pie.id %in% pts.include,
           event != "warfarin") %>%
    inner_join(tmp.enox.courses, by = "pie.id") %>%
    filter(event.datetime > first.datetime + hours(4),
           event.datetime < end.datetime) %>%
    select(pie.id) %>%
    distinct

excl.anticoag <- tmp.anticoag$pie.id

pts.include <- pts.include[! pts.include %in% excl.anticoag]

# make groups ------------------------------------------------------------------

# get age and indication for included patients
tmp.demograph <- raw.excl.demograph %>%
    filter(pie.id %in% pts.include)

ref.indications.codes <- read.csv("Lookup/anticoag_indications.csv", colClasses = "character")

ref.indications.icd9 <- icd9_lookup(ref.indications.codes)

# get indication based on diagnosis codes
tmp.indications <- raw.excl.diagnosis %>%
    filter(pie.id %in% pts.include) %>%
    inner_join(ref.indications.icd9, by = c("diag.code" = "icd9.code")) %>%
    mutate(disease.state = factor(disease.state),
           value = TRUE) %>%
    select(pie.id, disease.state, value) %>%
    group_by(pie.id, disease.state) %>%
    distinct %>%
    spread(disease.state, value, fill = FALSE, drop = FALSE)

# get indication based on warfarin indication data
tmp.warf.ind <- raw.excl.warf %>%
    filter(pie.id %in% pts.include) %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    summarize(indication = last(indication)) %>%
    mutate(afib = str_detect(indication, "Atrial fibrillation"),
           dvt = str_detect(indication, "Deep vein thrombosis"),
           pe = str_detect(indication, "Pulmonary embolism"),
           valve = str_detect(indication, "Heart valve"),
           other = str_detect(indication, "Other"))

# join both sets of indications together and merge
tmp.ind.join <- full_join(tmp.indications, tmp.warf.ind, by = "pie.id") %>% 
    group_by(pie.id) %>%
    mutate(afib = ifelse(sum(c(afib.x, afib.y), na.rm = TRUE) >= 1, TRUE, FALSE),
           dvt = ifelse(sum(c(dvt.x, dvt.y), na.rm = TRUE) >= 1, TRUE, FALSE),
           pe = ifelse(sum(c(pe.x, pe.y), na.rm = TRUE) >= 1, TRUE, FALSE),
           valve = ifelse(sum(c(valve.x, valve.y), na.rm = TRUE) >= 1, TRUE, FALSE),
           other = ifelse(sum(c(other.x, other.y), na.rm = TRUE) >= 1, TRUE, FALSE)) %>%
    select(pie.id, afib:other)

excl.indication <- pts.include[! pts.include %in% tmp.ind.join$pie.id]

# remove any patients where the indication is unknown
pts.include <- pts.include[!pts.include %in% excl.indication]

# check for any readmits

# edw.pie.include <- str_c(pts.include, collapse = ";")
# print(edw.pie.include)

# raw.readmits <- list.files(exclude.dir, pattern="^readmits", full.names=TRUE) %>%
#     lapply(read.csv, colClasses="character") %>%
#     bind_rows %>%
#     transmute(pie.id = PowerInsight.Encounter.Id,
#               person.id = Person.ID)

tmp.readmits <- tmp.demograph %>%
    group_by(person.id) %>%
    summarize(count = n()) %>%
    # filter(count > 1) %>%
    inner_join(tmp.demograph, by = "person.id") %>%
    # inner_join(tmp.demograph, by = "pie.id") %>%
    inner_join(tmp.enox.courses, by = "pie.id") %>%
    group_by(person.id) %>%
    summarize(pie.id = first(pie.id))

excl.readmit <- pts.include[! pts.include %in% tmp.readmits$pie.id]

# remove any patients where the indication is unknown
pts.include <- pts.include[!pts.include %in% excl.readmit]

# find moderate renal impairment patients
tmp.crcl <- tmp.crcl %>%
    filter(pie.id %in% pts.include)

group.moderate <- tmp.crcl %>%
    filter(num.crcl.30_60 > 1)

group.moderate <- group.moderate$pie.id

group.normal <- tmp.crcl %>%
    filter(num.crcl.30_60 <= 1)

group.normal <- group.normal$pie.id


# matching ---------------------------------------------------------------------
# match based on age and indication

tmp.pts.mod <- tmp.demograph %>%
    filter(pie.id %in% group.moderate) %>%
    inner_join(tmp.ind.join, by = "pie.id") %>%
    mutate(group = "moderate",
           age.gt60 = ifelse(age > 60, TRUE, FALSE),
           vte = ifelse(dvt == TRUE | pe == TRUE, TRUE, FALSE)) 

tmp.pts.normal <- tmp.demograph %>%
    filter(pie.id %in% group.normal) %>%
    inner_join(tmp.ind.join, by = "pie.id") %>%
    mutate(group = "normal",
           age.gt60 = ifelse(age > 60, TRUE, FALSE),
           vte = ifelse(dvt == TRUE | pe == TRUE, TRUE, FALSE))  

# vte / no vte; age <= 60 / age > 60
tmp.mod.young.novte <- filter(tmp.pts.mod, age.gt60 == FALSE, vte == FALSE)
tmp.mod.old.novte <- filter(tmp.pts.mod, age.gt60 == TRUE, vte == FALSE)
tmp.mod.young.vte <- filter(tmp.pts.mod, age.gt60 == FALSE, vte == TRUE)
tmp.mod.old.vte <- filter(tmp.pts.mod, age.gt60 == TRUE, vte == TRUE)

tmp.norm.young.novte <- filter(tmp.pts.normal, age.gt60 == FALSE, vte == FALSE)
tmp.norm.old.novte <- filter(tmp.pts.normal, age.gt60 == TRUE, vte == FALSE)
tmp.norm.young.vte <- filter(tmp.pts.normal, age.gt60 == FALSE, vte == TRUE)
tmp.norm.old.vte <- filter(tmp.pts.normal, age.gt60 == TRUE, vte == TRUE)


tmp.sample <- sample_n(tmp.pts.normal[tmp.pts.normal$age.gt60 == FALSE, ], size = nrow(tmp.pts.mod[tmp.pts.mod$age.gt60 == FALSE, ])) %>%
        bind_rows(sample_n(tmp.pts.normal[tmp.pts.normal$age.gt60 == TRUE, ], size = nrow(tmp.pts.mod[tmp.pts.mod$age.gt60 == TRUE, ])))

data.patients <- bind_rows(tmp.pts.mod, tmp.sample) %>%
    mutate(group = factor(group))
