# exclude.R
# 
# determine which patients should be excluded

source("library.R")

# compress data files
gzip_files("Exclusion")

# read in all data files needed to evaluate for exclusion
raw.excl.diagnosis <- list.files("Exclusion", pattern="^diagnosis", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              diag.code = ICD9.Diagnosis.Code,
              diag.type = factor(Diagnosis.Type, exclude = ""),
              diag.seq = factor(Diagnosis.Code.Sequence)) 

raw.excl.weight <- list.files("Exclusion", pattern="^ht_wt", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event),
              result = as.numeric(Clinical.Event.Result),
              unit = factor(Clinical.Event.Result.Units)) 

raw.excl.labs <- list.files("Exclusion", pattern="^labs", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id,
              event.datetime = mdy_hms(Clinical.Event.End.Date.Time),
              event = factor(Clinical.Event),
              result = as.numeric(Clinical.Event.Result))
              
raw.excl.meds <- list.files("Exclusion", pattern="^medications", full.names=TRUE) %>%
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

# evaluate for inclusion criteria: at least 3 doses of enoxaparin over 72 hours

# find those with >= 3 doses and >= 72 hours of treatment
tmp.count.doses <- raw.excl.meds %>%
    filter(event == "enoxaparin",
           dose > 40) %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    summarize(count = n(),
              first = first(event.datetime),
              last = last(event.datetime)) %>%
    filter(count >= 3,
           difftime(last, first, units = "hours") >= 72)

enox_course <- function(diff) {
#     sapply(diff, function(x)
#         if (is.na(x)) {
#             course <- 1
#             course
#         } else if (x <= 28) {
#             course
#         } else {
#             course <- course + 1
#             course
#         }
#     )
}

# find patients who had gaps in enoxaparin therapy (> 30 hours between doses)
tmp.courses <- raw.excl.meds %>%
    filter(pie.id %in% tmp.count.doses$pie.id,
           event == "enoxaparin",
           dose > 40) %>%
    group_by(pie.id) %>%
    arrange(event.datetime) %>%
    mutate(hours.prev.dose = difftime(event.datetime, lag(event.datetime), units = "hours"),
           course.start = ifelse(is.na(hours.prev.dose) | hours.prev.dose > 36, TRUE, FALSE)) 
    
tmp.courses.count <- summarize(tmp.courses, start = sum(course.start)) %>%
    ungroup %>%
    group_by(start) %>%
    summarize(count = n())

# exclusion: pregnancy, weight < 45 or > 150, >1 CrCl <30 during enoxaparin,
# coadmin of other anticoags except warfarin, enoxaparin dose change by >10%

# find patients with moderate renal impairment: >1 CrCl 30-60 within 48h of
# enoxaparin start
