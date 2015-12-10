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