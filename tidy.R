# tidy.R
# 
# organize data for analysis

source("library.R")

# raw data ----

raw.blood <- read_edw_data(data.dir, file.name = "blood")
raw.demograph <- read_edw_data(data.dir, file.name = "demographics")
raw.diagnosis <- read_edw_data(data.dir, file.name = "diagnosis")
raw.home.meds <- read_edw_data(data.dir, file.name = "home_meds")
raw.labs <- read_edw_data(data.dir, file.name = "labs")
raw.measures <- read_edw_data(data.dir, file.name = "measures")
raw.meds.cont <- read_edw_data(data.dir, file.name = "meds_continuous")
raw.meds.sched <- read_edw_data(data.dir, file.name = "meds_sched")
raw.procedures <- read_edw_data(data.dir, file.name = "procedures")
raw.radiology <- read_edw_data(data.dir, file.name = "radiology")
raw.surgeries <- read_edw_data(data.dir, file.name = "surgeries")

if (!exists("data.patients")) {
    data.patients <- readRDS("included_patients.Rds")
}

incl.pts <- data.patients$pie.id

# demographics ----
data.demograph <- raw.demograph %>%
    inner_join(select(data.patients, pie.id, afib:vte), by = "pie.id")

rm(data.patients)

# diagnosis ----
# get desired diagnosis codes
ref.pmh.codes <- read.csv("Lookup/pmh_lookup.csv", colClasses = "character")

tmp.icd9.codes <- icd9_lookup(ref.pmh.codes)

tmp.diagnosis <- raw.diagnosis %>%
    filter(diag.type != "Admitting",
           diag.type != "Working") %>%
    inner_join(tmp.icd9.codes, by = c("diag.code" = "icd9.code")) %>%
    mutate(disease.state = factor(disease.state),
           value = TRUE) %>%
    select(pie.id, disease.state, value) %>%
    group_by(pie.id, disease.state) %>%
    distinct %>%
    spread(disease.state, value, fill = FALSE, drop = FALSE)


