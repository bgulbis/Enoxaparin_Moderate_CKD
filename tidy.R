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
raw.enox.freq <- read_edw_data(data.dir, file.name = "meds_sched_enox", type = "meds_sched_freq")

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
# use standard tidying function
data.diagnosis <- tidy_data("diagnosis", ref.data = ref.pmh.codes, pt.data = raw.diagnosis, patients = data.demograph) 

# home meds ----
# get desired medication classes
ref.home.meds <- med_lookup(c("anticoagulants", "antiplatelet agents"))
# use standard tidying function
data.home.meds <- tidy_data("outpt_meds", ref.data = ref.home.meds, pt.data = raw.home.meds, patients = data.demograph)

# hospital meds ----
if (!exists("tmp.enox.courses")) {
    tmp.enox.courses <- readRDS("enoxaparin_courses.Rds")
}

tmp.enox.courses <- filter(tmp.enox.courses, pie.id %in% incl.pts) %>%
    select(-last.datetime)
