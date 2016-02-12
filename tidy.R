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


# 
# raw <- list.files(data.dir, pattern = "surgeries", full.names = TRUE) %>%
#     lapply(read.csv, colClasses = "character") %>%
#     bind_rows %>%
#     distinct
