# tidy.R
# 
# organize data for analysis

source("library.R")

# raw data ----

raw.demograph <- read_edw_data(data.dir, pattern="^demographics")

tmp <- tidy_edw_data(raw.demograph, "demographics")
