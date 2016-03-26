# manual.R

# get results of diagnostic scans from manual review

source("0-library.R")
library(readxl)

raw.manual <- read_excel(paste(manual.dir, "diagnostic_scans.xlsx", sep = "/"))