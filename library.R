# library.R
# 
# packages to be used for data tidying and analysis

library(dplyr)
library(stringr)
library(BGTools)
library(lubridate)
library(tidyr)

# set directories for data
exclude.dir <- "Exclusion"
data.dir <- "Data"
lookup.dir <- "Lookup"

# compress source files
gzip_files(exclude.dir)
gzip_files(data.dir)
