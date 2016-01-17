# screen.R
# 
# create list of patients to screen for inclusion

source("library.R")

# read in all data files
pts.screen <- list.files("Screen", pattern="^Enoxaparin", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id) %>%
    distinct

pts.screen <- pts.screen$pie.id

# split the patients up into groups of 1000
edw.pie <- split(pts.screen, ceiling(seq_along(pts.screen)/500))
# combine the id's in each group into a string, separated by semi-colon
edw.pie <- lapply(edw.pie, str_c, collapse=";")

print(edw.pie)
