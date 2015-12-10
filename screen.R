# screen.R
# 
# create list of patients to screen for inclusion

source(library.R)

# read in all data files
pts.screen <- list.files("Screen", pattern="^identified", full.names=TRUE) %>%
    lapply(read.csv, colClasses="character") %>%
    bind_rows %>%
    transmute(pie.id = PowerInsight.Encounter.Id)

# split the patients up into groups of 100
edw.pie <- split(pts.screen$pie.id, ceiling(seq_along(pts.screen$pie.id)/50))
# combine the id's in each group into a string, separated by semi-colon
edw.pie <- lapply(edw.pie, str_c, collapse=";")

print(edw.pie)