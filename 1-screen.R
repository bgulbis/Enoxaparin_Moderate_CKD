# screen.R
# 
# create list of patients to screen for inclusion

source("library.R")

# read in all data files
pts.screen <- read_data("Screen", "Enoxaparin", base = TRUE) %>%
    transmute(pie.id = PowerInsight.Encounter.Id)

edw.pie <- concat_encounters(pts.screen$pie.id, 500)
print(edw.pie)
