library(dplyr)
library(lubridate)
library(stringr)

data <- read.csv("Proposal Data/Enoxaparin.csv", colClasses="character") %>%
    mutate(pie.id = factor(PowerInsight.Encounter.Id),
           med.date = mdy_hms(Clinical.Event.End.Date.Time)) %>%
    select(pie.id, med.date) %>%
    group_by(pie.id) %>%
    summarize(count = n()) %>%
    filter(count >= 5)

pie <- str_c(data$pie.id[1:700], collapse=";")
pie2 <- str_c(data$pie.id[701:length(data$pie.id)], collapse=";")

egfr <- read.csv("Proposal Data/eGFR1.csv", colClasses="character") %>%
    bind_rows(read.csv("Proposal Data/eGFR2.csv", colClasses="character")) %>%
    mutate(pie.id = factor(PowerInsight.Encounter.Id),
           result = as.numeric(Clinical.Event.Numeric.Result),
           lab.date = mdy_hms(Clinical.Event.End.Date.Time)) %>%
    select(pie.id, result, lab.date) %>%
    filter(result > 0,
           result < 60) %>%
    group_by(pie.id) %>%
    summarize(count = n()) %>%
    filter(count >= 2)
    
pie.diag <- str_c(egfr$pie.id, collapse=";")

diagnosis <- read.csv("Proposal Data/diagnosis.csv", colClasses="character") %>%
    transmute(pie.id = factor(PowerInsight.Encounter.Id),
              icd9 = ICD9.Diagnosis.Code) 

major <- filter(diagnosis, icd9 %in% c("430", "431", "432.1", "568.81", "719.16", "360.43")) %>%
    group_by(pie.id) %>%
    distinct

minor <- filter(diagnosis, icd9 %in% c("432", "459", "423", "599.7", "719.1", "719.11", "719.12", 
                                       "719.13", "719.14", "719.15", "719.17", "719.18", "719.19", 
                                       "784.7", "784.8", "786.3", "456","456.2","530.7","530.82",
                                       "531","531.01","531.2","531.21","531.4","531.41","531.6",
                                       "531.61","532","532.01","532.2","532.21","532.4","532.41",
                                       "532.6","532.61","533","533.01","533.2","533.21","533.4",
                                       "533.41","533.6","533.61","534","534.01","534.2","534.21",
                                       "534.4","534.41","534.6","534.61","569.3","578","578.1","578.9")) %>%
    group_by(pie.id) %>%
    distinct

test <- inner_join(major, minor, by="pie.id")



