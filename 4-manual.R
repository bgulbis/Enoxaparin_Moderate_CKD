# manual.R

# get results of diagnostic scans from manual review

source("0-library.R")
library(readxl)

raw.manual <- read_excel(paste(manual.dir, "diagnostic_scans.xlsx", sep = "/")) %>%
    mutate(fin = as.character(fin),
           thrombus = ifelse(thrombus == 1, TRUE, FALSE),
           bleed = ifelse(bleed == 1, TRUE, FALSE))

data.manual <- raw.manual %>%
    inner_join(ref.fins, by = "fin") %>%
    inner_join(data.enox.courses, by = "pie.id") %>%
    filter(rad.datetime >= first.datetime,
           rad.datetime <= end.datetime + days(2),
           !is.na(thrombus))

data.manual.thrmb <- data.manual %>%
    filter(thrombus == TRUE) %>%
    mutate(enox.days = as.numeric(difftime(rad.datetime, first.datetime, units = "days")),
           stroke = str_detect(rad.type, regex("head|brain", ignore_case = TRUE)),
           pe = str_detect(rad.type, regex("chest|pulm", ignore_case = TRUE))) %>%
    group_by(pie.id) %>%
    summarize(thrombus = first(thrombus),
              enox.days = min(enox.days),
              stroke.new = ifelse(sum(stroke) > 0, TRUE, FALSE),
              pe.new = ifelse(sum(pe) > 0, TRUE, FALSE)) %>%
    filter(enox.days >= 2)

data.manual.bleed <- data.manual %>%
    filter(bleed == TRUE) %>%
    mutate(enox.days = as.numeric(difftime(rad.datetime, first.datetime, units = "days")),
           ct.major = str_detect(rad.type, regex("head|brain", ignore_case = TRUE))) %>%
    group_by(pie.id) %>%
    summarize(ct.bleed = first(bleed),
              enox.days = min(enox.days),
              ct.major = ifelse(sum(ct.major) > 0, TRUE, FALSE)) %>%
    filter(enox.days >= 2)
