# analyze.R
# 
# perform analysis of primary and secondary outcomes

source("0-library.R")

# procedures ----
dtcols <- c("proc.date", "first.datetime", "end.datetime")

tmp.procedures <- raw.procedures %>%
    filter_dates(data.enox.courses, dtcols = dtcols) 

data.procedures <- tmp.procedures %>%
    group_by(pie.id) %>%
    summarize(proc = TRUE,
              proc.date = min(proc.date)) 

# bleeding ----

# get all hgb values during enoxaparin course + 2 days
dtcols <- c("lab.datetime", "first.datetime", "end.datetime")

tmp.hgb <- raw.labs %>%
    filter(lab == "hgb") %>% 
    filter_dates(data.enox.courses, dtcols = dtcols) %>%
    group_by(pie.id) %>%
    arrange(lab.datetime) 

# find all patients with a drop in hgb by >= 2 g/dL
tmp.hgb.drop <- lab_change(tmp.hgb, -2, max) %>%
    group_by(pie.id) %>%
    summarize(drop.datetime = min(lab.datetime),
              hgb.drop = TRUE)

# find patients getting transfused
dtcols <- c("blood.datetime", "first.datetime", "end.datetime")

tmp.prbc <- raw.blood %>%
    filter(blood.prod == "prbc") %>%
    filter_dates(data.enox.courses, dtcols = dtcols) %>%
    group_by(pie.id) %>%
    summarize(blood.datetime = min(blood.datetime),
              prbc = TRUE)

# find all patients with bleeding
data.bleed <- data.diagnosis %>%
    select(pie.id, starts_with("bleed")) %>%
    full_join(tmp.hgb.drop, by = "pie.id") %>%
    full_join(tmp.prbc, by = "pie.id") %>%
    full_join(data.manual.bleed, by = "pie.id") %>%
    full_join(data.procedures, by = "pie.id") %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .)), -pie.id, -contains("date")) %>%
    inner_join(data.enox.courses, by = "pie.id") %>%
    mutate(hgb.drop = ifelse(is.na(hgb.drop), FALSE, hgb.drop),
           prbc = ifelse(is.na(prbc), FALSE, prbc),
           major.bleed = ifelse(bleed.major == TRUE | ct.major == TRUE |
                                    (bleed.minor == TRUE & hgb.drop == TRUE) |
                                    (bleed.minor == TRUE & prbc == TRUE), 
                                TRUE, FALSE),
           minor.bleed = ifelse(major.bleed == FALSE & (bleed.minor == TRUE | ct.bleed == TRUE), 
                                TRUE, FALSE),
           major.bleed.proc = ifelse(major.bleed == TRUE & proc == TRUE, TRUE, FALSE),
           minor.bleed.proc = ifelse(minor.bleed == TRUE & proc == TRUE, TRUE, FALSE),
           tmp.drop.date = floor_date(drop.datetime, "day"),
           tmp.drop.proc = as.numeric(difftime(tmp.drop.date, proc.date, units = "days")),
           drop.after.procedure = ifelse(tmp.drop.proc >= 0 & tmp.drop.proc <= 1, TRUE, FALSE),
           tmp.prbc.date = floor_date(blood.datetime, "day"),
           tmp.prbc.proc = as.numeric(difftime(tmp.prbc.date, proc.date, units = "days")),
           prbc.after.procedure = ifelse(tmp.prbc.proc >= 0 & tmp.prbc.proc <= 1, TRUE, FALSE),
           tmp.drop.prbc = as.numeric(difftime(blood.datetime, drop.datetime, units = "days")),
           prbc.after.drop = ifelse(tmp.drop.prbc >= 0 & tmp.drop.prbc <= 1, TRUE, FALSE),
           non.proc.prbc = ifelse(prbc == TRUE, 
                                  ifelse(is.na(prbc.after.procedure) | 
                                             prbc.after.procedure == FALSE, 
                                         TRUE, FALSE), 
                                  NA)) %>%
    select(-starts_with("bleed"), -contains("tmp"), -contains("date"), -(dose.count:freq))

# analysis ----
# make data frames to use for analysis
analyze.demographics <- select(data.demograph, -person.id)
analyze.bleed <- left_join(data.groups, data.bleed, by = "pie.id") 
analyze.diagnosis <- data.diagnosis

analyze.home.meds <- left_join(data.groups, data.home.meds, by = "pie.id") %>%
    mutate_each(funs(ifelse(is.na(.), FALSE, .)), -pie.id, -group)
names(analyze.home.meds) <- make.names(names(analyze.home.meds))

analyze.thrombosis <- left_join(data.groups, data.manual.thrmb, by = "pie.id") %>%
    mutate(thrombus = ifelse(is.na(thrombus), FALSE, thrombus)) %>%
    select(-rad.datetime)

save_rds(analysis.dir, "analyze")
