# analyze.R
# 
# perform analysis of primary and secondary outcomes

source("library.R")

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
    distinct(pie.id) %>%
    mutate(hgb.drop = TRUE) %>%
    select(pie.id, hgb.drop)

# find patients getting transfused
dtcols <- c("blood.datetime", "first.datetime", "end.datetime")

tmp.prbc <- raw.blood %>%
    filter(blood.prod == "prbc") %>%
    filter_dates(data.enox.courses, dtcols = dtcols) %>%
    group_by(pie.id) %>%
    distinct(pie.id) %>%
    mutate(prbc = TRUE) %>%
    select(pie.id, prbc)

# find all patients with bleeding
data.bleed <- data.diagnosis %>%
    select(pie.id, starts_with("bleed")) %>%
    full_join(tmp.hgb.drop, by = "pie.id") %>%
    full_join(tmp.prbc, by = "pie.id") %>%
    mutate(hgb.drop = ifelse(is.na(hgb.drop), FALSE, hgb.drop),
           prbc = ifelse(is.na(prbc), FALSE, prbc),
           major.bleed = ifelse(bleed.major == TRUE | 
                                    (bleed.minor == TRUE & hgb.drop == TRUE) |
                                    (bleed.minor == TRUE & prbc == TRUE), 
                                TRUE, FALSE),
           minor.bleed = ifelse(major.bleed == FALSE & bleed.minor == TRUE, 
                                TRUE, FALSE),
           drop.prbc = ifelse(hgb.drop == TRUE & prbc == TRUE, TRUE, FALSE)) %>%
    select(-starts_with("bleed"))

# save bleeding data
saveRDS(data.bleed, "Preliminary Analysis/bleeding.Rds")
