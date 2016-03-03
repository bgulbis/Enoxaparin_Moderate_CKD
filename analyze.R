# analyze.R
# 
# perform analysis of primary and secondary outcomes

source("library.R")

# bleeding ----

# get all hgb values during enoxaparin course + 2 days
tmp.hgb <- raw.labs %>%
    inner_join(data.enox.courses, by="pie.id") %>%
    filter(lab == "hgb",
           lab.datetime >= first.datetime,
           lab.datetime <= end.datetime) %>%
    group_by(pie.id) %>%
    arrange(lab.datetime) 
    
# find all patients with a drop in hgb by >= 2 g/dL
tmp.hgb.drop <- lab_change(tmp.hgb, -2, max) %>%
    distinct(pie.id) %>%
    mutate(hgb.drop = TRUE) %>%
    select(pie.id, hgb.drop)

# find patients getting transfused
tmp.prbc <- raw.blood %>%
    inner_join(data.enox.courses, by="pie.id") %>%
    filter(blood.prod == "prbc",
           blood.datetime >= first.datetime,
           blood.datetime <= end.datetime) %>%
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
    select(pie.id, major.bleed, minor.bleed, hgb.drop, prbc, drop.prbc)

# save bleeding data
saveRDS(data.bleed, "Preliminary Analysis/bleeding.Rds")
