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

# find all patients with bleeding
data.bleed <- data.diagnosis %>%
    select(pie.id, starts_with("bleed")) %>%
    full_join(tmp.hgb.drop, by = "pie.id") %>%
    mutate(hgb.drop = ifelse(is.na(hgb.drop), FALSE, hgb.drop),
           major.bleed = ifelse(bleed.major == TRUE | 
                                    (bleed.minor == TRUE & hgb.drop == TRUE), 
                                TRUE, FALSE),
           minor.bleed = ifelse(major.bleed == FALSE & bleed.minor == TRUE, 
                                TRUE, FALSE)) %>%
    select(pie.id, major.bleed, minor.bleed, hgb.drop)
