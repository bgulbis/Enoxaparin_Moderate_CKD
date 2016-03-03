# analyze.R
# 
# perform analysis of primary and secondary outcomes

source("library.R")

# get all hgb values during enoxaparin course + 2 days
tmp.hgb <- raw.labs %>%
    inner_join(data.enox.courses, by="pie.id") %>%
    filter(lab == "Hgb",
           lab.datetime >= first.datetime,
           lab.datetime <= end.datetime + days(2)) %>%
    group_by(pie.id) %>%
    arrange(lab.datetime) 
    
# find all drops in hgb by >= 2 g/dL
tmp <- lab_change(tmp.hgb, -2, max)
