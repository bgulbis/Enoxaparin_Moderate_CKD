# analyze.R
# 
# perform analysis of primary and secondary outcomes

source("library.R")

lab_change_test <- function(lab.data, change.by, decrease = TRUE, back = 2, units = "days") {
    # calculate the number of rows that are included within the window
    dots <- list(~count_rowsback(lab.datetime))
    lab.data <- mutate_(lab.data, .dots = setNames(dots, "rowsback"))
    
    # calculate the running max during the time window
    dots <- list(~zoo::rollapplyr(as.numeric(lab.result), rowsback, max, fill = NA,
                                  partial = TRUE))
    lab.data <- mutate_(lab.data, .dots = setNames(dots, "runmax"))

    # drop = as.numeric(result) - runmax,
    
    if (decrease == TRUE) {
        lab.data <- filter_(lab.data, .dots = list(~change <= change.by))
    } else {
        lab.data <- filter_(lab.data, .dots = list(~change >= change.by))
    }
        
        
    return(lab.data)
}

tmp.hgb <- raw.labs %>%
    inner_join(data.enox.courses, by="pie.id") %>%
    filter(lab == "Hgb",
           lab.datetime >= first.datetime,
           lab.datetime <= end.datetime + days(2)) %>%
    group_by(pie.id) %>%
    arrange(lab.datetime) 
    
tmp <- lab_change_test(tmp.hgb)
