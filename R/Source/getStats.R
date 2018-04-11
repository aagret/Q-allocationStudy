
# functions to generate portfolio statistics
getStats <- function(db= database, per= period) {
    
    db <- lapply(db, function(x) lapply(x, function(y) y[per,]))
    
        # if (is.null(db$indexes$returns$EUR)) {
        # 
        # per <- index(db)
        # rates <- as.xts(getData("rates.csv"))
        # db <- cbind(db$indexes, rates[per])
        # 
        # }
    
    ndays <- last(index(db$indexes$returns)) - first(index(db$indexes$returns))
    
    riskFree <- mean(db$indexes$returns$EU1m) / 26000
    
    db$indexes$returns$US1m <- db$indexes$returns$EU1m <- NULL
    
    db$indexes$cor     <- round(cor(db$indexes$returns), 4)

    rollCor <- runCor(db$indexes$returns$DEQexa, db$indexes$returns$Q_hdg, 30)
    rollCor[is.na(rollCor)] <- 0
    
    db$indexes$rollCor <- rollCor

    stats <- list()
    stats <- lapply(db, calcStats, riskFree)

    return(stats)

}
