
# fnction to calculate requested stats
calcStats <- function(db= database, rf= riskFree) {
    
    db$returns[1,] <- 0
    
    # compute dbistics
    db$data <- rbind(
        
        table.AnnualizedReturns(db$returns, Rf= rf),
        table.Distributions(db$returns),
        table.DrawdownsRatio(db$returns),
        table.DownsideRiskRatio(db$returns),
        table.AnnualizedReturns(db$returns, Rf= rf),
        data.frame(SharpeRatio(db$returns, Rf= rf)),
        data.frame(DownsideDeviation(db$returns, MAR= 0)),
        skewness(db$returns),
        kurtosis(db$returns)
    )
    
    db$data <- unique(db$data)
    db$data <- as.data.frame(t(db$data))
    
    
    # find best in class
    bestReturn   <- db$data[db$data$`Annualized Return` ==
                                  max(db$data$`Annualized Return`), ]
    
    bestSharpe   <- db$data[db$data$`Annualized Sharpe` ==
                                  max(db$data$`Annualized Sharpe`),]
    
    bestSharpe   <- bestSharpe[bestSharpe$`Annualized Return` ==
                                   max(bestSharpe$`Annualized Return`),]
    
    bestSortino  <- db$data[db$data$`Sortino ratio` ==
                                  max(db$data$`Sortino ratio`),]
    
    bestSortino  <- bestSortino[bestSortino$`Annualized Return` ==
                                    max(bestSortino$`Annualized Return`),]
    
    lowSharpeVol <- db$data[db$data$`StdDev Sharpe` ==
                                  max(db$data$`StdDev Sharpe`),]
    
    stats <- rbind(rbind(bestReturn,  bestSharpe),
                   rbind(bestSortino, lowSharpeVol))
    
    #stats$name <- rownames(stats)
    
    #stats[, c(ncol(stats), seq(1,ncol(stats)-1)), FALSE]

    
    db$stats <- stats
    
    return(db)
    
}
