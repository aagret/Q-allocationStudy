
library(data.table)
library(quantmod)
        
library(xts)
library(TTR)

library(PerformanceAnalytics)
library(ggplot2)



workDir <- "//ARTHASERVER/Data/Alexandre/R-Projects/Allocator"
setwd(workDir)

# function to read hostorical datas from .csv files
getData <- function(db= filename) {
    
    db <- fread(db)
    db[, Date:= as.Date(Date, format= "%d.%m.%Y")]
    db[is.na(db)] <- 0
    db <- as.xts(db)
    
}

# function to generate 100 mix of two assets
generatePortfolios <- function(db1= asset1, db2= asset2) {

    port <- db2
    
    for (i in seq(1, 99, 1)) {
        ret <- (db1 * (100 -i) / 100) + (db2 * i / 100)
        colnames(ret) <- paste0(colnames(db2), i)
        port <- cbind(port, ret)
    }
    
    port <- cbind(port, db1)
    
}

# functions to generate portfolio statistics
getStats <- function(db= database, per= period) {
    
    db <- lapply(db, function(x) lapply(x, function(y) y[per,]))

    if (is.null(db$indexes$returns$EUR)) {
        
        per <- index(db)
        rates <- as.xts(getData("rates.csv"))
        db <- cbind(db$indexes, rates[per])
    }
    
    riskFree <- mean(db$indexes$returns$EUR) /36500
    
    db$indexes$returns$EUR <- db$indexes$returns$USD <- NULL
    
    stats <- list()
    stats <- lapply(db, calcStats, riskFree)

}

calcStats <- function(stat= database, rf= riskFree) {
    
    stat$returns[1,] <- 0
    
    stat$stats <- rbind(
        
        table.AnnualizedReturns(stat$returns, Rf= rf),
        table.Distributions(stat$returns),
        table.DrawdownsRatio(stat$returns),
        table.DownsideRiskRatio(stat$returns),
        table.AnnualizedReturns(stat$returns, Rf= rf),
        data.frame(SharpeRatio(stat$returns, Rf= rf)),
        data.frame(DownsideDeviation(stat$returns, MAR= 0)),
        skewness(stat$returns),
        kurtosis(stat$returns)
    )
    
    stat$stats <- unique(stat$stats)
    stat$stats <- as.data.frame(t(stat$stats))
    
    stat$bestReturn   <- stat$stats[stat$stats$`Annualized Return`== 
                                   max(stat$stats$`Annualized Return`), ]
    
    bestSharpe   <- stat$stats[stat$stats$`Annualized Sharpe`== 
                             max(stat$stats$`Annualized Sharpe`),]
    
    stat$bestSharpe   <- bestSharpe[bestSharpe$`Annualized Return`== 
                                         max(bestSharpe$`Annualized Return`),]
    
    bestSortino  <- stat$stats[stat$stats$`Sortino ratio`== 
                             max(stat$stats$`Sortino ratio`),]
    
    stat$bestSortino  <- bestSortino[bestSortino$`Annualized Return`== 
                                          
                                          max(bestSortino$`Annualized Return`),]
    
    
    stat$lowSharpeVol <- stat$stats[stat$stats$`StdDev Sharpe`== 
                                   max(stat$stats$`StdDev Sharpe`),]
    
    return(stat)
    
}

# function to plot historical returns
plotHisto <- function(db= database) {
    
    per <- paste0(start(db),"/",end(db))
    
    db[1,] <- 0
    db <- exp(cumsum(db))
    db <- melt(as.data.table(db), "index")
    
    ggplot(data=db) + ggtitle(per) +
        geom_line(aes(x=index, y=value, group=variable, color=variable))
    
}

# function to plot efficient frontiers
plotFrontier <- function(db= database, typ= "Sortino") {
    
    per <- paste0(start(db$returns),"/",end(db$returns))
    
    if (typ == "Sortino") {
        dat  <- "`Annualised downside risk`"
        type <- "Sortino ratio"
    }
    
    if (typ == "Sharpe") {
        
        dat  <- "`Annualized Std Dev`"
        type <- "Annualized Sharpe"
    }
    
    first <- first(rownames(db$stats))
    last  <- last (rownames(db$stats))
    
    ggplot() + ggtitle(per) + #xlim(0, 0.15) + ylim(-0.06, 0.12) +
        
        geom_point(data= db$stats,
                   aes_string(x=dat, y="`Annualized Return`"),
                   color="black") +
        
        geom_point(data= db$stats[first,],
                   aes_string(x=dat, y="`Annualized Return`"), 
                   size=5, colour="black") +
        
        geom_label(data= db$stats[first,], 
                   aes_string(x=dat, y="`Annualized Return`"),
                   label=paste0(first," - ", typ, ": ", 
                                db$stats[first, grepl(type, colnames(db$stats))]),
                   hjust=-.2) +
        
        geom_point(data= db$stats[last,],
                   aes_string(x=dat, y="`Annualized Return`"), 
                   size=5, colour="black") +
        
        geom_label(data= db$stats[last,], 
                   aes_string(x=dat, y="`Annualized Return`"),
                   label=paste0(last, " - ", typ, ": ",
                                db$stats[last, grepl(type, colnames(db$stats))]),
                   hjust=-.2) +
        
        geom_point(data= db[[paste0("best", typ)]],
                   aes_string(x=dat, y="`Annualized Return`"), 
                   size=5, colour="red") +
        
        geom_label(data= db[[paste0("best", typ)]], 
                   aes_string(x=dat, y="`Annualized Return`"),
                   label=paste0(rownames(db[[paste0("best", typ)]]), 
                                "- ", typ, ": ",
                                db[[paste0("best", typ)]][grepl(type, colnames(db$stats[last,]))]),
                   hjust=-0.2)  #+
        # 
        # geom_point(data= stats0$stats["DEQex",],
        #        aes_string(x=dat, y="`Annualized Return`"), 
        #        size=5, colour="blue") +
        # 
        # geom_label(data= stats0$stats["DEQex",], 
        #            aes_string(x=dat, y="`Annualized Return`"),
        #            label=paste("DEQex", typ, ": ", 
        #                        stats0$stats["DEQex",][grepl(type, colnames(db$stats[last,]))]), 
        #            hjust=1.2) +
        # 
        # geom_point(data= stats0$stats["Bench",],
        #            aes_string(x=dat, y="`Annualized Return`"), 
        #            size=5, colour="green") +
        # 
        # geom_label(data= stats0$stats["Bench",], 
        #            aes_string(x=dat, y="`Annualized Return`"),
        #            label=paste("Bench", typ, ": ", 
        #                        stats0$stats["Bench",grepl(type, colnames(db$stats[last,]))]),
        #            hjust=1.2) +
        # 
        # geom_point(data= stats0$stats["BE500T",],
        #            aes_string(x=dat, y="`Annualized Return`"), 
        #            size=5, colour="purple") +
        # 
        # geom_label(data= stats0$stats["BE500T",], 
        #            aes_string(x=dat, y="`Annualized Return`"),
        #            label=paste("BE500T", typ, ": ", 
        #                        stats0$stats["BE500T",grepl(type, colnames(db$stats[last,]))]),
        #            hjust=1.2) +
        # 
        # geom_point(data= stats0$stats["QasCash",],
        #            aes_string(x=dat, y="`Annualized Return`"),
        #            size=5, colour="yellow") +
        # 
        # geom_label(data= stats0$stats["QasCash",],
        #            aes_string(x=dat, y="`Annualized Return`"),
        #            label=paste0("QasCash", " - ", typ, ": ",
        #                         stats0$stats["QasCash", grepl(type, colnames(stats0$stats))]),
        #            hjust=1.2)
        # 
    
}



###########LOAD and set datas 

data <- list()
# merge .csv datas
data$indexes$returns <- cbind(getData("returns.csv"), getData("navQ.csv"), getData("rates.csv"))

# compute hedging cost for usd assets             
data$indexes$returns$Hdg_Cst <- lag(data$indexes$returns$EUR - data$indexes$returns$USD - 0.375) *
    c(0, diff(index(returns))) / 36500

data$indexes$returns$Hdg_Cst[1] <- 0

# compute hedged indexes in EUR
data$indexes$returns$Q_hdg <- log(1 + data$indexes$returns$Q) + log(1 + data$indexes$returns$Hdg_Cst)
data$indexes$returns$SPXT_hdg <- log(1 + data$indexes$returns$SPXT) + log(1 + data$indexes$returns$Hdg_Cst)

# compute DEQ ex cash asset selection performance
data$indexes$returns$DEQex <- data$indexes$returns$DEQ / data$indexes$returns$DEQ_Inv
data$indexes$returns$DEQex[1] <- 0
data$indexes$returns$DEQex <- na.locf(data$indexes$returns$DEQex)

# compute Q + DEQ ex cash history
data$indexes$returns$QasCash <- (data$indexes$returns$DEQ * data$indexes$returns$DEQ_Inv) + 
    (data$indexes$returns$Q_hdg * (1 - data$indexes$returns$DEQ_Inv))

# remove non index datas
data$indexes$returns$DEQ_Inv <- data$indexes$returns$Hdg_Cst <- NULL



data$port1$returns   <- generatePortfolios(data$indexes$returns$DEQ,  data$indexes$returns$Q_hdg)
data$port2$returns   <- generatePortfolios(data$indexes$returns$SPXT, data$indexes$returns$Q)




########
##############************************

# define study period
period  <-"2016-12-30/2018-01-31"
indices <- c("DEQ", "DEQex", "Q_hdg", "QasCash", "BE500T", "Bench")

study <- getStats(data, period)


####
# table result
# tableau Recap# 

result <- function(db= database, idx= indices) {
    

    res <- db$indexes$stats[idx, c(1, 3, 25, 21)]
    
    t <- lapply(db[-1], function(x) x[-1:-2])
                             
    lapply(t, function(x) lapply(t[x], function(y) y[, c(1, 3, 25, 21)]))
                             
    
    
    for (i in 2:length(db)) {
        
        res <- rbind(res,
                     db[-1][[-1]][, c(1, 3, 25, 21)])
        
        
    }
}

a <- stats$indexes$stats[indices, c(1, 3,25,21)]
a <- rbind(a, 
           stats$port1$bestReturn[, c(1, 3,25,21)])


a <- rbind(a, 
           stats$port1$bestSharpe[,c(1, 3,25,21)])

a <- rbind(a, 
           stats$port1$bestSortino[,c(1, 3,25,21)])

a$name <- c(indices, 
            rownames(stats$port1$bestReturn), 
            rownames(stats$port1$bestSharpe),
            rownames(stats$port1$bestSortino))

#rownames(a) <- c("Benchmark", "Bloomberg 500", "DEQ", "DEQ ex Cash", 
#                 "Q hedge €", "Q as DEQ_Cash", "Best Retrun", 
#                 "Best Sharpe Ratio", "Best Sortino Ratio")


colnames(a) <- c("Return an.", "Sharpe", "Sharpe Vol.", "Sortino", "Name")    




#########
plotHisto(stats$indexes$returns[,indices])
plotFrontier(stats$port1)
plotFrontier(stats$port2)

# compute portfolios statistics
# stats0 <- getStats(returns, period)
plotHisto(stats$indexes$returns)



### plots to PDF
plotFrontier(stats0$port1, "Sharpe")

pdf("test.pdf")
print(a)
print(plotHisto(stats$indexes$returns[,indices]))
print(plotFrontier(stats, "Sortino"))
print(plotFrontier(port1Stats, "Sharpe"))
print(plotFrontier(port2Stats, "Sortino"))
print(plotFrontier(port2Stats, "Sharpe"))
dev.off()

port1Stats$bestSharpe

