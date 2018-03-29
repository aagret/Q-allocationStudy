

####
# table result
# tableau Recap# 

indices <- c("DEQ", "DEQex", "Q_hdg", "QasCash", "BE500T", "Bench")

result <- function(db= database, idx= indices) {
    
    
    res <- db$indexes$stats[idx, c(1, 3, 25, 21)]
    
    t <- lapply(db[-1], function(x) x[-1:-2])
    
    lapply(t, function(x) lapply(t[x], function(y) y[, c(1, 3, 25, 21)]))
    
    
    
    for (i in 2:length(db)) {
        
        res <- rbind(res,
                     db[-1][[-1]][, c(1, 3, 25, 21)])
        
        
    }
}

a <- study$indexes$stats[indices, c(1, 3,25,21)]
a <- rbind(a, 
           study$port1$bestReturn[, c(1, 3,25,21)])


a <- rbind(a, 
           study$port1$bestSharpe[,c(1, 3,25,21)])

a <- rbind(a, 
           study$port1$bestSortino[,c(1, 3,25,21)])

a$name <- c(indices, 
            rownames(study$port1$bestReturn), 
            rownames(study$port1$bestSharpe),
            rownames(study$port1$bestSortino))

#rownames(a) <- c("Benchmark", "Bloomberg 500", "DEQ", "DEQ ex Cash", 
#                 "Q hedge €", "Q as DEQ_Cash", "Best Retrun", 
#                 "Best Sharpe Ratio", "Best Sortino Ratio")


# colnames(a) <- c("Return an.", "Sharpe", "Sharpe Vol.", "Sortino", "Name")    




#########
# plot test# 
plotHisto(studies$indexes$returns)
plotFrontier(study$port1, "Sharpe")

### plots to PDF
pdf("test.pdf")
print(a)
print(plotHisto(stats$indexes$returns[,indices]))
print(plotFrontier(stats, "Sortino"))
print(plotFrontier(port1Stats, "Sharpe"))
print(plotFrontier(port2Stats, "Sortino"))
print(plotFrontier(port2Stats, "Sharpe"))
dev.off()


