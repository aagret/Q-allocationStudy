
# function to plot efficient frontiers
plotFrontier <- function(db= database, typ= "Sortino") {
    
   # per <- paste0(start(db$indexes$returns),"/",end(db$indexes$returns))
    
    if (typ == "Sortino") {
        dat  <- "`Annualised downside risk`"
        type <- "Sortino ratio"
    }
    
    if (typ == "Sharpe") {
        
        dat  <- "`Annualized Std Dev`"
        type <- "Annualized Sharpe"
    }
    
    first <- first(rownames(db))
    last  <- last (rownames(db))
    
    minX <- min(db[, gsub("`", "", dat)]) 
    maxX <- max(db[, gsub("`", "", dat)]) * 1.05
    minY <-  min(db[, "Annualized Return"]) * 0.95
    maxY <-  max(db[, "Annualized Return"])
    maxY <- ceiling(maxY * 100 + 1) / 100

    best <- db[db[grepl(type,colnames(db))]==max(db[grepl(type,colnames(db))]),]
    best <- best[best["Annualized Return"]==max(best["Annualized Return"]),]
    
    rf   <- colnames(db)[grepl("Annualized Sharpe", colnames(db))]
    rf   <- gsub("Annualized Sharpe ", "",rf)
    rf   <- gsub("[()=%]", "", rf)
    rf   <- as.numeric(gsub("Rf", "", rf))  / 100
    
    gg<- ggplot()  +  
        
        theme(text=element_text(size=6),
              axis.title=element_text(size=10, face="bold")) + 
        
        geom_point(data= db["DEQ",],
                   aes_string(x=dat, y="`Annualized Return`"),
                   size=5, 
                   colour="green4") +
        
        geom_text(data= db["DEQ",],
                  aes_string(x=dat, y="`Annualized Return`"),
                  label="DEQ",
                  hjust=-0.5, vjust= -1.2, size= 3,
                  colour="green4")  +
        
        geom_point(data= db["Q_hdg",],
                   aes_string(x=dat, y="`Annualized Return`"),
                   size= 5, 
                   colour="cyan") +
        
        geom_text(data= db["Q_hdg",],
                  aes_string(x=dat, y="`Annualized Return`"),
                  label="Q_hdg",
                  hjust=-0.5, vjust= -1.2, size= 3,
                  colour="cyan")  +
        
        geom_point(data= db["Q_hdg5",],
                   aes_string(x=dat, y="`Annualized Return`"),
                   size= 5, 
                   colour="turquoise4") +
        
        geom_text(data= db["Q_hdg5",],
                  aes_string(x=dat, y="`Annualized Return`"),
                  label="Q_hdg-5%",
                  hjust=1, vjust= -1.2, size= 3,
                  colour="turquoise4")  +
        
        geom_point(data= db["Q_hdg10",],
                   aes_string(x=dat, y="`Annualized Return`"),
                   size= 5, 
                   colour="turquoise") + 
        
        geom_text(data= db["Q_hdg10",],
                  aes_string(x=dat, y="`Annualized Return`"),
                  label="Q_hdg-10%",
                  hjust=1, vjust= -1.2, size= 3,
                  colour="turquoise")  +
        
        geom_point(data= db,
                   aes_string(x=dat, y="`Annualized Return`"),
                   color="black") + xlim(minX, maxX) + ylim(minY, maxY) +
        
        geom_point(data= db[first,],
                   aes_string(x=dat, y="`Annualized Return`"), 
                   size=5, colour="black") +
        
        geom_text(data= db[first,], 
                  aes_string(x=dat, y="`Annualized Return`"),
                  label=paste0(first," - ", typ, ": ", 
                               db[first, grepl(type, colnames(db))]),
                  hjust=1, vjust= -1.2, size= 3,
                  colour="black") +
        
        geom_point(data= db[last,],
                   aes_string(x=dat, y="`Annualized Return`"), 
                   size=5, 
                   colour="black") +
        
        geom_text(data= db[last,], 
                  aes_string(x=dat, y="`Annualized Return`"),
                  label=paste0(last, " - ", typ, ": ",
                               db[last, grepl(type, colnames(db))]),
                  hjust=1, vjust= -1.2, size= 3,
                  colour="black") +
        
        geom_point(data= best,
                   aes_string(x=dat, y="`Annualized Return`"),
                   size=5, 
                   colour="red") +
        
        geom_text(data= best,
                  aes_string(x=dat, y="`Annualized Return`"),
                  label=paste0(rownames(best), 
                               "-",
                               paste0("best", typ),
                               ": ",
                               best[grepl(type,colnames(db))]),
                  hjust=1, vjust= -1.2, size= 3, 
                  colour="red") +
        
        geom_abline(intercept=rf, slope=
                        (best$`Annualized Return` - rf )/ best$`Annualised downside risk`,
                    size=1, color="red", linetype= "dotted")
    
    
    
    # 
    # geom_point(data= stats0$stats["DEQex",],
    #        aes_string(x=dat, y="`Annualized Return`"), 
    #        size=5, colour="blue") +
    # 
    # geom_label(data= stats0$stats["DEQex",], 
    #            aes_string(x=dat, y="`Annualized Return`"),
    #            label=paste("DEQex", typ, ": ", 
    #                        stats0$stats["DEQex",][grepl(type, colnames(db[last,]))]), 
    #            hjust=1.2) +
    # 
    # geom_point(data= stats0$stats["Bench",],
    #            aes_string(x=dat, y="`Annualized Return`"), 
    #            size=5, colour="green") +
    # 
    # geom_label(data= stats0$stats["Bench",], 
    #            aes_string(x=dat, y="`Annualized Return`"),
    #            label=paste("Bench", typ, ": ", 
    #                        stats0$stats["Bench",grepl(type, colnames(db[last,]))]),
    #            hjust=1.2) +
    # 
    # geom_point(data= stats0$stats["BE500T",],
    #            aes_string(x=dat, y="`Annualized Return`"), 
    #            size=5, colour="purple") +
    # 
    # geom_label(data= stats0$stats["BE500T",], 
    #            aes_string(x=dat, y="`Annualized Return`"),
    #            label=paste("BE500T", typ, ": ", 
    #                        stats0$stats["BE500T",grepl(type, colnames(db[last,]))]),
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
