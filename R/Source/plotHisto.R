
# function to plot historical returns
plotHisto <- function(db= database) {
    
    per <- paste0(start(db),"/",end(db))
    
    # db[1,] <- 0
    db <- exp(cumsum(db))
    db <- melt(as.data.table(db), "index")
    
    ggplot(data=db) + 
        ggtitle(per) + 
        theme(legend.position=c(0, 0.8)) +
        geom_line(aes(x=index, y=value, group=variable, color=variable))
    
}
