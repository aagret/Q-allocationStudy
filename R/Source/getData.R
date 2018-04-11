
# function to read hostorical datas from .csv files
getData <- function(file= filename) {
    
    returns <- read_excel(file, na="NA")
    returns <- returns[complete.cases(returns),]
    setDT(returns)
    returns[, Dates:=as.Date(Dates)]
    
    # use D as DEQ prices before 17.6.15
    returns[Dates < as.Date("2015-06-17"), DEQ:= D]
    returns[, D:=NULL]
    
    # use XEU rates before 1.1.1999
    returns[Dates < as.Date("1999-01-01"), EU1m:= XE1m]
    returns[, XE1m:=NULL]
    
    # get Q proforma datas
    q <- fread("RawData/Q Capital sim net daily returns.csv")
    colnames(q) <- c("Dates", "Q")
    q[, Dates:= as.Date(Dates, format= "%d.%m.%Y")]

    # merge both datas
    setkey(returns, Dates)
    setkey(q, Dates)
    
    returns <- merge(returns, q, all=TRUE)
    returns[is.na(returns)] <- 0

    # use Qu real from 27.2.2018
    q <- returns[Dates >= as.Date("2018-02-27"), ROC(Qu, type="discrete")]
    returns[Dates > as.Date("2018-02-27"), Q:= q[-1]]
    rm(q)
    
    # compute Q as index
    returns[, Q:= cumprod(1 + Q) * 100]
    

    returns[, Hdg_Cst:= 1 + 
                c(0, diff(returns$Dates)) *
                shift(returns$EU1m - returns$US1m - 0.25, 1) /
                36500]
    
    returns$Hdg_Cst[1] <- 1
    returns[, Hdg_Cst:= cumprod(Hdg_Cst)]
    
    # temporary remove rates data
    dev <- returns[, c("Dates", "US1m", "EU1m")]
    returns[, `:=` (US1m= NULL, EU1m= NULL)]

    # return xts of log returns
    returns <- as.xts(ROC(returns))
    returns[1,] <- 0
    
    # add dev datas (non log return)
    returns <- cbind(returns, as.xts(dev))
    rm(dev)
    
    # compute hedge indexes log returns
    returns$Q_hdg    <- returns$Q    + returns$Hdg_Cst
    returns$SPX_hdg  <- returns$SPX  + returns$Hdg_Cst
    returns$SPXT_hdg <- returns$SPXT + returns$Hdg_Cst
    
    return(returns)
        
}
