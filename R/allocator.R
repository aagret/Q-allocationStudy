###########  ###########
########  Main  ########
########################


###########LOAD and set datas 
#############################

data <- list()

# merge .csv datas
data$indexes$returns <- cbind(getData("RawData/returns.csv"),
                              getData("RawData/navQ.csv"), 
                              getData("RawData/rates.csv"))

# compute hedging cost for usd assets             
data$indexes$returns$Hdg_Cst <- lag(data$indexes$returns$EUR - data$indexes$returns$USD - 0.375) *
    c(0, diff(index(data$indexes$returns))) / 36500

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


########### Generate Portfolio mix
##################################

data$port1$returns   <- generatePortfolios(data$indexes$returns$DEQ,  data$indexes$returns$Q_hdg)
data$port2$returns   <- generatePortfolios(data$indexes$returns$SPXT, data$indexes$returns$Q)




########
##############************************

# define study period
period  <- c("2013-02-28/2018-01-31",
             "2015-06-17/2018-01-31",
             "2016-12-30/2018-01-31",
             
             "2013-02-28/2015-05-14", 
             "2014-05-15/2016-01-29",
             "2016-01-29/2016-11-11",
             "2016-11-11/2017-08-17",
             "2017-08-17/2018-01-31"
             )



studies <- lapply(period, function(x) getStats(data, x))


saveRDS(studies, "TidyData/studies.rds")

######################



