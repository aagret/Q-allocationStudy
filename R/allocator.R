###########  ###########
########  Main  ########
########################


###########LOAD and set datas 
#############################



# get returns datas
returns <- getData("RawData/indexes.xlsx")
write.csv(returns, "TidyData/returns.csv")

# remove Qu and Qe due to lack of history
returns$Qu <- returns$Qe <- NULL


# compute DEQ ex cash asset selection performance
DEQi <- fread("RawData/mergeNAV.csv",select= c(1:4))
DEQi[, DEQi:= (abs(Futures) + Securities) / NAV]

DEQi[, Dates:= as.Date(Date)]
DEQi <- DEQi[, .(Dates, DEQi)]
DEQi <- as.xts(DEQi)

returns <- cbind(returns, DEQi)
rm(DEQi)

# carry previous cash pos to next day if missing
returns$DEQi <- na.locf(returns$DEQi)

# calc theo ex cash return
returns$DEQex <- returns$DEQ / returns$DEQi
returns[is.na(returns)] <- 0


# comupte DEQ ex Cash&Fees&Tax Performance from Bloom Port
DEQia <- fread("RawData/DEQexBloom.csv")
DEQia[, Dates:= as.Date(Date, format="%d.%m.%Y")]
DEQia <- DEQia[, .(Dates, DEQia)]
DEQia <- as.xts(DEQia)

returns <- cbind(returns, DEQia)


# carry previous cash pos to next day if missing
returns$DEQia <- na.locf(returns$DEQia)

# calc theo ex cash return
returns$DEQexa <- returns$DEQ / returns$DEQia
returns[is.na(returns)] <- 0

# compute Q + DEQ ex cash history
returns$QasCash <- (returns$DEQexa * pmax(0.90, returns$DEQia)) +
    (returns$Q_hdg * pmin(0.1, (1 - returns$DEQia)))

returns$QasCash["/2013-02-28"] <- 0

# compute Q5% + DEQ ex cash history
returns$Q5 <- (returns$DEQexa * pmax(0.95, returns$DEQia)) +
    (returns$Q_hdg * pmin(0.05, (1 - returns$DEQia)))

returns$Q5["/2015-06-17"] <- 0

# compute Q + DEQ ex cash history
returns$Q10 <- (returns$DEQexa * pmax(0.90, returns$DEQia)) +
    (returns$Q_hdg * pmin(0.1, (1 - returns$DEQia)))

returns$Q10["/2015-06-17"] <- 0

#rm(DEQia)

# remove non index datas
returns$DEQi <- returns$DEQia <- returns$Hdg_Cst <- NULL

data <- list()
data$indexes$returns <- returns


########### Generate Portfolio mix
##################################

data$port1$returns   <- generatePortfolios(data$indexes$returns$DEQ,    data$indexes$returns$Q_hdg)
data$port2$returns   <- generatePortfolios(data$indexes$returns$DEQexa, data$indexes$returns$Q_hdg)




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
             "2017-08-17/"
             )


studies <- lapply(period, function(x) getStats(data, x))

saveRDS(studies, "TidyData/studies.rds")

######################



