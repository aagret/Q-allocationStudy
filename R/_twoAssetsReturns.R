## R Script used to analyse returns of DEQ, Q and some Indexes over various time 
## periods, and produce various mix of two assets protfolios to generate
## efficient frontiers based on historical risks/returns ratios.

########################
########  Init  ########
########################


#### initiate requested libraries ####
library(data.table)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)



#### set working directory ####
codeDir <- "//ARTHASERVER/Data/Alexandre/R-Projects/Allocator"

workDir <- "//ARTHASERVER/Data/Alexandre/R-Projects/Allocator"
setwd(workDir)


### load Functions ###
source(paste0(codeDir, "/R/allocatorFunctions.R"))

### start Main process ###
source(paste0(codeDir, "/R/allocator.R"))

## generate Doc

render(paste0(codeDir,"/R/Rmd/Report3.Rmd"))

#################
###### END ######
#################

#######################
########  End  ########
#######################
