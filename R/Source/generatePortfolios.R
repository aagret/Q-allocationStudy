
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

