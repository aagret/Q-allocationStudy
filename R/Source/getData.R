
# function to read hostorical datas from .csv files
getData <- function(db= filename) {
    
    db <- fread(db)
    db[, Date:= as.Date(Date, format= "%d.%m.%Y")]
    db[is.na(db)] <- 0
    db <- as.xts(db)
    
}