####
#### This script contains the function of the project
####

#### Load R files of RStock
source("Settings.R")

RStock.GetSymbols <- function(StockSymbols, maxNumberOfSymbolsPerCall=1, nbDaysHistory=nbDaysHistoryModel) {
  
  #### Load libraries
  library(xts)
  
  startDate = Sys.Date() - nbDaysHistory
  
  #print(paste("nbDaysHistory", nbDaysHistory, sep = " "))
  #print(paste("startDate", startDate, sep = " "))
  
  # sapply(StockSymbols, function(x){
  #   try(
  #     getSymbols(
  #       x,
  #       from=startDate),
  #     silent=TRUE)
  # 
  #    # try(
  #    #   print(get(x, envir=WoW)),
  #    #   Stock <- cbind(Stock, get(x, envir=WoW)),
  #    #   silent=TRUE)
  # 
  # })
  
  Stock <- NULL
  ErrorStockSymbols <- NULL
  
  #### Get symbols info by batch of maxNumberOfSymbolsPerCall
  for (i in 1:ceiling(length(StockSymbols)/maxNumberOfSymbolsPerCall)){
    if (i > length(StockSymbols)/maxNumberOfSymbolsPerCall){
      #### last iteration for incomplete maxNumberOfSymbolsPerCall
      x <- i
      y <- length(StockSymbols) %% maxNumberOfSymbolsPerCall
    } else {
      #### Iterate for full sets of maxNumberOfSymbolsPerCall
      x <- (i*maxNumberOfSymbolsPerCall)-(maxNumberOfSymbolsPerCall-1)
      y <- i*maxNumberOfSymbolsPerCall
    }
    
    try(
      getSymbols(StockSymbols[x:y], from=startDate),
      silent=TRUE)
    
    for (j in StockSymbols[x:y]){
      
      result <- tryCatch({
        Stock <- cbind(Stock, get(j))
        dummy <- "" #### silent the result
      }, error=function(err) {
        return(j)
      })
      
      #### Catch symbols that could not be fetch
      if (result!="") {
        ErrorStockSymbols = cbind(ErrorStockSymbols, result)
      }
      
      #### Remove the object from environnement
      try(
        rm(list=j),
        silent=TRUE)
      
    }
  }
  
  #### Filter StockSymbols to remove unfetched symbols
  if (!is.null(ErrorStockSymbols)) {
    StockSymbols <- StockSymbols[!grepl(paste(ErrorStockSymbols, collapse="|"), StockSymbols)]
  }

  #print(StockSymbols)
  #print(data.frame(as.xts(Stock)))
  list(data.frame(as.xts(Stock)),StockSymbols)
  
}

#### 
#### Function to prepare data set in order to create models and predict
####
RStock.PrepareDataSet <- function(Stock) {
  
  Stock <- data.frame(as.xts(Stock))
  
  Stock <- xts(Stock,order.by=as.Date(rownames(Stock)))
  Stock <- as.data.frame(Stock)
  
  #### Only select Open and Close column
  StockOpClUpDw <- cbind(select(Stock, ends_with(".Open")), select(Stock, ends_with(".Close")))
  
  #### Iterate through all symbols and add OPCL and UDDW (flag)
  for (i in StockSymbols){
    
    opcl <- 1 - select(Stock, matches(gsub("\\s", "", paste(i, ".Open")))) / select(Stock, matches(gsub("\\s", "", paste(i, ".Close"))))
    colnames(opcl) <- gsub("\\s", "", paste(i, ".OPCL"))
    
    StockOpClUpDw <- cbind(StockOpClUpDw, opcl)
    
    updw <- ifelse(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".OPCL")))) >= UPDW_threshold, 1, 0)
    colnames(updw) <- gsub("\\s", "", paste(i, ".UPDW"))
    
    StockOpClUpDw <- cbind(StockOpClUpDw, updw)
    
    #### TODO : Adjust to take into account days off (weekends and public holidays)
    opcl_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".OPCL")))),1)
    if (is.null(opcl_minus_1)) {
      opcl_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".OPCL")))),2)
    } 
    if (is.null(opcl_minus_1)) {
      opcl_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".OPCL")))),3)
    }
    if (is.null(opcl_minus_1)) {
      opcl_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".OPCL")))),4)
    }
    if (is.null(opcl_minus_1)) {
      opcl_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".OPCL")))),5)
    }
    colnames(opcl_minus_1) <- gsub("\\s", "", paste(i, ".DAY_MINUS_1_OPCL"))
    
    StockOpClUpDw <- cbind(StockOpClUpDw, opcl_minus_1)
    
    updw_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".UPDW")))),1)
    if (is.null(updw_minus_1)) {
      updw_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".UPDW")))),2)
    } 
    if (is.null(updw_minus_1)) {
      updw_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".UPDW")))),3)
    }
    if (is.null(updw_minus_1)) {
      updw_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".UPDW")))),4)
    }
    if (is.null(updw_minus_1)) {
      updw_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".UPDW")))),5)
    }
    colnames(updw_minus_1) <- gsub("\\s", "", paste(i, ".DAY_MINUS_1_UPDW"))
    
    StockOpClUpDw <- cbind(StockOpClUpDw, updw_minus_1)  
  }
  
  #### Remove Stock from environnement
  rm(list="Stock")
  
  #### Filter to keep only OPCL and UDDW columns
  StockOpClUpDw <- cbind(select(StockOpClUpDw, ends_with("OPCL")), select(StockOpClUpDw, ends_with("UPDW")))
  
  #### Reorder Columns
  StockOpClUpDw <- StockOpClUpDw[,order(names(StockOpClUpDw))]
  
  #### Remove OPCL for the first attemp at predicting (want to try predicting UPDW first, then may be categorize UPDW increment)
  StockUpDw <- select(StockOpClUpDw, ends_with("UPDW"))
  
  #### Remove Stock from environnement
  rm(list="StockOpClUpDw")
  
  #### Cast date to date type and sort
  StockUpDw$date <- as.Date(row.names(StockUpDw))
  StockUpDw <- StockUpDw[order(as.Date(StockUpDw$date, "%m/%d/%Y"), decreasing = TRUE),]
  
  #### Add significant date elements to the data frame
  StockUpDw$wday <- as.POSIXlt(StockUpDw$date)$wday
  StockUpDw$yday <- as.POSIXlt(StockUpDw$date)$yday
  StockUpDw$mon <- as.POSIXlt(StockUpDw$date)$mon
  
  #### Remove first rows with NAs
  StockUpDw <- StockUpDw[1:nrow(StockUpDw)-1,]
  
  #### Deal with NAs to each column, TODO, validate the approches... 
  StockUpDw <- StockUpDw %>% replace(., is.na(.), 0)
  
  #### Remove Date column
  StockUpDw <- subset(StockUpDw, select=-c(date))
  
}

####
#### Function to generate sets in order to create models (combinaisons and permutations)
####
RStock.GenerateSets <- function(StockSymbols, nbPermutation = 1) {

  #### Warning when nbPermutation is greater than then number of symbols
  if (nbPermutation>=length(StockSymbols)){
    stop("The number of permutation must be lower than the number of symbols")
    
  } else {
    for (j in 1:nbPermutation){
      if (j == 1){
        #### Generate pertumations instead of combinaison in order to have firts level combinaison a compared to b, b compared to a, etc.
        GeneratedSets <- as.data.frame(permutations(StockSymbols, 2))
        colnamesSet <- c("V0","V1")
        colnames(GeneratedSets) <- colnamesSet
      } else {
        #### Generate combinaison for each symbols (generate b+c for a, a+c for b and a+b for c, but not c+b for a... since it the same)
        
        #### Add a third column to normalize de data frame and rename it so that it fits the next data frame
        GeneratedSets[,j+1] <- NA
        colnamesSet <- append(colnamesSet, c(paste("V",j, sep="")))
        colnames(GeneratedSets) <- colnamesSet
        
        for (i in StockSymbols){
          Symbol <- as.data.frame(i)
          colnames(Symbol) <- "V0"
          Combs <- as.data.frame(combinations(StockSymbols[StockSymbols!=i], j))
          Combs <- cbind(Symbol, Combs)
          GeneratedSets <- rbind(GeneratedSets,Combs)
        }
        
      }
    }
  }
  
  GeneratedSets
  
}
