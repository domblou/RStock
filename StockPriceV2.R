# TODO : add evaluate mode

# 1. In this mode, only get 2 days of history and only fetch symbols with precision under, say 10%.
# 2. Do not create the model, instead load it from file.
# 3. For each sets (using today's values), predict the outcome for tomorrow, and log buy orders only.
#    The day after, log the real outcome.
# 4. Keep Open and Close values in order to keep track record of performance.

a <- 5

####
#### The objective of this script is to identify which symbols are related to others in order to predict the next day variation.
#### It iterates through all symbols, finds the right number of observation in order to predict each symbols. Each symbol has 
#### it's optimized combinaison of symbols which are saved for predicting the daily variations. The models are reviewed daily.
#### Only predictions with an ROC higher than .75 are considered for the daily prediction. The results of the prediction is append
#### to PredictionHistory.csv to later validate it against the actual variation and keep track of performance.
####
#### The script also append the result of the previous day prediction in PredictionHistory.csv, again in order to keep track of 
#### performance.
####

#### Prerequisites
####
#### Local file StockSymbols.csv with one column and one symbol per line

#### Remove object from environnement 
rm(list=ls())

#### Install pakages
# install.packages("quantmod")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("lubridate")
# install.packages("xgboost")
# install.packages("pROC")
# install.packages("igraph")
# install.packages("DiagrammeR")
# install.packages("arrangements")

#### Load libraries
library(quantmod)
library(dplyr)
library(xts)
library(stringr)
library(lubridate)
library(xgboost)
library(pROC)
library(igraph)
library(DiagrammeR)
library(arrangements)

#### Parameters
testMode <- TRUE #### Limit the number of symbols according to testModeMaxSymbols
testModeMaxSymbols <- 15 #### Only use X symbols
testModeStockSymbols <- NULL #c("AAAAA","AAV","ABT", "ABX") #### c("AAAA.AAA", "AAV","ABT") or NULL to use StockSymbolsFile

WorkingDirectory <- "/Users/dominicblouin/Documents/R Stock ML project"
StockSymbolsFile <- "StockSymbolsTMX.csv"
maxNumberOfSymbolsPerCall <- 1 #### Maximum number of symbols per call (15 for yahoo), SET TO 1 TO HANDLE ERROR PER SYMBOL
nbDaysHistory <- 365 #### Number of days of historical data
UPDW_threshold <- 0.005 #### Significative variation, ignore lower than that
nbPermutation <- 2 #### Number of permutations to generate (a, b and c with a permutation of 1 would generate a+b, a+c, b+a, b+c, c+a and c+b.
                   #### A permutation of 3 with this set would generate the permutation 2 and a+b+c, a+c+b, c+a+c, b+c+a, c+a+b, and c+b+a)
                   #### Max 4 to get things under control (meaning sets of 2 to 5).
useDateInfoToPredict <- "" #### set the regex to "(wday|yday|mon)|" in order to use date info to predict
xBoost.max.depth <- 6
xBoost.eta <- 1 
xBoost.nthread <- 2 
xBoost.nround <- 4

#### SetWorkingDirectory
setwd(WorkingDirectory)

startTime <- Sys.time()
print(paste("Start time:", startTime))

#### Set symbols to fetch
StockSymbols <- read.csv(file=StockSymbolsFile, sep=";", header=TRUE, colClasses = "character")$x

if (testMode){
  if (length(testModeStockSymbols)==0){
    StockSymbols <- StockSymbols[1:testModeMaxSymbols]
  }
  else {
    StockSymbols <- testModeStockSymbols[1:length(testModeStockSymbols)]
  }
}

#### Merge data frame of each stock together
Stock <- NULL

startDate = Sys.Date() - nbDaysHistory

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

#### Stop execution when no symbols fetched
if (length(StockSymbols)==0) {
  stop("No symbols fetched")
} else {
  
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
    
    opcl_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".OPCL")))),1)
    colnames(opcl_minus_1) <- gsub("\\s", "", paste(i, ".DAY_MINUS_1_OPCL"))
    
    StockOpClUpDw <- cbind(StockOpClUpDw, opcl_minus_1)
    
    updw_minus_1 <- quantmod::Lag(select(StockOpClUpDw, matches(gsub("\\s", "", paste(i, ".UPDW")))),1)
    colnames(updw_minus_1) <- gsub("\\s", "", paste(i, ".DAY_MINUS_1_UPDW"))
    
    StockOpClUpDw <- cbind(StockOpClUpDw, updw_minus_1)  
  }
  
  #### Remove Stock from environnement
  rm(list="Stock")
  
  #### Filter to keep only OPL and UDDW columns
  StockOpClUpDw <- cbind(select(StockOpClUpDw, ends_with("OPCL")), select(StockOpClUpDw, ends_with("UPDW")))
  
  #### Reorder Columns
  StockOpClUpDw <- StockOpClUpDw[,order(names(StockOpClUpDw))]
  
  #### Remove OPCL for the first attemp at predicting (want to try predicting UPDW first, then may be categorize UPDW increment)
  StockUpDw <- select(StockOpClUpDw, ends_with("UPDW"))
  
  #### Cast date to date type and sort
  StockUpDw$date <- as.Date(row.names(StockUpDw))
  StockUpDw <- StockUpDw[order(as.Date(StockUpDw$date, "%m/%d/%Y"), decreasing = TRUE),]
  
  #### Add significant date elements to the data frame
  StockUpDw$wday <- as.POSIXlt(StockUpDw$date)$wday
  StockUpDw$yday <- as.POSIXlt(StockUpDw$date)$yday
  StockUpDw$mon <- as.POSIXlt(StockUpDw$date)$mon
  
  #### Remove first rows with NAs
  StockUpDw <- StockUpDw[1:nrow(StockUpDw)-1,]
  
  #### Remove Date column
  StockUpDw <- subset(StockUpDw, select=-c(date))
  
  #### Shuffle the data
  StockUpDw <- StockUpDw[sample(nrow(StockUpDw)),]
  
  #### For each symbols, try to predict outcome based on others
  ####
  #### IMPROVEMENT : 1- Find the right symbols to predict outcome (for instance only 3 might be better than all).
  ####                  For this, we need to save the setting for all symbols to predict.
  ####
  
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
        #### Generate combinaison for each symbols (generate b+c for a, a+c for b and a+b for c, do not c+b for a... since it the same)
        
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
  
  #### Deal with NAs to each column, TODO, validate the approches... 
  StockUpDw <- StockUpDw %>% replace(., is.na(.), 0)
  
  #### Iterate through all genarated sets
  GeneratedSets$Err <- 0.0
  for (i in 1:nrow(GeneratedSets)){
    #### Set column to predict
    StockUpDw$outcome <- select(StockUpDw, matches(gsub("\\s", "", paste(GeneratedSets[i,1], ".UPDW"))))
    
    #### set NAs to 0 TODO, validate the approches...
    StockUpDw$outcome[is.na(StockUpDw$outcome)] <- 0
    
    #### Preparation to filter the column names for the generatedSet
    CombNames <- useDateInfoToPredict
    if (CombNames=="") {
      CombNames <-"(xxxxxxxxxxxxx)"
    }
   
    for (j in 2:ncol(GeneratedSets)){
      #### Filter <NA>
      if (!is.na(GeneratedSets[i,j])) {
        #### append symbols
        CombNames <- paste(CombNames, "|(",GeneratedSets[i,j],".DAY_MINUS_1_UPDW",")", sep="")
      }
    }
    predictorNames <- names(StockUpDw)[grep(CombNames, names(StockUpDw))]
  
    set.seed(1234)
    split <- sample(nrow(StockUpDw), floor(0.7*nrow(StockUpDw)))
    train <-StockUpDw[split,]
    test <- StockUpDw[-split,]
    
    bst <- xgboost(data = as.matrix(train[,predictorNames]), 
                   label = as.matrix(train$outcome), 
                   verbose = 0,
                   max.depth = xBoost.max.depth,
                   eta = xBoost.eta, 
                   nthread = xBoost.nthread, 
                   nround = xBoost.nround,
                   objective = "binary:logistic",
                   missing = NA)
    
    # importance_matrix <- xgb.importance(predictorNames, model = bst)
    # 
    # xgb.plot.importance(importance_matrix)
    # xgb.plot.tree(feature_names = predictorNames, model = bst)
    # xgb.plot.multi.trees(model = bst, feature_names = predictorNames)
    # xgb.plot.deepness(model = bst)
    
    predictions = predict(bst, as.matrix(test[,predictorNames]))
    
    binary_predictions <- as.numeric(predictions > 0.5)
    
    err <- mean(binary_predictions != as.matrix(test[,predictorNames]))
    
    GeneratedSets[i,"Err"] <- err
    

    
    #print(paste(paste(paste("Symbol: ", GeneratedSets[i,1], collapse=" "), paste(predictorNames, collapse=" ")), paste("Error pct:", err, collapse=" ")))
    
    # cv.res <- xgb.cv(data = as.matrix(train[,predictorNames]), 
    #                  label = as.matrix(train$outcome), 
    #                  nfold = 5,
    #                  nrounds = 20, 
    #                  objective = "binary:logistic",
    #                  early_stopping_rounds = 10, 
    #                  maximize = FALSE)
    
  }
  
  print(tail(arrange(GeneratedSets,desc(Err)), n = 10))
  
  write.csv(GeneratedSets, "Resultat.csv")

}

# library(pROC)
# auc <- roc(test$outcome, predictions)
# print(paste('AUC score:', auc$auc))

endTime <- Sys.time()
print(paste("End time:", endTime))
executionTime <- round(difftime(endTime, startTime, units="mins"), 2)
print(paste("Execution time (mins):", executionTime))
