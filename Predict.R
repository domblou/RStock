####
#### The objective of this script is to predict based on daily survey
####

# TODO : add evaluate mode

# 1. In this mode, only get 2 days of history and only fetch symbols with precision under, say 10%. Only fetch these symbols.
# 2. Do not create the model, instead load it from file.
# 3. For each sets (using today's values), predict the outcome for tomorrow, and log buy orders only.
#    The day after, log the real outcome.
# 4. Keep Open and Close values in order to keep track record of performance.

#### Remove object from environnement 
rm(list=ls())

#### Load libraries
library(data.table)
library(dplyr)
library(stringr)

#### Load R files of RStock
source("Settings.R")
source("Functions.R")

startTime <- Sys.time()
print(paste("Start time:", startTime))

#### SetWorkingDirectory
setwd(WorkingDirectory)

SymbolsToSurvey <- read.csv(file=SymbolsToSurveyFile, sep=",", header=TRUE, colClasses = "character")

StockSymbols <- NULL
#### Set symbols to fetch (those where error percentage is under keepPredictorUnder)
for (i in 2:ncol(SymbolsToSurvey)){
  if (colnames(SymbolsToSurvey)[i] %like% "V"){
    colnames(SymbolsToSurvey)[i] <- c("V0")
    StockSymbols <- rbind(StockSymbols, SymbolsToSurvey[i])
  }
}

#### Extract unique symbols to fetch for daily survey
StockSymbols <- as.character(unique(StockSymbols %>% dplyr::filter(V0 != "<NA>"))[[1]])

#### Get the last days the stock prices were downloaded
# if(file.exists(SymbolsHistoryFile)){
#   SymbolsHistory <- read.csv(file=SymbolsHistoryFile, sep=",", header=TRUE, colClasses = "character")
#   LastDataDownloaded <- max(SymbolsHistory$)
#   nbDaysHistoryPredict <- 1
# } else {
#   SymbolsHistory <- as.data.frame(NULL)
# }

#### Call RStock.GetSymbols wrapper to get stock prices
StockAndSymbols <- RStock.GetSymbols(StockSymbols, nbDaysHistory=nbDaysHistoryPredict)
Stock <- as.data.frame(StockAndSymbols[1])
StockSymbols <- StockAndSymbols[[2]]

#### Get the prepared dataset
StockUpDw <- RStock.PrepareDataSet(Stock)

#### Only take the last to day (done like that to reuse RStock.PrepareDateSet. Otherwize just create an other function) 
StockUpDw <- head(RStock.PrepareDataSet(Stock),1)

#### Remove UPDW columns and rename to DAY_MINUS_1_UPDW
StockUpDw <- StockUpDw[,-which(names(StockUpDw) %like% c("DAY_MINUS_1_UPDW"))]
StockUpDw <- StockUpDw %>% 
  rename_at(.vars = vars(ends_with(".UPDW")),
            .funs = funs(sub("[.]UPDW$", ".DAY_MINUS_1_UPDW", .)))

flag <- 1
for (i in list.files(ModelsDirectory, full.names = FALSE)){

#i <- list.files(ModelsDirectory, full.names = FALSE)[1]
  colnamesSet <- c()
  
  SplitedSymbols <- strsplit(str_sub(i, 1, str_length(i)-4), "-")[[1]]
  SplitedSymbols <- SplitedSymbols[SplitedSymbols!="NA"]
  
  Set <- as.data.frame(t(as.data.frame(strsplit(str_sub(i, 1, str_length(i)-4), "-")[[1]])))
  
  for (j in 1:(nbPermutation+1)){
    if  (j==1) {
      colnamesSet <- append(colnamesSet, c("Observation"))
    } else {
      colnamesSet <- append(colnamesSet, c(paste("Feature",j-1, sep="")))
    }
  }
  colnames(Set) <- colnamesSet
  #rownames(Set) <- rownames(StockUpDw[1,])

  Set$Date <- as.Date.character(rownames(StockUpDw[1,])) + 1
  Set$Set <- str_sub(i, 1, str_length(i)-4)
  
  #### Order columns
  Set <- Set[c(c("Date", "Set"), colnamesSet)]
  
  Set$BinaryPrediction <- 0
  Set$BinaryResult <- 0
  
  if (flag==1) {
    Sets <- Set[1,-which(1!=1)]
  }
  flag <- 0
  
  GeneratedSets <- t(as.data.frame(SplitedSymbols, stringAsFactor=FALSE))
  
  #### Preparation to filter the column names for the generatedSet
  CombNames <- useDateInfoToPredict
  if (CombNames=="") {
     CombNames <-"(xxxxxxxxxxxxx)"
   }
   
  #### Filter predictor column (-1 to remove extra column Err and Model)
  for (j in 1:ncol(GeneratedSets)){
    
    #### Filter <NA> and outcome column
    if (!is.na(GeneratedSets[1,j]) & GeneratedSets[1,1] != GeneratedSets[1,j]) {
      #### append symbols. Use current day value (not previous day like when we are creating models)
      CombNames <- paste(CombNames, "|(",GeneratedSets[1,j],".DAY_MINUS_1_UPDW",")", sep="")
    }
  }
  
  #### Load the saved model
  load(paste(ModelsDirectory, "/", i, sep =""))
  
  predictorNames <- names(StockUpDw)[grep(CombNames, names(StockUpDw))]
  
  #### If symbols were correctly fetch and columns have been found, predict
  if (length(predictorNames) > 0) {
    #### Predict from the model
    predictions = predict(bst, as.matrix(StockUpDw[1,predictorNames]))
    
    #### Calculate de binary prediction (0 or 1, with 1 being positive)
    binary_predictions <- as.numeric(predictions > 0.5)
    
    Set[1,"BinaryPrediction"] <- binary_predictions

    if (exists("Sets")) {
      Sets <- rbind(Sets, Set)
    }else {
      Sets <- Set
    }
    
  }
  
  rm(bst)

}

#### Read SymbolsToSurvey in order save the error margin for this prediction
SetsErr <- as.data.frame(sapply(Sets$Set, function(x){
 
  SymbolsToSurvey[SymbolsToSurvey$Set==x,"Err"]
}), stringsAsFactors = FALSE)

colnames(SetsErr) <- c("Err")
Sets <- cbind(Sets, SetsErr)

if(file.exists(PredictionResultFile)){
  write.table(Sets, PredictionResultFile, append=T, row.names=F, col.names=F, sep = ",")
} else {
  write.csv(Sets, PredictionResultFile, row.names=F)
}

endTime <- Sys.time()
print(paste("End time:", endTime))
executionTime <- round(difftime(endTime, startTime, units="mins"), 2)
print(paste("Execution time (mins):", executionTime))


#### Remove object from environnement 
rm(list=ls())
