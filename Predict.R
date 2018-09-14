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

#### Load Functions.R
source("Functions.R")

#### Parameters
WorkingDirectory <- "/Users/dominicblouin/Documents/R Stock ML project"
ModelsDirectory <- "/Users/dominicblouin/Documents/R Stock ML project/Models"
SymbolsFile <- "Symbols.csv"
SymbolsToSurveyFile <- "SymbolsToSurvey.csv"

useDateInfoToPredict <- "" #### set the regex to "(wday|yday|mon)|" in order to use date info to predict

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

#### Call RStock.GetSymbols wrapper to get stock prices
StockAndSymbols <- RStock.GetSymbols(StockSymbols, nbDaysHistory=1)
Stock <- as.data.frame(StockAndSymbols[1])
StockSymbols <- StockAndSymbols[[2]]

#### Get the prepared dataset
StockUpDw <- RStock.PrepareDataSet(Stock)

for (i in list.files(ModelsDirectory, full.names = FALSE)){
  print(as.data.frame(strsplit(i, "-")[[1]]), stringAsFactor=FALSE)
  
  #### Preparation to filter the column names for the generatedSet
  CombNames <- useDateInfoToPredict
  if (CombNames=="") {
    CombNames <-"(xxxxxxxxxxxxx)"
  }
  
  ModelFileName <- GeneratedSets[i,1]
  #### Filter predictor column (-1 to remove extra column Err and Model)
  for (j in 2:(ncol(GeneratedSets)-1)){
    
    #### Filter <NA> and outcome column
    if (!is.na(GeneratedSets[i,j]) & GeneratedSets[i,1] != GeneratedSets[i,j]) {
      #### append symbols
      CombNames <- paste(CombNames, "|(",GeneratedSets[i,j],".DAY_MINUS_1_UPDW",")", sep="")
    }
  }
  
  predictorNames <- names(StockUpDw)[grep(CombNames, names(StockUpDw))]
  
  #### Predict from the model
  predictions = predict(bst, as.matrix(StockUpDw[rowToPredict,predictorNames]))
  
  #### 
  binary_predictions <- as.numeric(predictions > 0.5)
}

endTime <- Sys.time()
print(paste("End time:", endTime))
executionTime <- round(difftime(endTime, startTime, units="mins"), 2)
print(paste("Execution time (mins):", executionTime))
