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

#### Load Functions.R
source("Functions.R")

#### Parameters
testMode <- TRUE #### Limit the number of symbols according to testModeMaxSymbols
testModeMaxSymbols <- 15 #### Only use X symbols
testModeStockSymbols <- NULL #c("AAAAA","AAV","ABT", "ABX") #### c("AAAA.AAA", "AAV","ABT") or NULL to use StockSymbolsFile

WorkingDirectory <- "/Users/dominicblouin/Documents/R Stock ML project"
ModelsDirectory <- "/Users/dominicblouin/Documents/R Stock ML project/Models"
SymbolsFile <- "Symbols.csv"
SymbolsToSurveyFile <- "SymbolsToSurvey.csv"

maxNumberOfSymbolsPerCall <- 1 #### Maximum number of symbols per call (15 for yahoo), SET TO 1 TO HANDLE ERROR PER SYMBOL
nbDaysHistory <- 365 #### Number of days of historical data

UPDW_threshold <- 0.005 #### Significative variation, ignore lower than that
nbPermutation <- 2 #### Number of permutations to generate (a, b and c with a permutation of 1 would generate a+b, a+c, b+a, b+c, c+a and c+b.
                   #### A permutation of 3 with this set would generate the permutation 2 and a+b+c, a+c+b, c+a+c, b+c+a, c+a+b, and c+b+a)
                   #### Max 4 to get things under control (meaning sets of 2 to 5).
useDateInfoToPredict <- "" #### set the regex to "(wday|yday|mon)|" in order to use date info to predict

#### xBoost parameter to calibrate the models
xBoost.max.depth <- 6
xBoost.eta <- 1 
xBoost.nthread <- 2 
xBoost.nround <- 4

keepPredictorUnder <- 0.1 #### Predictor to keep for daily survey, also indicating which a model must be saved on disk. The lower the better 
                          #### when dealing with a large number of permutation.

startTime <- Sys.time()
print(paste("Start time:", startTime))

#### SetWorkingDirectory
setwd(WorkingDirectory)

#### If directory doesn't exists, create it, else delete files (models) in it
if (!dir.exists(ModelsDirectory)){
  dir.create(ModelsDirectory, showWarnings = FALSE)
} else {
  do.call(file.remove, list(list.files(ModelsDirectory, full.names = TRUE)))
}

#### Set symbols to fetch
StockSymbols <- read.csv(file=SymbolsFile, sep=";", header=TRUE, colClasses = "character")$x

if (testMode){
  if (length(testModeStockSymbols)==0){
    StockSymbols <- StockSymbols[1:testModeMaxSymbols]
  }
  else {
    StockSymbols <- testModeStockSymbols[1:length(testModeStockSymbols)]
  }
}
#### Call RStock.GetSymbols wrapper to get stock prices
StockAndSymbols <- RStock.GetSymbols(StockSymbols)
Stock <- as.data.frame(StockAndSymbols[1])
StockSymbols <- StockAndSymbols[[2]]

#### Get the prepared dataset for prediction
StockUpDw <- RStock.PrepareDataSet(Stock)

#### Shuffle the data
StockUpDw <- StockUpDw[sample(nrow(StockUpDw)),]

#### For each symbols, try to predict outcome based on others
GeneratedSets <- RStock.GenerateSets(StockSymbols, nbPermutation)

#### Deal with NAs to each column, TODO, validate the approches... 
StockUpDw <- StockUpDw %>% replace(., is.na(.), 0)

#### Iterate through all genarated sets
GeneratedSets$Err <- 0.0

#### For each generated set, build a model
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
 
  ModelFileName <- GeneratedSets[i,1]
  #### Filter predictor column (-1 to remove extra column Err and Model)
  for (j in 2:(ncol(GeneratedSets)-1)){

    #### Filter <NA> and outcome column
    if (!is.na(GeneratedSets[i,j]) & GeneratedSets[i,1] != GeneratedSets[i,j]) {
      #### append symbols
      CombNames <- paste(CombNames, "|(",GeneratedSets[i,j],".DAY_MINUS_1_UPDW",")", sep="")
    }
    
    #### Set model file name 
    ModelFileName <- paste(ModelFileName, GeneratedSets[i,j], sep="-")
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
  
  #### Predict from the model
  predictions = predict(bst, as.matrix(test[,predictorNames]))
  
  #### 
  binary_predictions <- as.numeric(predictions > 0.5) 
  
  #### Calculate the percentage of error
  err <- mean(binary_predictions != as.matrix(test[,predictorNames]))
  
  GeneratedSets[i,"Err"] <- err
  
  #### Save the model to file for daily scan
  if (err < keepPredictorUnder) {
    save(bst, file = paste(ModelsDirectory, "/", ModelFileName, ".rda", sep =""))
  }

  #print(paste(paste(paste("Symbol: ", GeneratedSets[i,1], collapse=" "), paste(predictorNames, collapse=" ")), paste("Error pct:", err, collapse=" ")))
  
  # cv.res <- xgb.cv(data = as.matrix(train[,predictorNames]), 
  #                  label = as.matrix(train$outcome), 
  #                  nfold = 5,
  #                  nrounds = 20, 
  #                  objective = "binary:logistic",
  #                  early_stopping_rounds = 10, 
  #                  maximize = FALSE)
  
}

print(subset(GeneratedSets, Err < keepPredictorUnder))

write.csv(subset(GeneratedSets, Err < keepPredictorUnder), SymbolsToSurveyFile)


# library(pROC)
# auc <- roc(test$outcome, predictions)
# print(paste('AUC score:', auc$auc))

endTime <- Sys.time()
print(paste("End time:", endTime))
executionTime <- round(difftime(endTime, startTime, units="mins"), 2)
print(paste("Execution time (mins):", executionTime))
