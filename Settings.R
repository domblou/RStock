####
#### This script contains the settings of RStock
####

#### Parameters
testModeMaxSymbols <- 500 #### Limit the number of symbols
testModeStockSymbols <- NULL #c("AAAAA","AAV","ABT", "ABX") #### c("AAAA.AAA", "AAV","ABT") or NULL to use StockSymbolsFile

WorkingDirectory <- "/Users/dominicblouin/Documents/R Stock ML project"
ModelsDirectory <- "./Models"
SymbolsFile <- "Symbols.csv"
SymbolsToSurveyFile <- "SymbolsToSurvey.csv"
PredictionResultFile <- "Prediction.csv"
SymbolsHistoryFile <- "SymbolsHistory.csv"

maxNumberOfSymbolsPerCall <- 1 #### Maximum number of symbols per call (15 for yahoo), SET TO 1 TO HANDLE ERROR PER SYMBOL
nbDaysHistoryModel <- 730 #### Number of days of historical data
nbDaysHistoryPredict <- 10 #### Number of days of historical data for predictions (previous days to validate the successfulness of the prediction)

UPDW_threshold <- 0.01 #### Significative variation, ignore lower than that
nbPermutation <- 3 #### Number of permutations to generate (a, b and c with a permutation of 1 would generate a+b, a+c, b+a, b+c, c+a and c+b.
#### A permutation of 3 with this set would generate the permutation 2 and a+b+c, a+c+b, c+a+c, b+c+a, c+a+b, and c+b+a)
#### Max 4 to get things under control (meaning sets of 2 to 5).
useDateInfoToPredict <- "" #### set the regex to "(wday|yday|mon)|" in order to use date info to predict

#### xBoost parameter to calibrate the models
xBoost.max.depth <- 6
xBoost.eta <- 1 
xBoost.nthread <- 2 
xBoost.nround <- 4

keepPredictorUnder <- 0.2 #### Predictor to keep for daily survey, also indicating which a model must be saved on disk. The lower the better 
#### when dealing with a large number of permutation.
