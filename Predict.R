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

#### Parameters
WorkingDirectory <- "/Users/dominicblouin/Documents/R Stock ML project"
ModelsDirectory <- "/Users/dominicblouin/Documents/R Stock ML project/Models"
SymbolsFile <- "Symbols.csv"
SymbolsToSurveyFile <- "SymbolsToSurvey.csv"

startTime <- Sys.time()
print(paste("Start time:", startTime))

#### SetWorkingDirectory
setwd(WorkingDirectory)

SymbolsToSurvey <- read.csv(file=SymbolsToSurveyFile, sep=",", header=TRUE, colClasses = "character")

StockSymbols <- NULL
#### Set symbols to fetch (those where error percentage is under keepPredictorUnder)
for (i in 2:ncol(SymbolsToSurvey)){
  colnames(SymbolsToSurvey[i]) <- c("V0")
  StockSymbols <- rbind(StockSymbols, SymbolsToSurvey[i])
}

print(StockSymbols)

endTime <- Sys.time()
print(paste("End time:", endTime))
executionTime <- round(difftime(endTime, startTime, units="mins"), 2)
print(paste("Execution time (mins):", executionTime))
