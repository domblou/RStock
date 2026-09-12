####
#### The objective of this script is to create a list of StockSymbols and save it in .csv
####

#### Get symbols from http://eoddata.com/stocklist/TSX/A.htm
library(XML)
library(quantmod)

#### Load R files of RStock
source("Settings.R")

# Symbols <- NULL
# for (i in LETTERS){
#   
#   url = paste("http://eoddata.com/stocklist/TSX/", i, ".htm", sep = "")
#   Symbols = append(Symbols, readHTMLTable(url, header=F, which=6, stringsAsFactors=F)[,1])
# }
# 
# write.csv(Symbols[2:length(Symbols)], StockSymbolsFile, row.names = FALSE)

symbols <- stockSymbols()
symbols <- symbols[,1]

write.csv(symbols, SymbolsFile, row.names = FALSE)

#### Remove object from environnement 
rm(list=ls())

