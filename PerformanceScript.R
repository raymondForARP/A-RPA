# libraries (data cleaning)
library(stringr)
library(plyr)
library(tidyverse)
library(lubridate)
library(readr)
# libraries (database)
library(DBI)
library(RMariaDB)

#Sets working dir
setwd('/Users/charlesraymond/A-RPA/')


# Write to MySQL
# rmariadb.settingsfile <-"/Users/ndflip7/desktop/arpa.cnf"

#sets the settings like username & password
rmariadb.settingsfile <- "/Users/charlesraymond/Desktop/arpaConfig/arpa.cnf"

table <- "equity"

#sets db parameter and gets the connection of the sql database
rmariadb.db <- 'arpa'
db <- dbConnect(RMariaDB::MariaDB(), 
                default.file=rmariadb.settingsfile, 
                group=rmariadb.db )

#Gets a list of all the tables in the db
theTables <- dbListTables(conn=db)


#this returns the whole specific table
getTable <- function(tableName){
  name <- paste("SELECT date_id, segment, asset_class FROM ", tableName, ";", sep = "")
  #final <- paste(first, ";", sep = "")
  table <- dbGetQuery(conn = db, name)
  return(table)
}

#practice
equity <- getTable(theTables[2])
#print(equity)

#returns a table of the specific query. Expected file path
getQuery <- function(fileName){
  table <- dbGetQuery(conn = db, statement = read_file(fileName))
  return(table)
}

#Gets the individual segment
getSegment <- function(theTables, segment){
  for(tableName in theTables){
    name <- paste("SELECT date_id, segment, asset_class FROM ", tableName, ";", sep = "")
    #final <- paste(first, ";", sep = "")
    table <- dbGetQuery(conn = db, name)
  }
}

#Sets the amount of digits to keep in a double
options(digits=10)

#equity_count <- length(segment[segment=="equity"])

#Converts the string columns to doubles
convertColumn <- function(table){
  table <- transform(table, price = as.double(price))
  table <- transform(table, unit = as.double(unit))
  table <- transform(table, exchange_rate = as.double(exchange_rate))
  return(table)
}



calculateEquityHolding <- function(price, unit){
  holding <- unit * price #* Beta(1 for now) /Fx (currency exchange rate)
  return(holding)
}

##This will be 0 in total
calculateFutureHolding <- function(price, unit){
  holding <- unit * price #* Beta(contract size)/FX(currency exchange rate)##Exposure
  return(holding)
}

##This will be 0 in total
calculateForwardHolding <- function(price, unit){
  holding <- price * unit #* Beta/Fx (Nominal_value=unit)###Exposure
  return(holding)
}

calculateOptionHolding <- function(price, unit){
  holding <- price * unit #* Beta
  return(holding)
}
#Exposure = quanitity * contract size * underlying * delta * FX

calculateEquityHolding <- function(price, unit){
  holding <- price * unit #* Beta
  return(holding)
}


#This will calculate the holdings
calculateHoldings <- function(price, unit, beta=1, exchange_rate=1){
  holding <- (price * unit * beta)/exchange_rate#* Beta
  return(holding)
}

#This calculates the total holdings, notice that
#options, forwards, and futures are all set to 0
#Returns a list of all the total holdings
calculateTotalHoldings<-function(table){
  
  total <- 0
  equity <- 0
  extra <- 0
  forwards <- 0
  futures <- 0
  imargin <- 0
  options <- 0
  ordinary <- 0
  overnight <- 0
  deposit <- 0
  current <- 0
  
  for(i in 1:nrow(table)){
      switch(table[['asset_class']][i],
             "equity" = {
                        current <- calculateHoldings(table[['price']][i], table[['unit']][i])
                        equity <- equity + current
                        total <- current + total
              },
              "forwards" = {
                        forwards <- 0 
                        total <- forwards + total
              },
             "futures" = {
                        futures <- 0 
                        total <- futures + total#just take out and simplify???
             },
             "imargin" ={
                        current <- calculateHoldings(table[['price']][i], table[['unit']][i])
                        imargin <- imargin + current
                        total <- current + total
             },
             "options" = {
                        options <- 0 
                        total <- options + total
             },
             "ordinary" = {
                        current <- calculateHoldings(table[['price']][i], table[['unit']][i])
                        ordinary <- ordinary + current
                        total <- current + total
             },
             "overnight" = {
                        current <- calculateHoldings(table[['price']][i], table[['unit']][i])
                        overnight <- overnight + current
                        total <- current + total
             },
             "deposit" = {
                        current <- calculateHoldings(table[['price']][i], table[['unit']][i])
                        deposit <- deposit + current
                        total <- current + total
             }
              )
  }
  theList <- list(total, equity, extra, forwards, futures, imargin, options, ordinary, overnight, deposit)
  return(theList)
}

#This calculates the total Exposure, notice that
#options, forwards, and futures are all set to 0
#Returns a list of all the total holdings
calculateTotalExposure<-function(table){
  
  total <- 0
  equity <- 0
  extra <- 0
  forwards <- 0
  futures <- 0
  imargin <- 0
  options <- 0
  ordinary <- 0
  overnight <- 0
  deposit <- 0
  current <- 0
  
  for(i in 1:nrow(table)){
    switch(table[['asset_class']][i],
           "equity" = {
             current <- calculateHoldings(table[['price']][i], table[['unit']][i])
             equity <- equity + current
             total <- current + total
           },
           "forwards" = {
             current <- calculateHoldings(table[['price']][i], table[['unit']][i])
             forwards <- forwards + current
             total <- current + total
           },
           "futures" = {
             current <- calculateHoldings(table[['price']][i], table[['unit']][i])
             futures <- futures + current
             total <- current + totall
           },
           "imargin" ={
             current <- calculateHoldings(table[['price']][i], table[['unit']][i])
             imargin <- imargin + current
             total <- current + total
           },
           "options" = {
             current <- calculateHoldings(table[['price']][i], table[['unit']][i])
             options <- options + current
             total <- current + total
           },
           "ordinary" = {
             current <- calculateHoldings(table[['price']][i], table[['unit']][i])
             ordinary <- ordinary + current
             total <- current + total
           },
           "overnight" = {
             current <- calculateHoldings(table[['price']][i], table[['unit']][i])
             overnight <- overnight + current
             total <- current + total
           },
           "deposit" = {
             current <- calculateHoldings(table[['price']][i], table[['unit']][i])
             deposit <- deposit + current
             total <- current + total
           }
    )
  }
  theList <- list(total, equity, extra, forwards, futures, imargin, options, ordinary, overnight, deposit)
  return(theList)
}

#Calculates the percent of the asset class to the entire fund
#Returns a list that goes in order of 
#total, equity, extra, forwards, futures, imargin, options, ordinary, overnight, deposit
calculatePercentHoldings <- function(theList){
  total <- theList[[1]]
  print(total)
  print(typeof(total))
  theList <- list(theList[[1]]/total,theList[[2]]/total,theList[[3]]/total,theList[[4]]/total,theList[[5]]/total,theList[[6]]/total,
                  theList[[7]]/total,theList[[8]]/total,theList[[9]]/total)
  return(theList)
}


#Test stuff is below this line
segment <- getQuery("segmentUnion.sql")
print(segment)

for(i in 1:nrow(segment)){
  print(segment[i,])
}
newSegment <- convertColumn(segment)
print(newSegment)

value <- calculateTotalHoldings(newSegment)
print(value)

percentValue <- calculatePercentHoldings(value)
print(percentValue)

#print(calculateHolding(newSegment))


dbDisconnect(db)





