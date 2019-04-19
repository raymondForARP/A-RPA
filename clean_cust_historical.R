# *************************************
# Cleaning Custodian Position Data    *
# by Nicholas Difilippo               *
# *************************************

# TODO (NICK) - Need to write error handle: if equity class does not exist 
# TODO (Nick & Charlie ) - modularize the code 
# TODO (NIck) - clean data types so sql numeric values work 
# TODO (Nick & Charlie ) - right now the code stops running if there is a 
# duplicate. 


# libraries (data cleaning)
library(stringr)
library(plyr)
library(tidyverse)
library(lubridate)
# libraries (database)
library(DBI)
library(RMariaDB)

setwd('/Users/charlesraymond/A-RPA/')


myData <-read.csv(file='/Users/charlesraymond/A-RPA/Mydata/MyData.csv', header=TRUE, sep=",")
#print(myData[['input.ticker']][2])
tickerList<- myData[['input.ticker']]
print(typeof(myData))
#print("Ticker List")
#print(tickerList)
missingIsin <- c()
#optionId <- c()

##converte EU date to normal date
findDate<- function(date){
  
  theDate <- c()
  
  string <- str_split(date, '-')
  #print(string)
  year = string [[1]][1]
  month = string[[1]][2]
  day <- str_split(string[[1]][3], ' ')
  day <- day[[1]][1]
  #print(day)
  
  stringDate <- paste(month,day,year,sep='/')
  #print(stringDate)
  stringDate <- mdy(stringDate)

  current <- stringDate
  #print(current)
  
  currentDay <- day(current)
  year <- year(current)
  month <- month(current)

  for(i in 1:7){
    
    currentDate <- paste(toString(month), toString(currentDay), toString(year), sep='/')
    #print(currentDate)
    #theDate <- paste(month,day,year,sep='/')
  
    theDate <- append(theDate, currentDate)
    
    currentDay <- currentDay - 1

  }
  #print(theDate)
  return(theDate)
}

findType <- function(type){
  if(type == "Call")
    return('C')
  else if(type == "Put")
    return('P')
  else
    return("NOPE")
}

##Come back to later, does first index twice (duplicate)
uniqueOptionID <- function(table, tickerList){
  match <- FALSE
  tick <- ''
  optionList <- c()
  for(i in 1:nrow(table))
  {
    #print(table[['maturity']][i])
    string <- table[['contract']][i]
    if(str_detect(string, "OPT.A.ESTX 50", negate = FALSE)){
      tick <- "sxe5"
    }
    #Make for s&p 500?
    #if(str_detect(string, "OPT.A.ESTX 50", negate = FALSE)){
      #tick <- "sxe5"
    #}
    
    maturityDate <- findDate(table[['maturity']][i])
    #print(maturityDate)
    type <- findType(table[['type_1']][i])
    type <- paste(type, table[['strike_price']][i], sep='')
    
    for(k in 1:7){
      #print(maturityDate[[k]])
      optionIndex <- paste(tick, maturityDate[[k]], type, 'Index', sep=' ')
      #print(optionIndex)
      optionList <- append(optionList, optionIndex)
    }
  }
  return(optionList)
}

isinEquityAndOrdinary <- function(table, tickerList){
  match <- FALSE
 # missingIsin <- list()
  
  ##EACH isin gets compared to the whole list of Isins
  ##when there is not a match(FALSE) we add it to a unique list
  for(i in 1:nrow(table)){
    
    isinName <- paste(table[['isin']][i], "EQUITY")#, sep = "")
    
    for(ticker in tickerList){
      
      if(isinName == ticker)
      {
        #print(isinName)
        match <- TRUE
      }
      else
        match <- FALSE
    }
      
    if(match == FALSE)
    {
      missingIsin <- append(missingIsin, isinName)
    }
  }
  return(missingIsin)
}




# controls for while loop 
again <- TRUE 
dates <- c("HI")


while (again == TRUE) { 
  perform <- TRUE
  
  # read in files
  temp <- list.files(path = "/Users/charlesraymond/A-RPA/", pattern = "*.csv")
  print(temp)
  
  if (length(temp) == 0 ){
    again <- TRUE
    print("temp is empty!")
  } else {
    setwd('/Users/charlesraymond/A-RPA/')
    #import all csv's 
    ##for some reason we are failing here...
    myfiles <- lapply(temp, read.delim, 
                      header = FALSE, sep = ',', stringsAsFactors = FALSE)#, the_dir="/Users/charlesraymond/A-RPA/")
    #myfiles <- lapply(temp, function(i){
                #read.csv(i, header=FALSE, sep = ',', stringsAsFactors = FALSE)
    
    current.date <- myfiles[[1]]$V1[4]
    print(current.date)
  }
  
  if(length(dates)!=0){
    
    for(theDate in dates){
      print(theDate)
      
      if(theDate == current.date){
        print("Perform = FALSE")
        perform <- FALSE
      }
      
    }
  }
  
  while (perform == TRUE) {
    #print("temp is no longer empty!")
    # define globabl variables 
    ordinary.master <- deposit.master <- options.master <- futures.master <-
      i.margin.master <- forwards.master <- overnight.master <- extra.master <-
      equity.master <- data.frame()
    # loop throught dfs 
    for (i in 1:length(myfiles)) {
      # data.df is the designated file for each iteration 
      data.df <- myfiles[[i]]
      # add segment 
      sgmnt <- rep(as.character(data.df$V2[3]), nrow(data.df))
      data.df <- cbind(sgmnt, data.df)
      
      # add date 
      date.temp <- data.df$V1[4]
      date.parse <- strsplit(date.temp, " ")
      date_id <- dmy(date.parse[[1]][3])
      data.df <- cbind(date_id, data.df)
      rm(date.temp, date.parse, date_id)
      
      # add block number 
      # create vector that contains location of where the word "Fund" occurs
      block.index <- which(data.df$V1 %in% "Fund")
      # add blocks column 
      count <- 0
      block.num <- 0
      for(x in 1:nrow(data.df)) {
        if (x %in% block.index) {
          count <- count + 1 
        }
        block.num[[x]] <- count 
      }

      # add block num to df 
      data.df <- cbind(block.num, data.df)
      data.df <- filter(data.df, block.num != 0) # remove blk 0 
      rm(block.index, block.num)
      
      # find asset class block function 
      find_block <- function(df, word, col) {
        if (ncol(df) < col) {
          return(NULL)
        }
        indx <- which(grepl(str_to_upper(word), str_to_upper(df[, col])))
        if (length(indx > 0)) {
          num <- unique((df[indx, 1]))
          block <- filter(df, df[, 1] %in% num)
          return(block)
        } else {
          return(NULL)
        }
      }

      
      # identify what block the asset is in, and create new data frame 
      # remove block from original data.df 
      # in new data frame, filter out only content that is useful 
      
      # non-useful blocks 
      # summary 
      summary   <- find_block(data.df, "summary", 4)
      data.df   <- filter(data.df, !(block.num %in% unique(summary$block.num)))
      # performance 
      perf      <- find_block(data.df, "performance", 4)
      data.df   <- filter(data.df, !(block.num %in% unique(perf$block.num)))
      # overnight deposit 
      overnight <- find_block(data.df, "counterparty", 12)
      data.df   <- filter(data.df, !(block.num %in% unique(overnight$block.num)))
      overnight <- overnight[-which(overnight$V8 == ""), ]
      # options 
      options   <- find_block(data.df, "Strike Price", 13)
      data.df   <- filter(data.df, !(block.num %in% unique(options$block.num)))
      options   <- options[-which(options$V2 == ""), ]  
      # futures 
      futures   <- find_block(data.df, "Book Price", 13)
      data.df   <- filter(data.df, !(block.num %in% unique(futures$block.num)))
      futures   <- futures[-which(futures$V2 == ""), ]  
      # forwards
      forwards  <- find_block(data.df, "Forward Rate", 8)
      data.df   <- filter(data.df, !(block.num %in% unique(forwards$block.num)))
      forwards  <- forwards[-which(forwards$V2 == ""), ]  
      # ordinary shares: securities and REIT's 
      ordinary  <- find_block(data.df, "Country", 8)
      data.df   <- filter(data.df, !(block.num %in% unique(ordinary$block.num)))
      ordinary  <- ordinary[-which(ordinary$V8 == ""), ]
      # bank deposit & variation margin
      deposit   <- find_block(data.df, "Receivable/ Liability EUR", 11) 
      data.df   <- filter(data.df, !(block.num %in% unique(deposit$block.num)))
      deposit   <- deposit[-which(deposit$V8 == ""), ]
      # initial margin & summary cash 
      i.margin  <- find_block(data.df, "Currency", 5 )  
      data.df   <- filter(data.df, !(block.num %in% unique(i.margin$block.num)))
      i.margin  <- i.margin[-which(i.margin$V8 == ""), ]
      # other equity. Funds, Gold, and ETF's 
      equity    <- find_block(data.df, "WKN", 5)  
      data.df   <- filter(data.df, !(block.num %in% unique(equity$block.num)))
      equity  <- equity[-which(equity$V8 == ""), ]
      # extra 
      data.df <- filter(data.df, block.num != 0)
      extra <- data.df 
      

      # create master list, which will be appended throughout the for loop 
      master <- function(df, m.df, count) {
        if (length(m.df) > 0) {
          if (length(df) > 0) {
            m.df <- rbind.fill(m.df, df[2:nrow(df), ])
            return(m.df)
          } else {
            return(m.df)
          }
        } else {
          if (length(df) > 0) {
            m.df <- df
            return(m.df)
          } else {
            return(m.df)
          }
        }
      }
      
      
      extra.master     <- master(extra,     extra.master,        i)
      equity.master    <- master(equity,    equity.master,       i)
      deposit.master   <- master(deposit,   deposit.master,      i)
      options.master   <- master(options,   options.master,      i)
      futures.master   <- master(futures,   futures.master,      i)
      ordinary.master  <- master(ordinary,  ordinary.master,     i)
      i.margin.master  <- master(i.margin,  i.margin.master,     i)
      forwards.master  <- master(forwards,  forwards.master,     i)
      overnight.master <- master(overnight, overnight.master,    i)
      
      # TODO(nick) : Need to break up equity into 2 data frames, ordinary shares 
      # and other 
    }
   # print("WE GET HERE 2")
    
    # remove unused data 
    rm(ordinary, extra, myfiles, overnight, summary, count, deposit, forwards,
       futures, i, i.margin, options, perf, sgmnt, 
       data.df, equity, x)
    
   # print("WE GET HERE 3")

    # add asset class name to df 
    extra.master <- cbind("extra", extra.master)
   # print("WE GET HERE 4")
    
    ordinary.master <- cbind("ordinary", ordinary.master)
    equity.master <- cbind("equity", equity.master)
    deposit.master <- cbind("deposit and variation margin", deposit.master)
    options.master <- cbind("options", options.master)
    futures.master <- cbind("futures", futures.master)
    i.margin.master <- cbind("initial margin and summary cash", i.margin.master)
    #print("WE GET HERE 4")
    
    forwards.master  <- cbind('forwards',  forwards.master)
    #print("WE GET HERE 4")
    
    if (length (overnight.master) != 0 ){
      overnight.master <- cbind("overnight deposit", overnight.master)
    } 
    #overnight.master <- cbind("overnight deposit", overnight.master)
    
    #print("WE GET HERE 5")
    
    # rename columns 
    colnames(ordinary.master) <- ordinary.master[1, ] 
    ordinary.master <- ordinary.master[-1, ]
    colnames(equity.master) <- equity.master[1, ] 
    equity.master <- equity.master[-1, ]
    colnames(extra.master) <- extra.master[4, ] 
    extra.master <- extra.master[c(-1:-4), ]
    colnames(deposit.master) <- deposit.master[1, ] 
    deposit.master <- deposit.master[-1, ]
    colnames(options.master) <- options.master[2, ] 
    options.master <- options.master[c(-1,-2), ]
    colnames(futures.master) <- futures.master[2, ] 
    futures.master <- futures.master[c(-1,-2), ]
    colnames(i.margin.master) <- i.margin.master[1, ] 
    i.margin.master <- i.margin.master[-1, ]
    colnames(forwards.master) <- forwards.master[2, ] 
    forwards.master <- forwards.master[c(-1,-2), ]
    #print("WE GET HERE 6")
    
    if (length (overnight.master) != 0 ){
      colnames(overnight.master) <- overnight.master[1, ] 
      overnight.master <- overnight.master[-1, ]
    } 
    #colnames(overnight.master) <- overnight.master[1, ] 
    #overnight.master <- overnight.master[-1, ]
    #print("WE GET HERE 6")
    
    # rename col #1 
    new_col <- c("asset_class", "block", "date_id", "segment")
    colnames(extra.master)[1:4] <- new_col
    colnames(ordinary.master)[1:4] <- new_col
    colnames(equity.master)[1:4] <- new_col
    colnames(deposit.master)[1:4] <- new_col
    colnames(options.master)[1:4] <- new_col
    colnames(futures.master)[1:4] <- new_col
    colnames(i.margin.master)[1:4] <- new_col
    colnames(forwards.master)[1:4] <- new_col
    
    if (length (overnight.master) != 0 ){
      colnames(overnight.master)[1:4] <- new_col
    } 
    #colnames(overnight.master)[1:4] <- new_col
    #print("WE GET HERE 7")
    
    # make column names unique 
    colnames(extra.master) <- make.names(colnames(extra.master),unique = T)
    colnames(ordinary.master) <- make.names(colnames(ordinary.master), unique = T)
    colnames(equity.master) <- make.names(colnames(equity.master), unique = T)
    colnames(deposit.master) <- make.names(colnames(deposit.master), unique = T)
    colnames(options.master) <- make.names(colnames(options.master), unique = T)
    colnames(futures.master) <- make.names(colnames(futures.master), unique = T)
    colnames(i.margin.master) <- make.names(colnames(i.margin.master), unique = T)
    colnames(forwards.master) <- make.names(colnames(forwards.master), unique = T)
    colnames(overnight.master) <- make.names(colnames(overnight.master), unique = T)
    
    # remove unncessary rows in the data frame 
    extra.master <- filter(extra.master, ISIN != "ISIN")
    extra.master <- filter(extra.master,  !(ISIN  %in% extra.master$segment))
    extra.master <- filter(extra.master,  ISIN != "")        
    equity.master <- filter(equity.master, ISIN != "")
    ordinary.master <- filter(ordinary.master, ISIN != "ISIN")
    deposit.master <- filter(deposit.master, Currency != "Currency")
    options.master <- filter(options.master, Contract != "Contract")
    futures.master <- filter(futures.master, Contract != "Contract")
    i.margin.master <- filter(i.margin.master, Currency != "Currency")
    forwards.master <- filter(forwards.master, Maturity != "Maturity")
    overnight.master <- filter(overnight.master, Currency != "Currency")
    #print("WE GET HERE 8")
    
    # add type & counter party to forwards 
    colnames(forwards.master)[5] <- "Type"
    short.loc <- which(forwards.master[ ,6] == "Short Position")
    forwards.master$Type[1:(short.loc)-1] <- "Long"
    forwards.master$Type[short.loc:nrow(forwards.master)] <- "Short"
    forwards.master$counter.party <- "Citibank N.A. [London Branch]"
    
    # clean forwards 
    forwards.master <- filter(forwards.master, 
                              Counterparty...Currency != 
                                forwards.master$Counterparty...Currency[2])
    forwards.master <- filter(forwards.master, 
                              Counterparty...Currency != 
                                "Short Position")
    forwards.master <- filter(forwards.master, 
                              Counterparty...Currency != 
                                "Long Position")
    
    # remove empty columns. Ex) X.1 -> X.n contain no information 
    del_Xn <- function(df) { 
      # delete column == "X" 
      if("X" %in% colnames(df)) {
        df <- select(df, -X)
      }
      # Delete NA. 
      if("NA." %in% colnames(df)) {
        df <- select(df, -NA.)
      }
      new.df <- df[, which(!str_detect(string = colnames(df), 
                                       pattern = "^X.[0-9]"))]
      # Delete block column 
      final <- select(new.df, -block)
      return(final)
    }
    extra.master     <- del_Xn(extra.master)
    equity.master    <- del_Xn(equity.master)
    deposit.master   <- del_Xn(deposit.master)
    options.master   <- del_Xn(options.master)
    futures.master   <- del_Xn(futures.master)
    ordinary.master  <- del_Xn(ordinary.master)
    i.margin.master  <- del_Xn(i.margin.master)
    forwards.master  <- del_Xn(forwards.master)
    
    if (length (overnight.master) != 0 ){
      overnight.master <- del_Xn(overnight.master)
    } 
    #overnight.master <- del_Xn(overnight.master)
    #print("WE GET HERE 9")
    
    ###!!!!!!!!!
    #SET FORWARD EXHCANGE TO 1
    ###!!!!!!!!!
    
    # Rename columns to fit SQL database 
    colnames(extra.master) <- tolower(gsub("\\.", "_", colnames(extra.master)))
    colnames(equity.master) <- tolower(gsub("\\.", "_", colnames(equity.master)))
    colnames(deposit.master) <- tolower(gsub("\\.", "_", colnames(deposit.master)))
    colnames(options.master) <- tolower(gsub("\\.", "_", colnames(options.master)))
    colnames(futures.master) <- tolower(gsub("\\.", "_", colnames(futures.master)))
    colnames(ordinary.master) <- tolower(gsub("\\.", "_", colnames(ordinary.master)))
    colnames(i.margin.master) <- tolower(gsub("\\.", "_", colnames(i.margin.master)))
    colnames(forwards.master) <- tolower(gsub("\\.", "_", colnames(forwards.master)))
    colnames(overnight.master) <- tolower(gsub("\\.", "_", colnames(overnight.master)))
    
    
   # missingIsin <- isinEquityAndOrdinary(equity.master, tickerList)
    #missingIsin <- isinEquityAndOrdinary(ordinary.master, tickerList)
    optionId <- uniqueOptionID(options.master, tickerList)
    data.frame(optionId)
    
    write.table(optionId, "Mydata/option.csv", row.names=FALSE, col.names=FALSE)
    
    print(optionId)
    
    #print(missingIsin)
    #checkIsin(equity.master, myData)
    # clean data types 
    
    # Write to MySQL
     rmariadb.settingsfile <-"/Users/ndflip7/desktop/arpa.cnf"
     #rmariadb.settingsfile <- "/Users/charlesraymond/Desktop/arpaConfig/arpa.cnf"
    
    print("WE GET HERE 9")
    
    rmariadb.db <- 'arpa'
    db <- dbConnect(RMariaDB::MariaDB(), 
                    default.file=rmariadb.settingsfile, 
                    group=rmariadb.db )
    # all numeric int variables should not have commas 
    # equity.master <- as.data.frame(sapply(equity.master, # need to fix 
    #                                       gsub, 
    #                                       pattern = ",", 
    #                                       replacement= ""))
    dbWriteTable(db, 'equity', equity.master, row.names = FALSE, append = TRUE)
    dbWriteTable(conn=db, name="ordinary", ordinary.master, append = T, row.names = F)
    dbWriteTable(conn=db, name="extra", extra.master, append = T, row.names = F)
    dbWriteTable(conn=db, name="forwards", forwards.master, append = T, row.names = F)
    dbWriteTable(conn=db, name="futures", futures.master, append = T, row.names = F)
    dbWriteTable(conn=db, name="imargin", i.margin.master, append = T, row.names = F)
    dbWriteTable(conn=db, name="options", options.master, append = T, row.names = F)
    #added this
    print("fails here")
    dbWriteTable(conn=db, name="deposit", deposit.master, append = T, row.names = F)
    dbWriteTable(conn=db, name="overnight", overnight.master, append = T, row.names = F)
    dbDisconnect(db)
    
    # clear variables 
    rm(ordinary.master , deposit.master , options.master , futures.master ,
       i.margin.master , forwards.master , overnight.master , extra.master ,
       equity.master,  new_col, short.loc)
    

    #Sys.sleep(1) # sleep time, need to optimize so parrallel code works 
    temp <- list.files(pattern = "*.csv") 
    if (length(temp) == 0) {
      again <- FALSE
      break
    } # else read in the file 
    
    myfiles <- lapply(temp, read.delim, 
                      header = FALSE, sep = ',', stringsAsFactors = FALSE)
    current.date <- myfiles[[1]]$V1[4]
    if (current.date %in% dates){
      again <- FALSE
      break
    }
    
    print("It worked!")
    perform <-FALSE
  }

  print("WE GET HERE, time to sleep")
  Sys.sleep(10) # sleep time, need to optimize so parrallel code works 
  #dates <- c(dates, current.date)
  #print("THE LIST OF DATES")
  #print(dates)
  
  addDate <- TRUE
  
  if(length(dates)!=0){
    
    for(theDate in dates){
      print(theDate)
      
      if(theDate == current.date){
        print("addDate = FALSE")
        addDate <- FALSE
      }
    }
    
    if(addDate != FALSE)
    {
      dates <- c(dates, current.date)
      print("added Date!")
    }
  }

  
}


