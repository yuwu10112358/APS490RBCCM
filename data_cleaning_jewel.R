source('constants.r')
#setwd("/Users/jewelho/Desktop/Capstone/Code/APS490RBCCM")
# The "RWeka" package and the options gives more space to store the data
# XLConnect package is for "readWorksheetFromFile" function
options( java.parameters = "-Xmx6g" )
#install.packages("RWeka")
library( "RWeka" )
#install.packages("XLConnect")
library(XLConnect)


# Instructions on how to use get data
# 0. CHANGE WORKING DIRECTOR & "filename" 
# 1.Run the code separately (or else it will take a long time to run)
# 2.Load data: change stock symbols then run "data_extraction"
# 3.Data cleaning: Run data_cleaning and data_cleaning2 separately
# 4.Done. Want to check what the data looks like? Run the commented lines below
# head(global_tables[["BMO_tick"]])
# tail(global_tables[["SPTSX_ask"]][1,1])

#=================================================================
#available stocks: AC,BNS,BMO,SPTSX
env <- global_tables
symbol <- "AC" 
filename <- "/Users/jewelho/dropbox/Capstone_Data_TSX/TSXdatafile.xlsx"
stock_name = c()
EquityList <- c("tick", "ask", "bid")
for (i in 1:3){stock_name[i] <- paste(symbol,EquityList[i],sep="_")}


#=======================================================================
data_extraction(filename, env, symbol, stock_name[1], stock_name[2], stock_name[3])
for (Name in stock_name) {
  data_cleaning(filename, env, symbol,Name)
  data_cleaning2(filename, env, symbol,Name)
}

#=====================================================================================
data_extraction <- function(filename, env, symbol, tick_name, bid_name, ask_name){ 
  #Definition: This function creates tables (tick, bid and ask) of stock prices. It imports stock price data from an Excel file that links to the Bloomberg terminal.
  #The excel sheet contains 3 tables arranged in order: Tick, Ask, Bid price.
  #Number of columns in each table can vary
  file <- readWorksheetFromFile(filename,sheet= symbol, startRow = 3,check.names = FALSE)
  mylist = c()
  mylist[1] = 1
  for(i in 1:length(file)){
    if(is.na(file[,i])) {
      mylist[length(mylist)+1] = i
    }}
  
  env[[tick_name]] = file[, mylist[1]:   (mylist[2] - 1)] 
  env[[bid_name]]  = file[,(mylist[2]+1):(mylist[3] - 1)]
  env[[ask_name]]  = file[,(mylist[3]+1):length(file)]
}
data_cleaning <- function(filename, env, symbol,Name){
  # Remove NA col
  maxrow <- nrow(env[[Name]])
  env[[Name]] = env[[Name]][complete.cases(env[[Name]][1:maxrow,] ) ,]
  # maxrow <- nrow(env[[tick_name]])
  # global_tables[["ABX_tick"]] = global_tables[["ABX_tick"]][complete.cases(global_tables[["ABX_tick"]][1:maxrow,] ) ,]
  
}
data_cleaning2 <- function(filename, env, symbol,Name){
  # Remove close market data
  Opentime <- as.POSIXct("2000-01-01 09:30:00", tz = "EST")
  Opentime <-strftime(Opentime, format="%H:%M:%S")
  Closetime <- as.POSIXct("2000-01-01 16:00:00", tz = "EST")
  Closetime <-strftime(Closetime, format="%H:%M:%S")
  row_to_keep <- logical(length = nrow(env[[Name]]) )
  
  for (i in 1:nrow(env[[Name]])){ 
    temp <- strftime(env[[Name]][i,1], format="%H:%M:%S")
    if ( temp >= Opentime && temp <= Closetime){
      row_to_keep[i] <- TRUE
    }else {
      row_to_keep[i] <- FALSE
      
    }
  }
  env[[Name]] = env[[Name]][row_to_keep,]
}

getquotes(global_tables,c("AC","BNS"),"2015-05-13 10:42:00 EDT")

getquotes<-function(env,symbol,time)
{
  # return (Nstocks * rows) 
  # if no quote then return empty
  # aftermarket hour, 930-4 then return empty
  # this function returns an updated mkt_quote table
  mkt_quote = data.frame(matrix(NA, length(symbol), length(mkt_quote_spec)))
  colnames(mkt_quote) <- mkt_quote_spec
  
  time_930 <- get_time_since_open(as.POSIXct("2000-01-01 09:30:00", tz = "EST"))
  time_931 <- get_time_since_open(as.POSIXct("2000-01-01 09:31:00", tz = "EST"))
  time_1559 <- get_time_since_open(as.POSIXct("2000-01-01 15:59:00", tz = "EST"))
  time_1600 <- get_time_since_open(as.POSIXct("2000-01-01 16:00:00", tz = "EST"))
  
  for (i in 1:length(symbol)){
    
    #time <- "2000-01-01 09:30:00 EST"
    
    datatable_name_tick <- paste(symbol[i], Con_Data_Tick_Suffix, sep = "")
    datatable_name_bid <- paste(symbol[i], Con_Data_Bid_Suffix, sep = "")
    datatable_name_ask <- paste(symbol[i], Con_Data_Ask_Suffix, sep = "")
    
    time_since_open <- get_time_since_open(as.POSIXct(time))
    
    if (time_since_open == time_930){
      # 9:30 then opening tick for 5 prices, val/vol/tick = 0
      j <- (1:nrow(env[[datatable_name_tick]]))[env[[datatable_name_tick]][[Con_Data_ColName_Date]] == time]
      mkt_quote[i, Con_FieldName_Sym] <- symbol[i]
      mkt_quote[i, Con_FieldName_CurrentBid] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][j]
      mkt_quote[i, Con_FieldName_CurrentAsk] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][j]
      mkt_quote[i, Con_FieldName_CurrentTick] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][j]
      mkt_quote[i, Con_FieldName_LastHighestBid] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][j]
      mkt_quote[i, Con_FieldName_LastLowestAsk] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][j]
      mkt_quote[i, Con_Data_ColName_LastNumTicks] <- 0
      mkt_quote[i, Con_Data_ColName_LastVolume] <- 0
      mkt_quote[i, Con_Data_ColName_LastValue] <- 0
      
    }
    else if (time_since_open == time_1600){
      # 15:59 close tick,val/vol/tick
      
      j <- (1:nrow(env[[datatable_name_tick]]))[env[[datatable_name_tick]][[Con_Data_ColName_Date]] == time - 60]
      mkt_quote[i, Con_FieldName_Sym] <- symbol[i]
      mkt_quote[i, Con_FieldName_CurrentBid] <- env[[datatable_name_tick]][[Con_Data_ColName_LastPrice]][j]
      mkt_quote[i, Con_FieldName_CurrentAsk] <- env[[datatable_name_tick]][[Con_Data_ColName_LastPrice]][j]
      mkt_quote[i, Con_FieldName_CurrentTick] <- env[[datatable_name_tick]][[Con_Data_ColName_LastPrice]][j]
      mkt_quote[i, Con_FieldName_LastHighestBid] <- env[[datatable_name_bid]][[Con_Data_ColName_High]][j]
      mkt_quote[i, Con_FieldName_LastLowestAsk] <- env[[datatable_name_ask]][[Con_Data_ColName_Low]][j]
      mkt_quote[i, Con_Data_ColName_LastNumTicks] <- env[[datatable_name_tick]][[Con_Data_ColName_NumTicks]][j]
      mkt_quote[i, Con_Data_ColName_LastVolume] <- env[[datatable_name_tick]][[Con_Data_ColName_Volume]][j]
      mkt_quote[i, Con_Data_ColName_LastValue] <- env[[datatable_name_tick]][[Con_Data_ColName_Value]][j]
    }
    
    else if (time_since_open >= time_931 & time_since_open <= time_1559){
      j <- (1:nrow(env[[datatable_name_tick]]))[env[[datatable_name_tick]][[Con_Data_ColName_Date]] == time]
      mkt_quote[i, Con_FieldName_Sym] <- symbol[i]
      mkt_quote[i, Con_FieldName_CurrentBid] <- env[[datatable_name_bid]][[Con_Data_ColName_Open]][j]
      mkt_quote[i, Con_FieldName_CurrentAsk] <- env[[datatable_name_ask]][[Con_Data_ColName_Open]][j]
      mkt_quote[i, Con_FieldName_CurrentTick] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][j]
      mkt_quote[i, Con_FieldName_LastHighestBid] <- env[[datatable_name_bid]][[Con_Data_ColName_High]][j-1]
      mkt_quote[i, Con_FieldName_LastLowestAsk] <- env[[datatable_name_ask]][[Con_Data_ColName_Low]][j-1]
      mkt_quote[i, Con_Data_ColName_LastNumTicks] <- env[[datatable_name_tick]][[Con_Data_ColName_NumTicks]][j-1]
      mkt_quote[i, Con_Data_ColName_LastVolume] <- env[[datatable_name_tick]][[Con_Data_ColName_Volume]][j-1]
      mkt_quote[i, Con_Data_ColName_LastValue] <- env[[datatable_name_tick]][[Con_Data_ColName_Value]][j-1]
    }
    else{
    }
  }
  
  # this function returns an updated mkt_quote table
  return (mkt_quote)
}
