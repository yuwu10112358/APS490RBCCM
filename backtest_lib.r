source('constants.r')
# setwd("/Users/jewelho/Desktop/Capstone/Code/APS490RBCCM")
# The "RWeka" package and the options gives more space to store the data
# XLConnect package is for "readWorksheetFromFile" function
options( java.parameters = "-Xmx6g" )
install.packages("RWeka")
library( "RWeka" )
install.packages("XLConnect")
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
# available stocks: AC,BNS,BMO,SPTSX
env <- global_tables
symbol <- "SPTSX" 
# the symbol is the same as the excel tab name
tick_name <-"SPTSX_tick"
bid_name <-"SPTSX_bid"
ask_name <-"SPTSX_ask"
filename <- "/Users/jewelho/dropbox/Capstone_Data_TSX/TSXdatafile.xlsx"
#=======================================================================


EquityList <- c(tick_name,bid_name,ask_name)
data_extraction(filename, env, symbol, tick_name, bid_name, ask_name)


for (Name in EquityList) {
  data_cleaning(filename, env, symbol, tick_name, bid_name, ask_name,Name)
  #data_cleaning2(filename, env, symbol, tick_name, bid_name, ask_name,Name)
}





data_extraction <- function(filename, env, symbol, tick_name, bid_name, ask_name)
{ 
  #Definition: This function creates tables (tick, bid and ask) of stock prices. It imports stock price data from an Excel file that links to the Bloomberg terminal.
  #Requirements:
  #The excel sheet contains 3 tables arranged in order: Tick, Ask, Bid price.
  #Number of columns in each table can vary
  file <- readWorksheetFromFile(filename, 
                                sheet= symbol, 
                                startRow = 3,
                                check.names = FALSE
  )
  
  #file <- read.xlsx(filename, sheet= symbol, startRow = 3,check.names = FALSE)
  mylist = c()
  mylist[1] = 1
  for(i in 1:length(file)){
    if(is.na(file[,i])) {
      mylist[length(mylist)+1] = i
    }}
  
  env[[tick_name]] = file[, mylist[1]:   (mylist[2] - 1)] 
  env[[bid_name]]  = file[,(mylist[2]+1):(mylist[3] - 1)]
  env[[ask_name]]  = file[,(mylist[3]+1):length(file)]
  # only consider complete cases (remove NA)
  #env[[bid_name]][complete.cases(env[[bid_name]]),]
  #env[[ask_name]][complete.cases(env[[ask_name]]),]
}
data_cleaning <- function(filename, env, symbol, tick_name, bid_name, ask_name,Name){
  # Remove NA col
  maxrow <- nrow(env[[Name]])
  env[[Name]] = env[[Name]][complete.cases(env[[Name]][1:maxrow,] ) ,]
  # maxrow <- nrow(env[[tick_name]])
  # global_tables[["ABX_tick"]] = global_tables[["ABX_tick"]][complete.cases(global_tables[["ABX_tick"]][1:maxrow,] ) ,]
  
}
data_cleaning2 <- function(filename, env, symbol, tick_name, bid_name, ask_name,Name){
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


# symbols 
# return (Nstocks * rows) 
# if no quote then return empty
# aftermarket hour, 930-4 then return empty

getquotes<-function(env,symbol,time){
  # getquotes (Nstocks * rows) 
  # time <- "2015-05-13 09:41:01 EDT"
  

  datatable_name_tick <- paste(symbol, Con_Data_Tick_Suffix, sep = "")
  datatable_name_bid <- paste(symbol, Con_Data_Bid_Suffix, sep = "")
  datatable_name_ask <- paste(symbol, Con_Data_Ask_Suffix, sep = "")
  
  #datable_table <- c(datatable_name_tick, datatable_name_bid, datatable_name_ask)
  #datable_list <- paste(symbol, datable_table, sep = "")
  
  mkt_quote = data.frame(matrix(NA, length(symbol), length(mkt_quote_spec)))
  colnames(mkt_quote) <- mkt_quote_spec
  
  mkt_quote[symbol, Con_FieldName_Sym] <- symbol
  mkt_quote[symbol, Con_FieldName_CurrentBid] <- env[[datatable_name_bid]][[Con_Data_ColName_Open]][[time]]
  mkt_quote[symbol, Con_FieldName_CurrentAsk] <- env[[datatable_name_ask]][[Con_Data_ColName_Open]][[time]]
  mkt_quote[symbol, Con_FieldName_CurrentTick] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[time]]
  mkt_quote[symbol, Con_FieldName_LastHighestBid] <- env[[datatable_name_bid]][[Con_Data_ColName_High]][[time-1]]
  mkt_quote[symbol, Con_FieldName_LastLowestAsk] <- env[[datatable_name_ask]][[Con_Data_ColName_Low]][[time-1]]
  mkt_quote[symbol, Con_Data_ColName_LastNumTicks] <- env[[datatable_name_tick]][[Con_Data_ColName_NumTicks]][[time-1]]
  mkt_quote[symbol, Con_Data_ColName_LastVolume] <- env[[datatable_name_tick]][[Con_Data_ColName_Volume]][[time-1]]
  mkt_quote[symbol, Con_Data_ColName_LastValue] <- env[[datatable_name_tick]][[Con_Data_ColName_Value]][[time-1]]
  
  
  
  # this function returns an updated mkt_quote table

  mkt_quote = data.frame(matrix(NA, length(symbol), length(mkt_quote_spec)))
  colnames(mkt_quote) <- mkt_quote_spec
  
  time_930 <- get_time_since_open(as.POSIXct("2000-01-01 09:30:00", tz = "EST"))
  time_931 <- get_time_since_open(as.POSIXct("2000-01-01 09:31:00", tz = "EST"))
  time_1559 <- get_time_since_open(as.POSIXct("2000-01-01 15:59:00", tz = "EST"))
  time_1600 <- get_time_since_open(as.POSIXct("2000-01-01 16:00:00", tz = "EST"))
  
  
  for (i in 1:length(symbol)){
  
  datatable_name_tick <- paste(symbol[i], Con_Data_Tick_Suffix, sep = "")
  datatable_name_bid <- paste(symbol[i], Con_Data_Bid_Suffix, sep = "")
  datatable_name_ask <- paste(symbol[i], Con_Data_Ask_Suffix, sep = "")
  
  datable_table <- c(datatable_name_tick, datatable_name_bid, datatable_name_ask)
  
  # datable_list <- paste(symbol, datable_table, sep = "")

  #loop through each symbol, look for the position of this 9:30 of each day.
  time <- "2000-01-01 09:30:00 EST"
  date <- strftime(time, format="%Y-%m-%d")
  timezone <- strftime(global_tables[["BNS_ask"]][12,1],format = "%Z")
  starttime <-paste(date, "09:30:00", timezone)
  
  
  for (j in 1:nrow(env[[datatable_name_tick]])){
    if(env[[datatable_name_tick]][[Con_Data_ColName_Date]][j] == starttime)
      break
  }
  for (j in 1:nrow(env[[datatable_name_bid]])){
    if(env[[datatable_name_bid]][[Con_Data_ColName_Date]][j] == starttime)
      break
  }
  for (j in 1:nrow(env[[datatable_name_ask]])){
    if(env[[datatable_name_ask]][[Con_Data_ColName_Date]][j] == starttime)
      break
  }
  
  
  if (time >= time_931 & time <= time_1559){
    mkt_quote[i, Con_FieldName_Sym] <- symbol
    mkt_quote[i, Con_FieldName_CurrentBid] <- env[[datatable_name_bid]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]]
    mkt_quote[i, Con_FieldName_CurrentAsk] <- env[[datatable_name_ask]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]]
    mkt_quote[i, Con_FieldName_CurrentTick] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]]
    mkt_quote[i, Con_FieldName_LastHighestBid] <- env[[datatable_name_bid]][[Con_Data_ColName_High]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_FieldName_LastLowestAsk] <- env[[datatable_name_ask]][[Con_Data_ColName_Low]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_Data_ColName_LastNumTicks] <- env[[datatable_name_tick]][[Con_Data_ColName_NumTicks]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_Data_ColName_LastVolume] <- env[[datatable_name_tick]][[Con_Data_ColName_Volume]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_Data_ColName_LastValue] <- env[[datatable_name_tick]][[Con_Data_ColName_Value]][[Con_Data_ColName_Date]][j-1]
  }
  
  else if (time == time_930){
    # 9:30 then opening tick for 5 prices, val/vol/tick = 0
    mkt_quote[i, Con_FieldName_Sym] <- symbol
    mkt_quote[i, Con_FieldName_CurrentBid] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]][j]
    mkt_quote[i, Con_FieldName_CurrentAsk] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]][j]
    mkt_quote[i, Con_FieldName_CurrentTick] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]][j]
    mkt_quote[i, Con_FieldName_LastHighestBid] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]][j]
    mkt_quote[i, Con_FieldName_LastLowestAsk] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]][j]
    mkt_quote[i, Con_Data_ColName_LastNumTicks] <- 0
    mkt_quote[i, Con_Data_ColName_LastVolume] <- 0
    mkt_quote[i, Con_Data_ColName_LastValue] <- 0
    
  }
  else if (time == time_1600){
    # 15:59 close tick,val/vol/tick 
    mkt_quote[i, Con_FieldName_Sym] <- symbol
    mkt_quote[i, Con_FieldName_CurrentBid] <- env[[datatable_name_tick]][[Con_Data_ColName_LastPrice]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_FieldName_CurrentAsk] <- env[[datatable_name_tick]][[Con_Data_ColName_LastPrice]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_FieldName_CurrentTick] <- env[[datatable_name_tick]][[Con_Data_ColName_LastPrice]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_FieldName_LastHighestBid] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_FieldName_LastLowestAsk] <- env[[datatable_name_tick]][[Con_Data_ColName_Open]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_Data_ColName_LastNumTicks] <- env[[datatable_name_tick]][[Con_Data_ColName_NumTicks]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_Data_ColName_LastVolume] <- env[[datatable_name_tick]][[Con_Data_ColName_Volume]][[Con_Data_ColName_Date]][j-1]
    mkt_quote[i, Con_Data_ColName_LastValue] <- env[[datatable_name_tick]][[Con_Data_ColName_Value]][[Con_Data_ColName_Date]][j-1]
  }
  else{
  }
  
  }
  # this function returns an updated mkt_quote table
  return (mkt_quote)

}


 #untested
update_pendingorderbook <- function (env, timestamp, symbol){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #taking in orderbook as argument and returns a list containing execution messages
  #for the purpose of this back testing order book will only contain pending limit orders
  orderbook <- env[[Con_GlobalVarName_LOB]]
  quotes <- getquotes(env, symbol, timestamp)
  
  
  symbols_on_book <- orderbook[, Con_FieldName_Sym]
  ask <- apply(symbol_on_books, 1, function (sym) {return (quotes[quotes[,Con_FieldName_Sym] = sym, Con_FieldName_LastLowestAsk])})
  bid <- apply(symbol_on_books, 1, function (sym) {return (quotes[quotes[,Con_FieldName_Sym] = sym, Con_FieldName_LastHighestBid])})
  
  ready_indices <- (orderbook[,Con_FieldName_Price] >= ask & orderbook[,Con_FieldName_Side] == Con_Side_Buy) |(orderbook[,Con_FieldName_Price] <= bid & orderbook[,Con_FieldName_Side] == Con_Side_Sell)
  ready_orders <- orderbook[ready_indices,]
  executed_price <- orderbook[ready_indices, Con_FieldName_Price]

  env[[orderbook_name]] <- orderbook[!ready_indices,]
  exec_msgs <- generate_fill_msgs(ready_orders, executed_price, timestamp)
  update_trades_pnl_tables(exec_msgs, env, timestamp)
  return (exec_msgs)
}

update_trades_pnl_tables<- function (fill_msgs, env, timestamp){
  #posTable and tradesTable are references and changes are permanent
  #takes in a list of execution messages and change the two tables, returns nothing
  #make sure every message is fill for sure
  fill_msgs <- fill_msgs[fill_msgs[,Con_FieldName_ExecStatus] == Con_ExecStatus_filled,]
  if (nrow(fill_msgs) == 0){
    return()
  }
  #update the position tables & trades table
  tradesbook_name <- Con_GlobalVarName_TradesBook
  positionbook <- env[[Con_GlobalVarName_PositionBook]]
  last_pos <- positionbook[[length(positionbook)]]
  new_pos <- last_pos
  previous_cash <- new_pos[new_pos[, Con_FieldName_Sym]== Con_Sym_Cash, Con_FieldName_Qty]
  cash_change <- 0
  for (i in 1:nrow(fill_msgs)){
    fill_sym <- fill_msgs[i, Con_FieldName_Sym]
    fill_side <- fill_msgs[i, Con_FieldName_Side]
    fill_qty <- fill_msgs[i, Con_FieldName_Qty]
    fill_price <- fill_msgs[i, Con_FieldName_AvgPrice]
    cash_change <- cash_change + (fill_side == Con_Side_Sell) * fill_qty * fill_price - (fill_side == Con_Side_Buy) * fill_qty * fill_price
    #cat("hi", length((1:nrow(new_pos))[new_pos[,Con_FieldName_Sym] == fill_sym]), "\n")
    if (length((1:nrow(new_pos))[new_pos[,Con_FieldName_Sym] == fill_sym]) == 0){
      #no positions exist for this symbol yet
      new_line_index <- nrow(new_pos) + 1
      new_pos[new_line_index, Con_FieldName_Sym] <- fill_sym
      new_pos[new_line_index, Con_FieldName_Qty] <- ((fill_side == Con_Side_Buy) * fill_qty
        - (fill_side == Con_Side_Sell) * fill_qty)
      new_pos[new_line_index, Con_FieldName_BookVal] <- new_pos[new_line_index, Con_FieldName_Qty] * fill_price
      new_pos[new_line_index, Con_FieldName_MktVal] <- new_pos[new_line_index, Con_FieldName_BookVal]
      #update the trades table
      insert_into_tradesbook(env, tradesbook_name, time = timestamp, sym = fill_sym,
                             side = fill_side, qty = fill_qty, price = fill_price,
                             openclose = Con_OpenClose_Open, pnl = NA)
    }
    else{
      #positions exist for this symbol
      index <- (1:nrow(new_pos))[new_pos[,Con_FieldName_Sym] == fill_msgs[i, Con_FieldName_Sym]][1]
      orig_quantity <- new_pos[index, Con_FieldName_Qty]
      orig_bkval <- new_pos[index, Con_FieldName_BookVal]
      new_pos[index, Con_FieldName_Qty] <- (orig_quantity + 
                  (fill_side == Con_Side_Buy) * fill_qty
                - (fill_side == Con_Side_Sell) * fill_qty)
      new_pos[index, Con_FieldName_MktVal] <- new_pos[index, Con_FieldName_Qty] * fill_price
      new_pos[index, Con_FieldName_BookVal] <- (new_pos[index, Con_FieldName_BookVal] + 
        ((fill_side == Con_Side_Buy) * fill_qty
         - (fill_side == Con_Side_Sell) * fill_qty) * fill_price)
        oc <- Con_OpenClose_Open
        pnl <- NA
        quantity <- fill_qty
        if ((orig_quantity < 0 & fill_side == Con_Side_Buy) | (orig_quantity > 0 & fill_side == Con_Side_Sell)){
          #the execution offsets a closes some positions
          if ((new_pos[index, Con_FieldName_Qty] > 0 & fill_side == Con_Side_Buy) |
              (new_pos[index, Con_FieldName_Qty] < 0 & fill_side == Con_Side_Sell)){
            #if the execution opens up an position as well
            opposite_pos_qty <- new_pos[index, Con_FieldName_Qty]
            oc = Con_OpenClose_Close
            pnl = -(orig_quantity) * (orig_bkval/orig_quantity - fill_price)
            new_pos[index, Con_FieldName_BookVal] <- new_pos[index, Con_FieldName_BookVal] + pnl
            quantity <- abs(orig_quantity)
            #cat(quantity, " ", orig_bkval, " ", orig_quantity, " ", orig_bkval/orig_quantity, " ", fill_price, " ", pnl, "\n")
            insert_into_tradesbook(env, tradesbook_name, time = timestamp, sym = fill_sym,
                                   side = fill_side, qty = opposite_pos_qty, price = fill_price,
                                   openclose = Con_OpenClose_Open, pnl = NA)
          }
          else{
            oc = Con_OpenClose_Close
            pnl = (((fill_side == Con_Side_Buy) * fill_qty - (fill_side == Con_Side_Sell) * fill_qty)
              * (orig_bkval/orig_quantity - fill_price))
            new_pos[index, Con_FieldName_BookVal] <- new_pos[index, Con_FieldName_BookVal] + pnl
            #cat(fill_qty, " ", orig_mktval, " ", orig_quantity, " ", orig_mktval/orig_quantity, " ", fill_price, " ", pnl, "\n")
          }  
        }
        insert_into_tradesbook(env, tradesbook_name, time = timestamp, sym = fill_sym,
                                 side = fill_side, qty = quantity, price = fill_price,
                                 openclose = oc, pnl = pnl)

    }
  }
  #if position is flat remove this line
  new_pos <- new_pos[!new_pos[,Con_FieldName_Qty] == 0,]
  
  new_pos[new_pos[, Con_FieldName_Sym]== Con_Sym_Cash, c(Con_FieldName_Qty, Con_FieldName_BookVal, Con_FieldName_MktVal)] <- previous_cash + cash_change
  positionbook[[length(positionbook) + 1]] <- new_pos
  names(positionbook)[length(positionbook)] <- timestamp
  env[[Con_GlobalVarName_PositionBook]] <- positionbook
}
#untested new orders, cancel and replace not implemented
handle_orders <- function (orders, symbol, env, timestamp){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #handles all orders (new, replace, cancels) and update the order book approriately
  #returns execution messages
  
  quotes <- getquotes(env, symbol, timestamp)
  
  new_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_New,]
  replace_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_Replace,]
  cancel_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_Cancel,]
  
  mkt_new <- new_orders[new_orders[,Con_FieldName_OrdType] == Con_OrdType_Mkt, ]
  mkt_order_symbols <- mkt_new[, Con_FieldName_Sym]
  mkt_ask <- apply(mkt_order_symbols, 1, function (sym) {return (quotes[quotes[,Con_FieldName_Sym] = sym, Con_FieldName_CurrentAsk])})
  mkt_bid <- apply(mkt_order_symbols, 1, function (sym) {return (quotes[quotes[,Con_FieldName_Sym] = sym, Con_FieldName_CurrentBid])})
  
  mkt_exec_prices <- (mkt_new[, Con_FieldName_Side] == Con_Side_Buy ) * mkt_ask + 
    (mkt_new[, Con_FieldName_Side] == Con_Side_Sell ) * mkt_bid
  
  
  limit_new <- new_orders[new_orders[,Con_FieldName_OrdType] == Con_OrdType_Limit, ]
  lmt_order_symbols <- limit_new[, Con_FieldName_Sym]
  lmt_ask <- apply(lmt_order_symbols, 1, function (sym) {return (quotes[quotes[,Con_FieldName_Sym] = sym, Con_FieldName_CurrentAsk])})
  lmt_bid <- apply(lmt_order_symbols, 1, function (sym) {return (quotes[quotes[,Con_FieldName_Sym] = sym, Con_FieldName_CurrentBid])})
  
  mkt_lmt_orders_indices <- (limit_new[,Con_FieldName_Price] >= lmt_ask & limit_new[,Con_FieldName_Side] == Con_Side_Buy) |(limit_new[,Con_FieldName_Price] <= lmt_bid & limit_new[,Con_FieldName_Side] == Con_Side_Sell)
  mkt_lmt_orders <- limit_new[mkt_lmt_orders_indices,]
  mkt_lmt_ask <- lmt_ask[mkt_lmt_orders_indices]
  mkt_lmt_bid <- lmt_bid[mkt_lmt_orders_indices]
  mkt_lmt_prices <- (mkt_lmt_orders[, Con_FieldName_Side] == Con_Side_Buy ) * mkt_lmt_ask + 
    (mkt_lmt_orders[, Con_FieldName_Side] == Con_Side_Sell ) * mkt_lmt_bid
  
  
  insert_into_orderbook(limit_new, env, Con_GlobalVarName_LOB)
  exec_replace <- handle_replaces(replace_orders, orderbook, timestamp)
  exec_cancel <- handle_cancels(cancel_orders, orderbook, timestamp)
  #fill must come after replace and cancel has been handled
  exec_fill <- rbind(generate_fill_msgs(mkt_new, mkt_exec_prices, timestamp), generate_fill_msgs(mkt_lmt_orders, mkt_lmt_prices, timestamp))
  update_trades_pnl_tables(exec_fill, env, timestamp)
  return(rbind(exec_replace, exec_cancel, exec_fill))
}
#tested
generate_fill_msgs <- function(ready_orders_list, exec_price, timestamp){
  fill_msgs <- data.frame(matrix(0, nrow(ready_orders_list), length(exec_msg_spec)))
  colnames(fill_msgs) <- exec_msg_spec
  fill_msgs[, Con_FieldName_OrdID] <- ready_orders_list[, Con_FieldName_OrdID]
  fill_msgs[, Con_FieldName_ExecStatus] <- rep(Con_ExecStatus_filled, nrow(ready_orders_list))
  fill_msgs[, Con_FieldName_Sym] <- ready_orders_list[, Con_FieldName_Sym]
  fill_msgs[, Con_FieldName_Qty] <- ready_orders_list[, Con_FieldName_Qty]
  fill_msgs[, Con_FieldName_AvgPrice] <- exec_price
  fill_msgs[, Con_FieldName_Side] <- ready_orders_list[, Con_FieldName_Side]
  fill_msgs[, Con_FieldName_Time] <- rep(timestamp, nrow(ready_orders_list))
  return (fill_msgs)
}
#tested
insert_into_orderbook <-function(limit_orders, env, orderbook_name){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #insert limit orders into orderbook, return nothing
  new_entries <- data.frame((matrix(0, nrow(limit_orders), length(orderbook_spec))))
  colnames(new_entries) <- orderbook_spec
  new_entries[, Con_FieldName_OrdID] <- limit_orders[, Con_FieldName_OrdID]
  new_entries[, Con_FieldName_Time] <- limit_orders[, Con_FieldName_Time]
  new_entries[, Con_FieldName_Sym] <- limit_orders[, Con_FieldName_Sym]
  new_entries[, Con_FieldName_Price] <- limit_orders[, Con_FieldName_Price]
  new_entries[, Con_FieldName_Qty] <- limit_orders[, Con_FieldName_Qty]
  new_entries[, Con_FieldName_Side] <- limit_orders[, Con_FieldName_Side]
  new_entries[, Con_FieldName_OrdType] <- limit_orders[, Con_FieldName_OrdType]
  env[[orderbook_name]] <- rbind(env[[orderbook_name]], new_entries)
}

#tested
insert_into_tradesbook <- function(env, tradesbook_name, time, sym, qty, side, price, openclose, pnl){
  new_line_index <- nrow(env[[tradesbook_name]]) + 1
  env[[tradesbook_name]][new_line_index, Con_FieldName_Time] <- time
  env[[tradesbook_name]][new_line_index, Con_FieldName_Sym] <- sym
  env[[tradesbook_name]][new_line_index, Con_FieldName_Side] <- side
  env[[tradesbook_name]][new_line_index, Con_FieldName_Qty] <- qty
  env[[tradesbook_name]][new_line_index, Con_FieldName_Price] <- price
  env[[tradesbook_name]][new_line_index, Con_FieldName_OpenClose] <- openclose
  env[[tradesbook_name]][new_line_index, Con_FieldName_Pnl] <- pnl
  
}
handle_cancels <- function(cancelorders, orderbook, timestamp){
  #returns execution messages
  #cat(cancelorders)
}

handle_replaces <- function(replaceorders, orderbook, timestamp){
  #returns execution messages
}

    # ################################################ 