-### TEST###
  -
  -#backtester <- function(stock_price,market_price,amount_invested,startdate,enddate){
  -#} 
  -
  -#
  -############### Jewel ############################
-# Nov 12 notes: 1) 1 function is sufficient 2) time series data clarifiaction
source('constants.r')
data_extraction <- function()
{
      - #Requirements:
        - #The excel sheet contains 3 tables arranged in order: Tick, Ask, Bid price.
        - #Number of columns in each table can vary
        -  
    library(XLConnect)
    file <- readWorksheetFromFile("Intraday_Test_Data.xlsx", 
                                    sheet=1, 
                                    startRow = 3,
                                    check.names = FALSE
                                    )
    mylist = c()
    mylist[1] = 1
    for(i in 1:length(file)){
          if(is.na(file[,i])) {
            mylist[length(mylist)+1] = i
            }}
    tick = file[, mylist[1]:   (mylist[2] - 1)] 
    bid  = file[,(mylist[2]+1):(mylist[3] - 1)]
    ask  = file[,(mylist[3]+1):length(file)]
}
  -
    -# 
    -# inputData <- function(){
    -#   #reads csv
    -#   ourdata <- read.csv("/Users/jewelho/Desktop/Capstone/Code/capstone1.csv")
    -#   #format date into time series
    -#   return(ourdata)
    -# }
    -# ##################################################
  -# 
    -# ################# Gordon ######################3
    -# 
    -# output<-function(tradematrix, pnl matrix){
    -#   #input benchmark performance
    -#   1.       Complete history of cumulative PnL of each stocks and the entire portfolio
    -#   2.       Number of trades in total. Average number of trades per day. Distribution of number of trades per day
    -#   3.       Average PnL per day. Distribution of PnL per day.
    -#   4.       % days profitable
    -#   5.       Average PnL per trade by stock and across stocks. Distribution of PnL per trade. (compute for both $ per share and $ per $ invested)
    -#   6.       % trades profitable
    -#   7.       Max drawdown and max drawdown period
    -#   8.       Annualized return
    -#   9.       Annualized std dev
    -#   10.   Sharpe ratio (assume benchmark is the market)
    -#   11.   Correlation of cumulative return with market return
    -#   #outputs performance
    -#   #outputs pdf, pretty stuff
    -# }
    -# #############################################
# ################### Yu & Paria ################
  -# # buy = 1; hold = 0; sell = -1
update_orderbook <- function (marketprice, orderbook, timestamp){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #taking in orderbook as argument and returns a list containing execution messages
  #for the purpose of this back testing order book will conly contain pending limit orders
  ready_indices = (orderbook[,Con_FieldName_Price] >= marketprice && orderbook[,Con_FieldName_Side] == Con_Side_Buy) || (orderbook[,Con_FieldName_Price] <= marketprice && orderbook[,Con_FieldName_Side] == Con_Side_Sell)
  ready_orders = orderbook[ready_indices,]
  orderbook = orderbook[!ready_indices,]
  return (generate_fill_msgs(ready_orders, marketprice, timestamp))
}

update_trades_pnl_tables<- function (fill_msgs, posTable, tradesTable){
  #posTable and tradesTable are references and changes are permanent
  #takes in a list of execution messages and change the two tables, returns nothing
  #make sure every message is fill for sure
  fill_msgs <- fill_msgs[fill_msgs[,Con_FieldName_ExecStatus] == Con_ExecStatus_filled,]
  #update the position tables
  
}

handle_orders <- function (orders, orderbook, marketprice, timestamp){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #handles all orders (new, replace, cancels) and update the order book approriately
  #returns execution messages
  new_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_New,]
  replace_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_Replace,]
  cancel_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_Cancel,]
  
  mkt_new <- new_orders[new_orders[,Con_FieldName_OrdType] == Con_OrdType_Mkt, ]
  limit_new <- new_orders[new_orders[,Con_FieldName_OrdType] == Con_OrdType_Limit, ]
  
  insert_into_orderbook(limit_new, orderbook)
  exec_replace <- handle_replaces(replace_orders, orderbook, timestamp)
  exec_cancel <- handle_cancels(cancel_orders, orderbook, timestamp)
  #fill must come after replace and cancel has been handled
  exec_fill <- rbind(generate_fill_msgs(mkt_new, marketprice, timestamp), update_orderbook(marketprice, orderbook, timestamp))
  
  return(rbind(exec_replace, exec_cancel, exec_fill))
}

generate_fill_msgs <- function(ready_orders_list, marketprice, timestamp){
  fill_msgs <- data.frame(matrix(0, nrow(ready_orders_list), length(exec_msg_spec)))
  colnames(fill_msgs) <- exec_msg_spec
  fill_msgs[, Con_FieldName_OrdID] <- ready_orders_list[, Con_FieldName_OrdID]
  fill_msgs[, Con_FieldName_ExecStatus] <- Con_ExecStatus_filled
  fill_msgs[, Con_FieldName_Sym] <- ready_orders_list[, Con_FieldName_Sym]
  fill_msgs[, Con_FieldName_Qty] <- ready_orders_list[, Con_FieldName_Qty]
  fill_msgs[, Con_FieldName_AvgPrice] <- marketprice
  fill_msgs[, Con_FieldName_Side] <- ready_orders_list[, Con_FieldName_Side]
  fill_msgs[, Con_FieldName_Time] <- timestamp
  return (fill_msgs)
}

insert_into_orderbook <-function(limit_orders, orderbook){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #insert limit orders into orderbook, return nothing
  new_entries <- data.frame((matrix(0, nrow(limit_orders), length(orderbook_spec))))
  new_entry[, Con_FieldName_OrdID] <- limit_orders[, Con_FieldName_OrdID]
  new_entry[, Con_FieldName_Time] <- limit_orders[, Con_FieldName_Time]
  new_entry[, Con_FieldName_Sym] <- limit_orders[, Con_FieldName_Sym]
  new_entry[, Con_FieldName_Price] <- limit_orders[, Con_FieldName_Price]
  new_entry[, Con_FieldName_Qty] <- limit_orders[, Con_FieldName_Qty]
  new_entry[, Con_FieldName_Side] <- limit_orders[, Con_FieldName_Side]
  new_entry[, Con_FieldName_OrdType] <- limit_orders[, Con_FieldName_OrdType]
  rbind(orderbook, new_entries)
}

handle_cancels <- function(cancelorders, orderbook){
  #returns execution messages
}

handle_replaces <- function(replaceorders, orderbook){
  #returns execution messages
}

    # ################################################ 