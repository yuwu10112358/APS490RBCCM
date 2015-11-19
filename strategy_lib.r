source('backtest_lib.r')

#####################   Snow      ##########################
strategy_naive <- function(starttime,endtime, symbol){
  # Active portion of strategy
  actiontime <- c(starttime, starttime+60, starttime+120) # Times to perform active portion
  actcounter <- 1 # count for which action time we are on
  
  # Constants
  quantity <- 1
  highprice <- 110
  midprice <- 105
  lowprice <- 100
  interval <- 0.1
  
  # Order number initialize
  ordercounter <- 1
  
  looprow <- endtime-starttime+1 # Assuming starttime and endtime are times in seconds
  
  #order format: msgtype, symbol, price, amount, b/s, market/limit/stop, refnumber, time
  #response format: orderID, DateCreated, DateExecuted, Symbol, Price, amount, b/s, market/limit/stop
  
  for (i in 1:looprow){
    #check market condition
    response <- update_orderbook(global_tables$bid_price$HIGH[i], global_tables$ask_price$LOW[i], global_tables$orderbook, global_tables$market_price$Date[i])
    
    # Passive portion
    if (nrow(response)!=0){
      passiveupdate(response)
    }
    
    #active portion of strategy
    if (global_tables$market_price$Date[i] == actiontime[actcounter]){ 
      actcounter <- actcounter +1
      orderline = data.frame(matrix())
      if(global_tables$bid_price$HIGH[i] >= highprice & global_tables$positionbook$Con_FieldName_Qty == 0){
        entry <- nrow(orderline+1)
        orderline[entry, Con_FieldName_MsgType] <- Con_MsgType_New
        orderline[entry, Con_FieldName_OrdID] <- ordercounter
        ordercounter <- ordercounter + 1
        orderline[entry, Con_FieldName_Sym] <- symbol
        orderline[entry, Con_FieldName_Price] <- global_tables$bid_price$HIGH[i]
        orderline[entry, Con_FieldName_Qty] <- quantity
        orderline[entry, Con_FieldName_Side] <- Con_Side_Buy
        orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt
      }
      else if(global_tables$ask_price$LOW[i] <= lowprice & global_tables$positionbook$Con_FieldName_Qty == 0){
        entry <- nrow(orderline+1)
        orderline[entry, Con_FieldName_MsgType] <- Con_MsgType_New
        orderline[entry, Con_FieldName_OrdID] <- ordercounter
        ordercounter <- ordercounter + 1
        orderline[entry, Con_FieldName_Sym] <- symbol
        orderline[entry, Con_FieldName_Price] <- global_tables$ask_price$LOW[i]
        orderline[entry, Con_FieldName_Qty] <- quantity
        orderline[entry, Con_FieldName_Side] <- Con_Side_Sell
        orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt
      }
      else if(global_tables$market_price$HIGH[i] <= midprice-interval & global_tables$market_price$HIGH[i] >= midprice+interval){
        if(global_tables$positionbook$Con_FieldName_Qty > 0) {
          entry <- nrow(orderline+1)
          orderline[entry, Con_FieldName_MsgType] <- Con_MsgType_New
          orderline[entry, Con_FieldName_OrdID] <- ordercounter
          ordercounter <- ordercounter + 1
          orderline[entry, Con_FieldName_Sym] <- symbol
          orderline[entry, Con_FieldName_Price] <- global_tables$bid_price$HIGH
          orderline[entry, Con_FieldName_Qty] <- quantity
          orderline[entry, Con_FieldName_Side] <- Con_Side_Sell
          orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt
        }
        else if(global_tables$positionbook$Con_FieldName_Qty < 0) {
          entry <- nrow(orderline+1)
          orderline[entry, Con_FieldName_MsgType] <- Con_MsgType_New
          orderline[entry, Con_FieldName_OrdID] <- ordercounter
          ordercounter <- ordercounter + 1
          orderline[entry, Con_FieldName_Sym] <- symbol
          orderline[entry, Con_FieldName_Price] <- global_tables$ask_price$LOW[i]
          orderline[entry, Con_FieldName_Qty] <- quantity
          orderline[entry, Con_FieldName_Side] <- Con_Side_Buy
          orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt
        }
      }
      orderline<- orderline[,-1]
      #response <- handle_orders(orderline, global_tables$orderbook, ask$LOW[i], tick$Date[i])
      if (nrow(response)!=0){
        passiveupdate(response)
      }
    }
  }
}

passiveupdate <- function(response){
  # Send subsequent orders
  
  #if (some condition to send cancel orders){
  #  send cancel orders
  #}
}
##################################################
