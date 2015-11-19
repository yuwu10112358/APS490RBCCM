
#####################   Snow      ##########################
strategy_naive <- function(starttime,endtime, symbol){
  # Active portion of strategy
  actiontime <- c(9,10,11) # Times to perform active portion
  actcounter <- 0 # count for which action time we are on
  
  # Constants
  quantity <- 1
  highprice <- 110
  midprice <- 105
  lowprice <- 100
  interval <- 0.1
  
  # Order number initialize
  Ordercounter <- 1
  
  looprow <- (starttime - endtime)/60 # Assuming starttime and endtime are times in seconds
  
  #order format: msgtype, symbol, price, amount, b/s, market/limit/stop, refnumber, time
  #response format: orderID, DateCreated, DateExecuted, Symbol, Price, amount, b/s, market/limit/stop
  
  for (i in 1:looprow){
    #check market condition
    response <- update_orderbook()
    
    # Passive portion
    if (is.empty(response)){
      passiveupdate(response)
    }
    
    #active portion of strategy
    if (bid$Date[i] == actiontime[actcounter]){ 
      orderline = data.frame(matrix(0, nrow=1, length(order_msg_spec)))
      if(bid$HIGH[i] >= highprice){
        orderline[nrows(orderline)+1, Con_FieldName_MsgType] <- Con_MsgType_New
        orderline[nrows(orderline)+1, Con_FieldName_OrdID] <- ordercounter
        ordercounter <- ordercounter + 1
        orderline[nrows(orderline)+1, Con_FieldName_Sym] <- Symbol
        orderline[nrows(orderline)+1, Con_FieldName_Price] <- bid$HIGH
        orderline[nrows(orderline)+1, Con_FieldName_Qty] <- quantity
        orderline[nrows(orderline)+1, Con_FieldName_Side] <- Con_Side_Buy
        orderline[nrows(orderline)+1, Con_FieldName_OrdType] <- Con_OrdType_Mkt
      }
      else if(stock_price[Con_PriceCol,i] <= lowprice){
        orderline[nrows(orderline)+1, Con_FieldName_MsgType] <- Con_MsgType_New
        orderline[nrows(orderline)+1, Con_FieldName_OrdID] <- ordercounter
        ordercounter <- ordercounter + 1
        orderline[nrows(orderline)+1, Con_FieldName_Sym] <- Symbol
        orderline[nrows(orderline)+1, Con_FieldName_Price] <- ask$LOW
        orderline[nrows(orderline)+1, Con_FieldName_Qty] <- quantity
        orderline[nrows(orderline)+1, Con_FieldName_Side] <- Con_Side_Sell
        orderline[nrows(orderline)+1, Con_FieldName_OrdType] <- Con_OrdType_Mkt
      }
      else if(stock_price[Con_PriceCol,i] <= midprice-interval & stock_price[Con_PriceCol,i] >= midprice+interval){
        if(positionmatrix$number > 0) {
          orderline[nrows(orderline)+1, Con_FieldName_MsgType] <- Con_MsgType_New
          orderline[nrows(orderline)+1, Con_FieldName_OrdID] <- ordercounter
          ordercounter <- ordercounter + 1
          orderline[nrows(orderline)+1, Con_FieldName_Sym] <- Symbol
          orderline[nrows(orderline)+1, Con_FieldName_Price] <- bid$HIGH
          orderline[nrows(orderline)+1, Con_FieldName_Qty] <- quantity
          orderline[nrows(orderline)+1, Con_FieldName_Side] <- Con_Side_Sell
          orderline[nrows(orderline)+1, Con_FieldName_OrdType] <- Con_OrdType_Mkt
        }
        else if(positionmatrix$number < 0) {
          orderline[nrows(orderline)+1, Con_FieldName_MsgType] <- Con_MsgType_New
          orderline[nrows(orderline)+1, Con_FieldName_OrdID] <- ordercounter
          ordercounter <- ordercounter + 1
          orderline[nrows(orderline)+1, Con_FieldName_Sym] <- Symbol
          orderline[nrows(orderline)+1, Con_FieldName_Price] <- ask$LOW
          orderline[nrows(orderline)+1, Con_FieldName_Qty] <- quantity
          orderline[nrows(orderline)+1, Con_FieldName_Side] <- Con_Side_Buy
          orderline[nrows(orderline)+1, Con_FieldName_OrdType] <- Con_OrdType_Mkt
        }
      }
      response <- handle_orders(orderbook)
      if (is.empty(response)){
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
