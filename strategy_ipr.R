source('backtest_lib.r')

#####################   Snow      ##########################
strategy_ipr <- function(starttime,endtime, symbol, env, bid, ask, mktprice, positionbook, pendingbook){
  # Active portion of strategy
  actiontime <- c(starttime + 60 * (0: 420), endtime) # Times to perform active portion
  
  actcounter <- 1 # count for which action time we are on
  
  # Order number initialize
  ordercounter <- 1
  
  looprow <- 60 * (endtime-starttime) # Assuming starttime and endtime are times in seconds
  symbol <- c("AC", "BNS", "BMO")
  
  #order format: msgtype, symbol, price, amount, b/s, market/limit/stop, refnumber, time
  #response format: orderID, DateCreated, DateExecuted, Symbol, Price, amount, b/s, market/limit/stop
  
  for (i in 1:looprow){
    #check market condition
    
    response <- update_orderbook(env[[bid]][["HIGH"]][i], env[[ask]][["LOW"]][i], 
                                 env, pendingbook, 
                                 env[[mktprice]][["Date"]][i])
    
    # Passive portion
    if (nrow(response)!=0){
      passiveupdate(response)
    }
    #active portion of strategy
    if (env[[mktprice]][["Date"]][i] == actiontime[actcounter]){ 
      actcounter <- actcounter +1
      orderline = data.frame(matrix(NA, 0, length(order_msg_spec)))
      colnames(orderline) <- order_msg_spec
      
      # assume all data points are present
      
      # for each stock calculate ipr
      for (j in 1:length(symbol)){
        # find current positions
        currposition <- env[[positionbook]][[length(env[[positionbook]])]]
        corsymbol <- (env[[positionbook]][[length(env[[positionbook]])]][[Con_FieldName_Sym]] == symbol[j])
        currposition[corsymbol,Con_FieldName_Qty]
        # calculate ipr
        
      }
      
      # rank stocks based on ipr
      
      
      # buy stocks based on ranking

#         entry <- nrow(orderline)+1
#         orderline[entry, Con_FieldName_MsgType] <- Con_MsgType_New
#         orderline[entry, Con_FieldName_OrdID] <- ordercounter
#         ordercounter <- ordercounter + 1
#         orderline[entry, Con_FieldName_Sym] <- symbol
#         orderline[entry, Con_FieldName_Price] <- env[[ask]][["LOW"]][i]
#         orderline[entry, Con_FieldName_Qty] <- quantity
#         orderline[entry, Con_FieldName_Side] <- Con_Side_Sell
#         orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt

      }
      #orderline<- orderline[,-1]
      response <- handle_orders(orderline, env, pendingbook, env[[bid]][["HIGH"]][i], env[[ask]][["LOW"]][i], env[[mktprice]][["Date"]][i])
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
