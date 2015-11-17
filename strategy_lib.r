
#####################   Snow      ##########################
strategy_naive <- function(starttime,endtime, symbol){
  # Active portion of strategy
  actiontime <- cbind(9,10,11) # Times to perform active portion
  actcounter <- 0 # count for which action time we are on
  orderbook <- matrix(data=NA,nrow=1,ncol=7)
  colnames(orderbook) <- c("msgtype", "symbol", "price", "amount", "b/s", "market/limit/stop", "refnumber")
  
  col <- (starttime - endtime)/60 # Assuming starttime and endtime are times in seconds
  
  #order format: msgtype, symbol, price, amount, b/s, market/limit/stop, refnumber
  #response format: orderID, DateCreated, DateExecuted, Symbol, Price, amount, b/s, market/limit/stop
  
  for (i in 1:col){
    #check market condition
    response <- update_orderbook()
    
    # Passive portion
    if (invalid(response)){
      passiveupdate(response)
    }
    
    #active portion of strategy
    if (time = actiontime[actcounter]){ 
      if(stock_price[2,i] >= 110){
        orderline = cbind("new","AAPL",ourdata[i,2],1, 1,"market",NA)
      }
      else if(stock_price[2,i] <= 100){
        orderline = cbind("new","AAPL",ourdata[i,2],1, -1,"market",NA)
      }
      else if(stock_price[2,i] <= 104.9 & stock_price[2,i] >= 105.1){
        if(positionmatrix$number > 0) {
          orderline = cbind("new","AAPL",ourdata[i,2],1, -1,"market",NA)
        }
        else if(positionmatrix$number < 0) {
          orderline = cbind("new","AAPL",ourdata[i,2],1, 1,"market",NA)
        }
      }
      orderbook <- orderbook[-1,] #Remove NA row of orderbook
      response <- execute_orders(orderbook)
      if (invalid(response)){
        passiveupdate(response)
      }
    }
  }

}

passiveupdate <- function(response){
  # Update Tables and send subsequent further orders
  # response format: orderID, DateCreated, DateExecuted, Symbol, Price, amount, b/s, market/limit/stop
  # Update trading matrix
  currpositions = cbind(response[1,4],0)
  for (i in 1:length(response)){
    for (j in 1:length(tradingmatrix)){ #Find same symbol to find position in that stock
      if (response[i,4] = tradingmatrix[length(tradingmatrix)-j,3]){
        pnl = (response[i,5]-tradingmatrix[length(tradingmatrix)-j,5])/tradingmatrix[length(tradingmatrix)-j,5]
        if (tradingmatrix[length(tradingmatrix)-j,6]>0 & response[i,7]=-1){
          tradetype = "close"
          break
        }
        else if (tradingmatrix[length(tradingmatrix)-j,6]<0 & response[i,7]=1) {
          tradetype = "close"
          break
        }
        else {
          tradetype = "open"
          pnl = NA
          break
        }
          # Add errorcheck if trade is larger than position
      }
    }
    tradingcol <- cbind(response[i,4], response[i,7], response[i,6], response[i,3], response[i,5], tradetype ,pnl)
    tradingmatrix <- rbind(tradingmatrix, tradingcol)
  }
  
  #if (some condition to send cancel orders){
  #  send cancel orders
  #}
}
##################################################
