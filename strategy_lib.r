-
  -#####################   Snow      ##########################
strategy_naive <- function(starttime,endtime, symbol/sector){
  -  #this is what we are changing each iteration
for everyminute between starttime and endtime{
      -    #check market condition
        update_orderbook()
      -    
        if (time for strategy to do something){
          if(stock_price[2,i] >= 110){
            snow = c(stock_price[1,i],stock_price[2,i],-1)
            -      # sell array (time, price, action)
            }
          elseif(stock_price[2,i] <= 100){
            snow = c(stock_price[1,i],stock_price[2,i],1)
            }
          -    # elseif(stock_price[2,i] = 105)
            -    #    if(snow[3] == -1) 
            -    # snow = c(stock_price[1,i],stock_price[2,i],0)
            -    #order: msgtype, orderID, datetime, price, amount, b/s, market/limit/stop
            execute_orders(order)
          }
      }
}

passive_handling <- function(list of exe){
  #determine what does the strategy do with filled limit orders, cancelled orders, etc
}
-##################################################3