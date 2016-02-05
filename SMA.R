
# Simple Moving Average
#library? Error: could not find function

current_time  <- 10
market_close  <- (global_tables$market_price)["LAST_PRICE"]
duration      <- 10
value         <- SMA(current_time, duration,market_close)


SMA<- function(current_time, duration,market_close){
  
  if(current_time < duration){
    
    warning('Not Enough Data')
    return(-1)
    
  }
  
  else{
  
  close <- market_close[seq(current_time-duration + 1, current_time,1),"LAST_PRICE"]
  simple_moving_average = mean(close,trim = 0, na.rm = TRUE)
  
  return(simple_moving_average)                  
  
  }
}


