
# Simple Moving Average

#current_time  <- 25
#market_close  <- head(AC_tick$LAST_PRICE,100)
#duration      <- 10


SMA<- function(current_time, duration,market_close){
  
  if(current_time < duration){
    
    warning('Not Enough Data')
    return(-1)
    
  }
  
  else{
  
  close <- market_close[seq(current_time-duration + 1, current_time,1)]
  simple_moving_average = mean(close,trim = 0, na.rm = TRUE)
  
  return(simple_moving_average)                  
  
  }
}


