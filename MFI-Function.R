
#Money Flow Index

current_time  <-25
market_high   <- head(AC_tick$HIGH,100)
market_low    <- head(AC_tick$LOW,100)
market_close  <- head(AC_tick$LAST_PRICE,100)
market_volume <- head(AC_tick$VOLUME,100)
duration      <- 14


MFI <- function(current_time, market_high, market_low, market_close, market_volume, duration){
  
if(current_time > duration){
  
  
  high   <- market_high[seq(current_time-duration, current_time,1)]
  low    <- market_low[seq(current_time-duration, current_time,1)]
  close  <- market_close[seq(current_time-duration, current_time,1)]
  volume <- market_volume[seq(current_time-duration, current_time,1)] 
  
  
typical_price = (high + low + close)/3
N = length(typical_price)
raw_money_flow = typical_price * volume


positive_flow <-vector() 
negative_flow <-vector()

positive_flow[1] = 0
negative_flow[1] = 0


for(i in 2:N){
  
  if(typical_price[i] == typical_price[i-1]){
    
    positive_flow[i] = 0
    negative_flow[i] = 0
  }
  
  
else{
  
  if(typical_price[i] > typical_price[i-1]){
    
    positive_flow[i] = raw_money_flow[i]
    negative_flow[i] = 0
    
  }
  
  else{  
    positive_flow[i] = 0
    negative_flow[i] = raw_money_flow[i]
    }
}
  
}

  
cumulative_positive_flow = sum(positive_flow, NA, na.rm = TRUE)
cumulative_negative_flow = sum(negative_flow,NA, na.rm = TRUE)
money_flow_ratio = cumulative_positive_flow/cumulative_negative_flow
money_flow_index = 100 - 100/(1 + money_flow_ratio)


  
  return(money_flow_index)

}
  

  else{
  
  warning(' Not Enough Data')
  return(-1)
  
}
}




