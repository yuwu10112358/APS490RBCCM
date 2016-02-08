# 14-Period Ease of Movement

current_time  <-20
market_high   <- head(AC_tick$HIGH,100)
market_low    <- head(AC_tick$LOW,100)
market_volume <- head(AC_tick$VOLUME,100)
duration      <- 14


EMV <- function( current_time, market_high, market_low,market_volume, duration){
  
  
  if(current_time > duration){
    
    
     high   <- market_high[seq(current_time-duration, current_time,1)]
     low    <- market_low[seq(current_time-duration, current_time,1)]
     volume <- market_volume[seq(current_time-duration, current_time,1)] 

  
     distance_moved   <- vector()
     box_ratio        <- vector()
     ease_of_movement <- vector()
     
     distance_moved[1]   = 0
     box_ratio[1]        = 0
     
     N = length(high)
  
  
  
  for(i in 2:N){
    
    distance_moved[i]    = ((high[i] + low[i])/2 - (high[i-1] + low[i-1])/2)
    box_ratio[i]         = ((volume[i]/10000000)/(high[i]-low[i]))

    
  }
    
     ease_of_movement = distance_moved/box_ratio
     ease_of_movement[1] = 0
     SMA_EMV = sum(ease_of_movement, NA, na.rm = TRUE)/duration
     

    
    return(SMA_EMV)
    
  }
  
  
  else{
    
    warning(' Not Enough Data')
    return(-1)
    
  }
}

value <- EMV( current_time, market_high, market_low,market_volume, duration)
  