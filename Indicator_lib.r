## Indicator Library

EMA <- function(starttime, endtime, duration, symbol, env,datatype){
  if (starttime == endtime){
    return(SMA(starttime, duration, symbol, env, datatype)) #### Change when SMA function is written
  }
  else{
    timeweight = 2/(duration +1)
    EMAval = (env[[datatype]][["CLOSE"]][currtime] - 
                EMA(starttime - 1, endtime, duration, symbol, env,datatype))*timeweight +
                EMA(starttime - 1, endtime, duration, symbol, env,datatype)
    return (EMAval)
  }
}

MACD <- function(starttime, endtime1, endtime2, endtime3, duration1, duration2, duration3, symbol, env,datatype){
  MACDval = EMA(starttime, endtime1, duration1, symbol, env, datatype) -
            EMA(starttime, endtime2, duration2, symbol, env, datatype)
  signalval = EMA(starttime, endtime3, duration3, symbol, env, datatype)
  return (MACDval - signalval)
}