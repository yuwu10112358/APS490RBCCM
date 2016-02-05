
##Money Flow Index
MFI <- function(currentTime, env, High, Low, Close, Volume){
  
  TypicalPrice[currentTime] = (High + Low + Close)/3
  RawMoneyFlow[currentTime] <-TypicalPrice[currentTime]*Volume[currentTime]
  
  if (TypicalPrice[currentTime] > TypicalPrice[currentTime-1]){
    
    PositiveMF[currentTime]= RawMoneyFlow[currentTime]
    NegativeMF[currentTime]= 0
  }
  
  else{
    
    NegativeMF[currentTime]= RawMoneyFlow[currentTime]
    PositiveMF[currentTime]= 0
    
  }
  
  cNMF[currentTime]= CNMF[currentTime-1]+NegativeMF[currentTime]-NegativeMF[currentTime-14]
  cPMF[currentTime]= CPMF[currentTime-1]+PositiveMF[currentTime]-PositiveMF[currentTime-14]
  
  MoneyFlowRatio[currentTime] = CPMF[currentTime]/cNMF[currentTime]
  MoneyFlowIndex = 100 - 100/(1 + MoneyFlowRatio[currentTime])
  
  return(MoneyFlowIndex)
}



## Simple Moving Average

SMA<- function(currentTime, duration, symbol, env, datatype,Close){
  
   SimpleMovingAverage[currentTime] = SMA(currentTime - 1, duration, symbol, env, datatype,Close)+
                                      (env[[datatype]][["CLOSE"]][currtime])-
                                      (env[[datatype]][["CLOSE"]][currtime - duration])
                                      
   return(SimpleMovingAverage)                  
  
}

## TEST

##TypicalPrice  <-c(24.63,24.69,24.99,25.36,25.19,25.17,25.01,24.96,25.08,25.25,25.21,25.37,25.61,25.58,25.46,25.33,25.09,25.03,24.91,24.89,25.13,24.64)
##Volume        <-c(18730,12272,24691,18358,22964,15919,16067,16568,16019,9774,22573,12987,10907,5799,7395,5818,7165,5673,5625,5023,7457,11798)
##RawMoneyFlow  <-c()
##NegativeRMF   <-c()
##PositiveRMF   <-c()
##Table         <-data.frame(TypicalPrice,Volume)

##N = nrow(table)


##for (i in 1:N){
  
##  RawMoneyFlow[i+1]<-TypicalPrice[i+1]*Volume[i+1]

##    if (TypicalPrice[i+1]>TypicalPrice[i]){
    
##      PositiveRMF[i+1]= RawMoneyFlow[i+1]
##      NegativeRMF[i+1]= 0
##    }
      
##      else{
        
##       NegativeRMF[i+1]= RawMoneyFlow[i+1]
##       PositiveRMF[i+1]= 0
    
##  }
  
##}


##Matrix <- data.frame(RawMoneyFlow, PositiveRMF,NegativeRMF)

##k <- 1

##for (j in 15:N){
##  PMF <- sum(Matrix$PositiveRMF[j-k])
##  NMF <- sum(Matrix$NegativeRMF[j-k])
  
##  k <- k + 1
  
##}


ask_price <- global_tables$ask_price
bid_price <- global_tables$bid_price
ask_price[1:10, "HIGH"]
something<-(global_tables$ask_price)[1:3, "HIGH"]

