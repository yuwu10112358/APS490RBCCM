### TEST###

#backtester <- function(stock_price,market_price,amount_invested,startdate,enddate){
#} 


input <- function(){
  ourdata <- read.csv("/Users/jewelho/Desktop/Capstone/Code/capstone1.csv")
  return(ourdata)
}


output

# buy = 1; hold = 0; sell = -1
strategy <- function(stock_price,market_price,amount_invested,startdate,enddate){
  for(i in 1:length(stock_price[1,])){
    if(stock_price[2,i] >= 110){
      snow = c(stock_price[1,i],stock_price[2,i],-1)
        # sell array (time, price, action)
    }
    elseif(stock_price[2,i] <= 100){
      snow = c(stock_price[1,i],stock_price[2,i],1)
    }
   # elseif(stock_price[2,i] = 105)
    #    if(snow[3] == -1) 
     # snow = c(stock_price[1,i],stock_price[2,i],0)
    }
}

