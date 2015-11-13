### TEST###

#backtester <- function(stock_price,market_price,amount_invested,startdate,enddate){
#} 

#
############### Jewel ############################
# Nov 12 notes: 1) 1 function is sufficient 2) time series data clarifiaction
data_extraction <- function()
{
 #Requirements:
 #The excel sheet contains 3 tables arranged in order: Tick, Ask, Bid price.
 #Number of columns in each table can vary
 #StartRow = 3
  
  install.packages("XLConnect")
  library(XLConnect)
  file <- readWorksheetFromFile("/APS490RBCCM/Intraday_Test_Data.xlsx", 
                        sheet=1, 
                        startRow = 3,
                        check.names = FALSE
                        )

  mylist = c()
  mylist[1] = 1
  for(i in 1:length(file)){
    if(is.na(file[,i])) {
      mylist[length(mylist)+1] = i
    }}
  
tick = file[, mylist[1]:   (mylist[2] - 1)] 
bid  = file[,(mylist[2]+1):(mylist[3] - 1)]
ask  = file[,(mylist[3]+1):length(file)]
}


inputData <- function(){
  #reads csv
  ourdata <- read.csv("/Users/jewelho/Desktop/Capstone/Code/capstone1.csv")
  #format date into time series
  return(ourdata)
}
##################################################

################# Gordon ######################3

output<-function(tradematrix, pnl matrix){
  #input benchmark performance
  1.       Complete history of cumulative PnL of each stocks and the entire portfolio
  2.       Number of trades in total. Average number of trades per day. Distribution of number of trades per day
  3.       Average PnL per day. Distribution of PnL per day.
  4.       % days profitable
  5.       Average PnL per trade by stock and across stocks. Distribution of PnL per trade. (compute for both $ per share and $ per $ invested)
  6.       % trades profitable
  7.       Max drawdown and max drawdown period
  8.       Annualized return
  9.       Annualized std dev
  10.   Sharpe ratio (assume benchmark is the market)
  11.   Correlation of cumulative return with market return
  #outputs performance
  #outputs pdf, pretty stuff
}
#############################################

################### Yu & Paria ################
# buy = 1; hold = 0; sell = -1
execute_orders <- function(orders){
  
  if order = market order
    #do something
    #update tradematrix and pnl matrix
  else if order = limit orders
    #put in lob or some shit
  else if order = stop orders
    #same as above
    
}

update_orderbook <- function(){
  #update lob and pnl and stuff
}
###############################################

