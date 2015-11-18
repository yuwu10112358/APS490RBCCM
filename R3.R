
#Test pending table
orderID     <-c(1,2,3)
timeDate    <-c("10/05/2015","10/05/2015","10/05/2015")
stockSymbol <-c("AAPL","AAPL","AAPL")
price       <-c(100,97,105)
numOfStock  <-c(50,50,50)
typeOfOrder <-c(1,-1,-1)
orderType   <-c("Market","Stop","Limit")


PendingTable <- data.frame(orderID,timeDate,stockSymbol,price,numOfStock,typeOfOrder,orderType)
tradingTable <- data.frame( "stockSymbol" = character(),"typeOfOrder" = integer(),"NumberOfStock" = integer(),"Date"=as.Date(character()), "Price" = numeric(),"Open/close" = character(), stringsAsFactors=FALSE)
positionTable<- data.frame( "Date"=as.Date(character()),"Asset" = character(),"MarketValue" = numeric(),"InitialValue" = numeric(),"NumberOfStock" = integer(),"NetValue" = numeric(),stringsAsFactors=FALSE)
positionTable[1,]<-c("10/05/2015","Cash",100000.00,100000.00,1,100000.00) 


#Go to pending table and check if there is any order to be executed

N = nrow(PendingTable)


for(i in 1:N){

  #execute Market orders
  if (!is.na(PendingTable$orderType[i])&& (PendingTable$orderType[i]=="Market"||PendingTable$orderType[i]=="Stop")){
    
    temp <- PendingTable[c(i),]
    
    #Remove market orders from pending table
    PendingTable <- PendingTable[-c(i),]
    
    #update trading table
    tradingTable[nrow(tradingTable) + 1, ] <- c( temp[3],temp[6],temp[5],temp[2],temp[4],"open")
    
    #update position table
    #positionTable[nrow(positionTable) + 1, ] <- c( temp[2],temp[3],temp[5],temp[2],temp[4],"open")
    
    temp <-c()
    
  } 
}







 