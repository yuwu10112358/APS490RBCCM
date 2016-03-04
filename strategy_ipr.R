# source('backtest_lib.r')
source(SMA.R)

return_and_stdev <- function(prices){
  for (i in 2:NROW(prices)){
    prices[i, "Return"] <- log10(prices[i,"Price"]/prices[i-1,"Price"])
  }
  final_return <- mean(na.omit(prices[, "Return"]))
  for (i in 2:NROW(prices)){
    prices[i, "StDev"] <- (prices[i, "Return"]-final_return)^2
  }
  final_stdev <- sum(na.omit(prices[, "StDev"]))/(NROW(prices)-1)
  return(list(ret=final_return, stdev=final_stdev))
}

#####################   Snow      ##########################
strategy_impliedpricerisk <- function(Stocks, env){
  # Active portion of strategy
  IPR_df <- data.frame(Date = as.character(), Symbol = as.character(), IPR = as.integer())
  #Stocks <- c("AC", "BNS", "BMO")
  EquityList <- c("tick", "ask", "bid")
  jump <- 13
  endtime <- 391
  # if SMA tool = 1, then short sma > long sma and we buy, and vice versa
  smatool <- data.frame(Symbol = as.character(), sma = as.integer())
  stock_data <- paste(Stocks[1],EquityList[1],sep="_")
  totaltime <- env[[stock_data]][["Date"]][1:endtime]
  actiontime <- totaltime[seq(1,endtime,30)] # Times to perform active portion
  actiontime <- actiontime[-c(which(strftime(actiontime, format="%H:%M:%S") == "16:00:00",arr.ind = TRUE))]
  actiontime <- actiontime[-c(which(strftime(actiontime, format="%H:%M:%S") == "09:30:00",arr.ind=TRUE))]

  actcounter <- 1 # count for which action time we are on
  
  # Order number initialize
  ordercounter <- 1
  
  looprow <- length(totaltime) # Assuming starttime and endtime are times in seconds
  
  # cat(looprow)
  
  #Define data
  
  for (i in 1:looprow){
    #check market condition
    IPR_df <- data.frame()
    
    response <- update_pendingorderbook(env, totaltime[i],stock)
    
    # Passive portion
    if (nrow(response)!=0){
      smatool <- passiveupdate(response, i, env, Stocks, smatool))
    }
    
    for (stock in Stocks){
      stock_data <- paste(stock,EquityList[1],sep="_")
      tick_data <- env[[stock_data]]
      
      #active portion of strategy
      if (totaltime[i] == actiontime[floor(actcounter)]){ 
        actcounter <- actcounter + 1/length(Stocks)
        orderline = data.frame(matrix(NA, 0, length(order_msg_spec)))
        colnames(orderline) <- order_msg_spec
        
        P_asterix <- tick_data[i, "LAST_PRICE"]
        P_asterix_j_date <-  tick_data[i - jump, "LAST_PRICE"]
        start_row <- 1
        price_estimates <- data.frame(Price = tick_data[start_row:i, "LAST_PRICE"])
        ret1 <- return_and_stdev(price_estimates)$ret
        stdev <- return_and_stdev(price_estimates)$stdev
        z <- (log(P_asterix/P_asterix_j_date) - jump * ret1) / (sqrt(jump * stdev))
        IPR <- pnorm(z)
        IPR_df <- rbind(IPR_df, data.frame(Date = totaltime[i], Symbol = stock, IPR = IPR))
        
        if (stock == Stocks[length(Stocks)]){
          if (floor(actcounter) == 1){
            currposition <- global_tables$positionbook[["0"]]
          } else {
            currposition <- global_tables[["positionbook"]][[length(global_tables[["positionbook"]])]]
          }
          
          totalvalue <- currposition$MarketValue[1]
          
          if (nrow(currposition) == 1){
            totalvalue <- currposition$MarketValue[1]
          } else {
            for (s in 2:nrow(currposition)){
              stock = currposition$Symbol[s]
              totalvalue <- totalvalue + currposition$Quantity[s]*tick_data[i,"LAST_PRICE"]
            }
          }
          for (b in 1:nrow(IPR_df)){
            stock = as.character(IPR_df$Symbol[b])
            stock_data <- paste(stock,EquityList[1],sep="_")
            tick_data <- env[[stock_data]]
            # assign percentages to each stock for cash allocations
            cash_alloc <- -(IPR_df[b, "IPR"]) + 0.5
            # send an order to the market with sell and using the existing cash * appropriate Pct
            # Calculate dollar value based on percentage cash_alloc * totalvalue
            allocvalue <- cash_alloc*totalvalue
            # Divide cashalloc*totalvalue by share price to obtain # of shares
            futurequant<- floor(allocvalue/tick_data[i,"LAST_PRICE"])
            # Subtract future # of shares with current number of shares
            pastquant = 0
            if (sum(currposition$Symbol == stock) == 0){
              pastquant = 0
            } else {
              pastquant = currposition[which(currposition$Symbol == stock),"Quantity"]
            }
            tradequant <- futurequant - pastquant
            # Send appropriate order
            
            entry <- nrow(orderline) + 1
            orderline[entry, Con_FieldName_MsgType] <- Con_MsgType_New
            orderline[entry, Con_FieldName_OrdID] <- ordercounter
            ordercounter <- ordercounter + 1
            orderline[entry, Con_FieldName_Sym] <- as.character(IPR_df$Symbol[b])
            
            orderline[entry, Con_FieldName_Qty] <- abs(tradequant)
            if (tradequant > 0){
              orderline[entry, Con_FieldName_Side] <- Con_Side_Buy
              orderline[entry, Con_FieldName_Price] <- tick_data[i,"LOW"]
            } else{
              orderline[entry, Con_FieldName_Side] <- Con_Side_Sell
              orderline[entry, Con_FieldName_Price] <- tick_data[i,"HIGH"]
            }
            orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt
            orderline[entry, Con_FieldName_Time] <- as.character(totaltime[i])
            #           print(orderline)
            #           response <- handle_orders(orderline, Stocks, global_tables, tick_data[a+29, "Date"])
            #           print(response)
          }
          response <- handle_orders(orderline, Stocks, global_tables, as.character(totaltime[i]))
          smatool <- passiveupdate(response, i, env, Stocks, smatool)
        }
      }
    }
  }
}

passiveupdate <- function(response, i, env, Stocks, smatool){
  # Send subsequent orders
  longdur <- 25
  shortdur <- 10
  for (stock in Stocks){
    stock_data <- paste(stock,EquityList[1],sep="_")
    tick_data <- env[[stock_data]]
    if (SMA(i, shortdur,tick_data) <= SMA(i,longdur, tick_data)){
      if smatool$symbol[sma] == 1{
        # Submit order to sell
      }
    } else {
      if smatool$symbol[sma] == 0{
        # Submit order to buy
      }
    }
        
    }
  }
}
##################################################
