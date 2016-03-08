# source('backtest_lib.r')
source('SMA.R')

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

strategy_impliedpricerisk <- function(Stocks, env){
  # Active portion of strategy
  IPR_df <- data.frame(Date = as.character(), Symbol = as.character(), IPR = as.integer())
  
  global_tables[[Con_GlobalVarName_LOB]]<- data.frame(matrix(0, 0, length(orderbook_spec)))
  colnames(global_tables[[Con_GlobalVarName_LOB]]) <- orderbook_spec
  
  #the position book is a list of data frames
  init_pos <- data.frame(matrix(0, 1, length(positionbook_spec)))
  colnames(init_pos) <- positionbook_spec
  init_pos[,Con_FieldName_Sym] = Con_Sym_Cash
  init_pos[,c(Con_FieldName_Qty, Con_FieldName_BookVal, Con_FieldName_MktVal)] = init_cash
  
  global_tables[[Con_GlobalVarName_PositionBook]] <- list(init_pos)
  names(global_tables[[Con_GlobalVarName_PositionBook]])[1] = 0
  
  global_tables[[Con_GlobalVarName_TradesBook]] <- data.frame(matrix(0, 0, length(tradesbook_spec)))
  colnames(global_tables[[Con_GlobalVarName_TradesBook]]) <- tradesbook_spec
  
  global_tables[[Con_GlobalVarName_MktPrice]] <- list(vector())
  global_tables[[Con_GlobalVarName_BidPrice]] <- list(vector())
  global_tables[[Con_GlobalVarName_AskPrice]] <- list(vector())
  global_tables[[Con_GlobalVarName_ListDates]] <- list(vector())
  
  #Stocks <- c("AC", "BNS", "BMO")
  EquityList <- c("tick", "ask", "bid")
  jump <- 13
  endtime <- 390
  # if SMA tool = 1, then short sma > long sma and we buy, and vice versa
  smatool <- data.frame(matrix(2, nrow = length(Stocks), ncol = 2))
  colnames(smatool) <- c("Symbol", "sma")
  smatool$Symbol <- Stocks
  stock_data <- paste(Stocks[1],EquityList[1],sep="_")
  totaltime <- env[[stock_data]][["Date"]][1:endtime]
  actiontime <- totaltime[seq(1,endtime,30)] # Times to perform active portion
  actiontime <- actiontime[-c(which(strftime(actiontime, format="%H:%M:%S") == "09:30:00",arr.ind=TRUE))]

  actcounter <- 1 # count for which action time we are on
  
  # Order number initialize
  ordercounter <- 1
  
  looprow <- length(totaltime) # Assuming starttime and endtime are times in seconds
  
  cat(looprow)
  
  #Define data
  
  for (i in 1:looprow){
    #check market condition
    IPR_df <- data.frame()
    
    response <- update_pendingorderbook(env, totaltime[i],Stocks)
    
    # Passive portion
    if (nrow(response)!=0){
      smatool <- passiveupdate(response, i, env, Stocks, smatool)
    }
    
    for (stock in Stocks){
      stock_data <- paste(stock,EquityList[1],sep="_")
      tick_data <- env[[stock_data]]
      
      #active portion of strategy
      if (totaltime[i] == actiontime[actcounter]){ 
        
        orderline = data.frame(matrix(NA, 0, length(order_msg_spec)))
        colnames(orderline) <- order_msg_spec
        
        P_asterix <- tick_data[i-1, "LAST_PRICE"]
        P_asterix_j_date <-  tick_data[i - jump - 1, "LAST_PRICE"]
        start_row <- 1
        price_estimates <- data.frame(Price = tick_data[start_row:i, "LAST_PRICE"])
        ret1 <- return_and_stdev(price_estimates)$ret
        stdev <- return_and_stdev(price_estimates)$stdev
        z <- (log(P_asterix/P_asterix_j_date) - jump * ret1) / (sqrt(jump * stdev))
        IPR <- pnorm(z)
        IPR_df <- rbind(IPR_df, data.frame(Date = totaltime[i], Symbol = stock, IPR = IPR))
        
        if (stock == Stocks[length(Stocks)]){
          if (actcounter == 1){
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
              stock_data <- paste(stock,EquityList[1],sep="_")
              tick_data <- env[[stock_data]]
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
          # smatool <- passiveupdate(response, i, env, Stocks, smatool)
          if (actcounter != length(actiontime)){
            actcounter <- actcounter + 1
          } else {
            actcounter <- length(actiontime)
          }
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
    stock_data <- paste(stock,"tick",sep="_")
    tick_data <- env[[stock_data]]
    if (SMA(i, shortdur,tick_data$LAST_PRICE) <= SMA(i,longdur, tick_data$LAST_PRICE)){
      if (smatool[which(smatool$Symbol == stock),"sma"] == 1){
        # Submit order to sell
      } else {
        smatool[which(smatool$Symbol == stock),"sma"] == 0
      }
      smatool[which(smatool$Symbol == stock),"sma"] == 0
    } else {
      if (smatool[which(smatool$Symbol == stock),"sma"] == 0){
        # Submit order to buy
      } else {
        smatool[which(smatool$Symbol == stock),"sma"] == 1
      }
      smatool[which(smatool$Symbol == stock),"sma"] == 1
    }
  }
}

##################################################
