# constants
source('backtest_lib.r')
jump <- 13

return_and_stdev <- function(prices){
  for (i in 2:NROW(prices)){
    prices[i, "Return"] <- log10(prices[i,"Price"]/prices[i-1,"Price"])
  }
  final_return <- mean(na.omit(prices[, "Return"]))
  for (i in 2:NROW(prices)){
    prices[i, "StDev"] <- (prices[i, "Return"]-final_return)^2
  }
  final_stdev <- sum(na.omit(prices[, "StDev"]))/(NROW(prices)-1)
  return(list(return=final_return, stdev=final_stdev))
}

strategy_ipr <- function(env){
  IPR_df <- data.frame(Date = as.character(), Symbol = as.character(), IPR = as.integer())
  Stocks <- c("AC", "BNS", "BMO")
  #Stocks <- c("AC")
  EquityList <- c("tick", "ask", "bid")
  a <- 1
  # only trading every 30 minutes, change this value when trading time is different 
  end_a <- nrow(env[["list_dates"]])*12*30
  end_a <- 1920
  
  # loop through each minute (a) but only calculate IPR every 30 minutes  
  
  while (a < end_a){
    for (stock in Stocks){
      stock_data <- paste(stock,EquityList[1],sep="_")
      tick_data <- env[[stock_data]]
      if (strftime(tick_data[a, "Date"], format="%H:%M:%S") == "15:30:00"){
        a <- a + 31
        next
      }
      next_date <- tick_data[a+29, "Date"] 
      P_asterix <- tick_data[a+29, "LAST_PRICE"]
      P_asterix_j_date <-  tick_data[(a+29) - jump, "LAST_PRICE"]
      start_row <- 1
      end_row <- which(tick_data$Date == next_date)
      price_estimates <- data.frame(Price = tick_data[start_row:end_row, "LAST_PRICE"])
      ret <- return_and_stdev(price_estimates)$return
      stdev <- return_and_stdev(price_estimates)$stdev
      z <- (log(P_asterix/P_asterix_j_date) - jump * ret) / (sqrt(jump * stdev))
      IPR <- pnorm(z)
      IPR_df <- rbind(IPR_df, data.frame(Date = next_date + 60, Symbol = stock, IPR = IPR))
      if (stock == Stocks[length(Stocks)]){
        temp_dframe <- subset(IPR_df, Date == next_date+60)
        temp_dframe$Symbol <- as.character(temp_dframe$Symbol)
        # order the dataframe based on IPR values 
        # temp_dframe <- temp_dframe[order(temp_dframe$IPR),]
        # create buy or sell orders
        
        # Access positionbook, find current stock position and current value
        # Sum up all values to get total portfolio value
        if (a + 29 == 30){
          currposition <- global_tables$positionbook[["0"]]
        } else {
          currposition <- global_tables[["positionbook"]][[length(global_tables[["positionbook"]])]]
        }
        
        totalvalue <- 0
        totalvalue <- currposition$MarketValue[1]
        
        if (nrow(currposition) == 1){
          totalvalue <- currposition$MarketValue[1]
        } else {
          for (s in 2:nrow(currposition)){
            stock = currposition$Symbol[s]
            stock_data <- paste(stock,EquityList[1],sep="_")
            tick_data <- env[[stock_data]]
            totalvalue <- totalvalue + currposition$Quantity[s]*tick_data[which(tick_data$Date == (next_date+60)),"LAST_PRICE"]
          }
        }
          
        ordercounter <- 1
        orderline = data.frame(matrix(NA, 0, length(order_msg_spec)))
        colnames(orderline) <- order_msg_spec
        
        for (b in 1:nrow(temp_dframe)){
            stock = as.character(temp_dframe$Symbol[b])
            stock_data <- paste(stock,EquityList[1],sep="_")
            tick_data <- env[[stock_data]]
            # assign percentages to each stock for cash allocations
            cash_alloc <- -(temp_dframe[b, "IPR"]) + 0.5
            # send an order to the market with sell and using the existing cash * appropriate Pct
            # Calculate dollar value based on percentage cash_alloc * totalvalue
            allocvalue <- cash_alloc*totalvalue
            # Divide cashalloc*totalvalue by share price to obtain # of shares
            futurequant<- floor(allocvalue/tick_data[a+30,"LAST_PRICE"])
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
            orderline[entry, Con_FieldName_Sym] <- temp_dframe$Symbol[b]
  
            orderline[entry, Con_FieldName_Qty] <- abs(tradequant)
            if (tradequant > 0){
              orderline[entry, Con_FieldName_Side] <- Con_Side_Buy
              # don't know if this is getting the right row (a+30) since a is incrementing differently
              # than the rows of tick_data. NEED TO CHECK 
              orderline[entry, Con_FieldName_Price] <- tick_data[a+30,"LOW"]
            } else{
              orderline[entry, Con_FieldName_Side] <- Con_Side_Sell
              orderline[entry, Con_FieldName_Price] <- tick_data[a+30,"HIGH"]
            }
            orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt
            orderline[entry, Con_FieldName_Time] <- as.character(next_date+60)
  #           print(orderline)
  #           response <- handle_orders(orderline, Stocks, global_tables, tick_data[a+29, "Date"])
  #           print(response)
        }
      }
    }
    response <- handle_orders(orderline, Stocks, global_tables, as.character(next_date+60))
    a <- a + 30
    # The following was not run so I did it manually to get the proper position and tradesbook 
    # if at the the end of all iterations we still own some quantity of stock, we need to clear inventory
    # may consider putting "generate_orderline" as a function in the future
    # I have hard-coded "2015-05-20 15:30:00" as the date (only valid for end_a = 1080)
    if (a == (end_a-1)){
      ordercounter <- 1
      orderline = data.frame(matrix(NA, 0, length(order_msg_spec)))
      colnames(orderline) <- order_msg_spec
      positionbook <- do.call(rbind, global_tables$positionbook)
      positionbook <- cbind(Timestamp = rownames(positionbook), positionbook)
      rownames(positionbook) <- 1:nrow(positionbook)
      positionbook$Timestamp <- sapply(strsplit(as.character(positionbook$Timestamp),".",fixed = TRUE), "[[", 1)
      sub_posbook <- subset(positionbook, Timestamp == "2015-05-20 15:30:00")
      for (o in 2:nrow(sub_posbook)){
        if (sub_posbook[o, "Quantity"] != 0){
          stock_data <- paste(sub_posbook[o, "Symbol"],EquityList[1],sep="_")
          tick_data <- env[[stock_data]]
          entry <- nrow(orderline) + 1
          orderline[entry, Con_FieldName_MsgType] <- Con_MsgType_New
          orderline[entry, Con_FieldName_OrdID] <- ordercounter
          ordercounter <- ordercounter + 1
          orderline[entry, Con_FieldName_Sym] <- sub_posbook$Symbol[o]
          orderline[entry, Con_FieldName_Qty] <- sub_posbook$Quantity[o]
          tradequant <- sub_posbook[o, "Quantity"]
          if (tradequant < 0){
            orderline[entry, Con_FieldName_Side] <- Con_Side_Buy
            orderline[entry, Con_FieldName_Price] <- tick_data[which(tick_data$Date == "2015-05-20 15:30:00"),"LOW"]
          } else {
            orderline[entry, Con_FieldName_Side] <- Con_Side_Sell
            orderline[entry, Con_FieldName_Price] <- tick_data[which(tick_data$Date == "2015-05-20 15:30:00"),"HIGH"]
          }
          orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt
          orderline[entry, Con_FieldName_Time] <- as.character("2015-05-20 15:30:00")
        }
      }
      # for testing purposes: 
      Stocks <- c("AC", "BNS", "BMO")
      response <- handle_orders(orderline, Stocks, global_tables, as.character("2015-05-20 15:30:00"))
    }
  }
}
