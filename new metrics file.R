get_quantities <- function(curr_side, filtered_tradesbook, j, totalqty_owned){
  # add the quantity owned at each time step
  if (curr_side == 2){
    totalqty_owned <- totalqty_owned - abs(filtered_tradesbook[j, "Quantity"])
  } else if (curr_side == 1){
    totalqty_owned <- totalqty_owned + abs(filtered_tradesbook[j, "Quantity"])
  }
  # qty_owned <- filtered_tradesbook[j, "QuantityOwned"]
  return(totalqty_owned)
}

convert_posbook_df <- function(positionbook){
  positionbook <- do.call(rbind, positionbook)
  positionbook <- cbind(Timestamp = rownames(positionbook), positionbook)
  rownames(positionbook) <- 1:nrow(positionbook)
  positionbook$Timestamp <- sapply(strsplit(as.character(positionbook$Timestamp),".",fixed = TRUE), "[[", 1)
  return(positionbook)
}

output <- function(tradesbook, positionbook){
  EquityList <- c("tick", "ask", "bid")
  env <- global_tables
  
  # clear all outstanding positions
  
  ordercounter <- 1
  orderline = data.frame(matrix(NA, 0, length(order_msg_spec)))
  colnames(orderline) <- order_msg_spec
  positionbook <- convert_posbook_df(global_tables$positionbook)
  seclast_time <- as.character(as.POSIXct(positionbook[(nrow(positionbook)-1), "Timestamp"], origin = "1970-01-01"))
  last_time <- as.character(as.POSIXct(positionbook[nrow(positionbook), "Timestamp"], origin = "1970-01-01")+1740)
  sub_posbook <- subset(positionbook, Timestamp == seclast_time)
#   for (o in 2:(nrow(sub_posbook))){
#      if (sub_posbook[o, "Quantity"] != 0){
#        stock_data <- paste(sub_posbook[o, "Symbol"],EquityList[1],sep="_")
#        tick_data <- env[[stock_data]]
#        entry <- nrow(orderline) + 1
#        orderline[entry, Con_FieldName_MsgType] <- Con_MsgType_New
#        orderline[entry, Con_FieldName_OrdID] <- ordercounter
#        ordercounter <- ordercounter + 1
#        orderline[entry, Con_FieldName_Sym] <- sub_posbook$Symbol[o]
#        orderline[entry, Con_FieldName_Qty] <- abs(sub_posbook$Quantity[o])
#        tradequant <- sub_posbook[o, "Quantity"]
#        if (tradequant < 0){
#          orderline[entry, Con_FieldName_Side] <- Con_Side_Buy
#          orderline[entry, Con_FieldName_Price] <- tick_data[which(tick_data$Date == last_time),"LOW"]
#        } else {
#          orderline[entry, Con_FieldName_Side] <- Con_Side_Sell
#          orderline[entry, Con_FieldName_Price] <- tick_data[which(tick_data$Date == last_time),"HIGH"]
#        }
#        orderline[entry, Con_FieldName_OrdType] <- Con_OrdType_Mkt
#        orderline[entry, Con_FieldName_Time] <- as.character(last_time)
#      }
#   }
#   # for testing purposes: 
#   Stocks <- c("CPD", "SU", "ABX")
#   response <- handle_orders(orderline, Stocks, global_tables, as.character(last_time))
#   positionbook <- convert_posbook_df(global_tables$positionbook)
  
  # find the unique stocks that have been traded over the duration 
  stock_list <- unique(as.vector(tradesbook$Symbol))
  
  # Set up the data frame which will store the cumulative pnl values for each stock and portfolio 
  Pnl_df <- data.frame(Symbol=character(), DateTime=as.Date(character()), BidAskPrice=character(), 
                       BookValue=integer(), Side=integer(), Quantity = integer(),
                       Open.Close = character(), PnLStock = integer(), Cash = integer(),
                       Portfolio = integer(), PnLPortfolio = integer())
  
  # loop through all the stocks that have been traded 
  for (i in 1:length(stock_list)){
    stock_name <- stock_list[i] 
    # filter the tradesbook for only the selected stock 
    totalqty_owned <- 0
    filtered_tradesbook <- subset(tradesbook, Symbol == stock_name)
    rownames(filtered_tradesbook) <- 1:nrow(filtered_tradesbook)
    
    # loop through all the trades made in the tradesbook
    for (j in 1:(nrow(filtered_tradesbook)+1)){
      if (j == (nrow(filtered_tradesbook))){
        curr_side <- filtered_tradesbook$Side[j]
        curr_pos <- filtered_tradesbook$`Open/Close`[j]
        totalqty_owned <- get_quantities(curr_side, filtered_tradesbook, j, totalqty_owned)
        filtered_tradesbook[j, "QuantityOwned"] <- totalqty_owned
        book_value <- as.double(positionbook[positionbook$Timestamp == start_time_date & positionbook$Symbol == stock_name, ]["BookValue"] / 
                                  positionbook[positionbook$Timestamp == start_time_date & positionbook$Symbol == stock_name, ]["Quantity"])
        Pnl_df <- rbind(Pnl_df, data.frame(Symbol = stock_list[i] , DateTime = filtered_tradesbook[j, "Timestamp"],
                                           BidAskPrice = filtered_tradesbook[j, "Price"], 
                                           BookValue = book_value, Side = curr_side,
                                           Quantity = totalqty_owned, OpenClose = curr_pos))
        break
      }
      start_time_date <- filtered_tradesbook[j, "Timestamp"]
      end_time_date <- filtered_tradesbook[j + 1, "Timestamp"]
      next_pos <- filtered_tradesbook$`Open/Close`[j+1]
      # re-order so trades are closed before new trades are opened
      if (end_time_date == start_time_date & next_pos == "Close"){
        curr_line <- filtered_tradesbook[j, ] 
        next_line <- filtered_tradesbook[j+1, ] 
        filtered_tradesbook[j, ] <- next_line
        filtered_tradesbook[j+1, ] <- curr_line
        # next
      }
      curr_side <- filtered_tradesbook$Side[j]
      next_side <- filtered_tradesbook$Side[j+1]
      curr_pos <- filtered_tradesbook$`Open/Close`[j]
      next_pos <- filtered_tradesbook$`Open/Close`[j+1]
      book_value <- as.double(positionbook[positionbook$Timestamp == start_time_date & positionbook$Symbol == stock_name, ]["BookValue"] / 
                                 positionbook[positionbook$Timestamp == start_time_date & positionbook$Symbol == stock_name, ]["Quantity"])
      
      totalqty_owned <- get_quantities(curr_side, filtered_tradesbook, j, totalqty_owned)
      # qty_owned <- qty_owned_results[1]
      # totalqty_owned <- qty_owned_results[2]
      filtered_tradesbook[j, "QuantityOwned"] <- totalqty_owned
      
      if (curr_side == 1 && curr_pos == "Open"){
        # since we longed, the market value is the current bid price at HIGH (sell high)
        type <- "bid"
        stock_data <- paste(stock_name,EquityList[3],sep="_")
        stock_matrix <- env[[stock_data]]
      } else if (curr_side == 2 && curr_pos == "Open"){
        # since we shorted, the market value is the current ask price at LOW (buy low)
        type <- "ask"
        stock_data <- paste(stock_name,EquityList[2],sep="_")
        stock_matrix <- env[[stock_data]]
      } else if (curr_pos == "Close" && next_pos == "Open" && start_time_date == end_time_date){
        next
      }

      prices <- stock_matrix[which(stock_matrix$Date == start_time_date):(which(stock_matrix$Date == end_time_date)-1), "LAST_PRICE"]
      dates <- stock_matrix[which(stock_matrix$Date == start_time_date):(which(stock_matrix$Date == end_time_date)-1), "Date"]
      stock_names <- rep(stock_name, times = length(prices))
      book_values <- rep(book_value, times = length(prices))
      curr_sides <- rep(curr_side, times = length(prices))
      qty_owned <- rep(totalqty_owned, times = length(prices))
      curr_pos <- rep(curr_pos, times = length(prices))
      
      Pnl_df <- rbind(Pnl_df, data.frame(Symbol = stock_names, DateTime = dates,
                                         BidAskPrice = prices, 
                                         BookValue = book_values, Side = curr_sides,
                                         Quantity = qty_owned, OpenClose = curr_pos))
    }
    
  }
  test <- 90
# calculate the cumulative PnL of each stock 
  
  for (i in 1:NROW(Pnl_df)){
    curr_symbol <- as.character(Pnl_df[i, "Symbol"])
    prev_symbol <- as.character(Pnl_df[i-1, "Symbol"])
    curr_side <- Pnl_df[i, "Side"]
    
    # check if there is a 0 in BidAskPrice Column 
    
    if (Pnl_df[i, "BidAskPrice"] == 0){
      if (curr_symbol == prev_symbol){
        Pnl_df[i, "BidAskPrice"] <- Pnl_df[i-1, "BidAskPrice"]
      } else {
        Pnl_df[i, "BidAskPrice"] <- Pnl_df[i+1, "BidAskPrice"]
      }
    }
    
    # multiply by -1 to obtain a posititve quantity
    if (curr_side == 1){
      curr_pnl <- (Pnl_df[i, "BidAskPrice"] - Pnl_df[i-1, "BidAskPrice"])*Pnl_df[i, "Quantity"]
    } else {
      # book_value <- Pnl_df[i, "BookValue"]
      short_qty <- Pnl_df[i, "Quantity"]*-1
      curr_pnl <- (Pnl_df[i, "BidAskPrice"] - Pnl_df[i-1, "BidAskPrice"])*short_qty*-1
    }
    
    # find cum. value. Ensure that the cumulative value is only carried from previous if it was the same stock
    if (i == 1) {
      prev_pnl <- 0
      curr_pnl <- 0
    } else {
      if (curr_symbol == prev_symbol){
        prev_pnl <- Pnl_df[i-1, "CumPnLStock"]
      } else {
        prev_pnl <- 0
        curr_pnl <- 0
      }
    }
    Pnl_df[i, "CumPnLStock"] <- prev_pnl + curr_pnl
  }

  # order the Pnl data frame in order of date/time since it is sectioned by symbol
  
  Pnl_df <- Pnl_df[order(Pnl_df$DateTime, decreasing = FALSE), ]
  rownames(Pnl_df) <- 1:nrow(Pnl_df)
  
  # record the cash in the account at each time step, pulling from the positionbook. 
  
  for (i in 1:NROW(Pnl_df)){
    curr_date_time <- as.character(Pnl_df[i, "DateTime"])
    cash_amt <- which(positionbook$Timestamp==curr_date_time & positionbook$Symbol=="Cash")
    if (length(cash_amt)>0){
      Pnl_df[i, "Cash"] <- positionbook[which(positionbook$Timestamp==curr_date_time & positionbook$Symbol=="Cash"),
                                        "BookValue"]
    } else {
      Pnl_df[i, "Cash"] <- Pnl_df[i-1, "Cash"]
    }
  }
  
  # find the unique times in which some stock was owned
  time_list <- as.data.frame(unique(Pnl_df$DateTime))
  # change name of column header
  names(time_list)[1] <- "DateTime"
  
  # calculate portfolio values and pnl portfolio values at each unique time step 
  for (i in 1:NROW(time_list)){
    filtered_times <- subset(Pnl_df, DateTime == time_list[i,1])
    time_list[i, "PortfolioValue"] <- sum(filtered_times[,"BidAskPrice"]*filtered_times[,"Quantity"]) + filtered_times[nrow(filtered_times), "Cash"]
    time_list[i, "PnLPortfolio"] <- time_list[i, "PortfolioValue"] - init_cash
  }

  # calculate the final cumulative PnL of the portfolio 
  
  cumulative_pnl_portfolio <- time_list[NROW(time_list), "PnLPortfolio"]
  
  # calculate the final cumulative PnL of each stock traded

  cumulative_pnl_dataframe <- data.frame(Stock = stock_list, CumPnL = Pnl_df[(nrow(Pnl_df)-(length(stock_list)-1)):nrow(Pnl_df), "CumPnLStock"])
  
  # calculate the number of trades per day (trade is only when a position is closed)
  
  Unique_Dates_Traded <- unique(as.Date(unique(tradesbook$Timestamp)))
  Trades_distribution <- data.frame(Day = as.character(), TradeCount = as.integer())
  for (i in 1:length(Unique_Dates_Traded)){
    trades_count <- 0
    trades_count <- sum(tradesbook$`Open/Close` == "Close" & as.Date(tradesbook$Timestamp) == Unique_Dates_Traded[i])
    Trades_distribution <- rbind(Trades_distribution, data.frame(Day = Unique_Dates_Traded[i], 
                                                                 TradeCount = trades_count))
  }
  
  # calculate Portfolio PnL per day
  
  PnL_distribution <- data.frame(Date = as.Date(as.character()), PortfolioOpen = as.integer(),
                                 PortfolioClose = as.integer(), PnL = as.integer())
  for (i in 1:length(Unique_Dates_Traded)){
    temp_matrix <- subset(time_list, as.Date(time_list$DateTime) == Unique_Dates_Traded[i])
    Portfolio_Open <- temp_matrix[1, "PortfolioValue"]
    Portfolio_Close <- temp_matrix[NROW(na.omit(temp_matrix)), "PortfolioValue"]
    PnL <- (Portfolio_Close - Portfolio_Open)
    PnL_distribution <- rbind(PnL_distribution, data.frame(Date = Unique_Dates_Traded[i],
                                                           PortfolioOpen = Portfolio_Open,
                                                           PortfolioClose = Portfolio_Close,
                                                           PnL = PnL))
  }
  
  # calculate the average daily Portfolio PnL 
  
  average_daily_PnL <- mean(PnL_distribution$PnL)
  
  # calculate the percent days portfolio is profitable
  
  percent_profitable_days <- (sum(PnL_distribution$PnL > 0)/NROW(PnL_distribution))*100
  
  # calculate the number of trades in total
  
  total_trades <- sum(Trades_distribution$TradeCount)
  
  # calculate the average number of trades per day 
  
  average_daily_trades <- mean(Trades_distribution$TradeCount)
  
  # calculate the average PnL of all trades
  
  filter_pnL <- subset(tradesbook, PnL != "")
  average_PnL_all_trades <- mean(as.numeric(filter_pnL$PnL))
  
  # calculate the average PnL per trade by stock 
  
  average_pnl_dataframe <- data.frame(Symbol = as.character(), AveragePnLPerTrade = as.integer())
  
  for (i in 1:length(stock_list)){
    stock_name <- stock_list[i] 
    temp_matrix <- subset(tradesbook, Symbol == stock_name)
    avg_pnl <- mean(as.numeric(subset(temp_matrix, PnL != "")$PnL))
    average_pnl_dataframe <- rbind(average_pnl_dataframe, data.frame(Symbol = stock_name,
                                                                     AveragePnLPerTrade = avg_pnl))
  }
  
  # calculate the percent of trades that are profitable (percent of closed positions that are proftiable)
  
  percent_trades_profitable <- (sum(filter_pnL$PnL>0)/NROW(filter_pnL))*100
  
  # calculate maximum drawdown and maximum drawdown period
  
  library(tseries)
  mxdrwdown <- maxdrawdown(time_list$PortfolioValue)
  MDD <- mxdrwdown$maxdrawdown
  MDD_from <- time_list[mxdrwdown$from, "PortfolioValue"]
  MDD_to <- time_list[mxdrwdown$to, "PortfolioValue"]
  MDD_peak <- max(c(MDD_from, MDD_to))
  MDD_trough <- min(c(MDD_from, MDD_to))
  MDD_perc <- (MDD_trough - MDD_peak)/(MDD_peak)*100
  MDD_period <- c(time_list[mxdrwdown$from, "DateTime"], time_list[mxdrwdown$to, "DateTime"])
  
  # calculate cumulative returns for each stock for each trade 
#   
#   ret_dataframe <- data.frame(Symbol = as.character(), CumAssetReturns = as.integer())
#   
#   for (i in 1:length(stock_list)){
#     filter_trades <- subset(tradesbook, Symbol == stock_list[i])
#     filter_position <- subset(positionbook, Symbol == stock_list[i])
#     asset_return <- 0
#     for (j in 1:NROW(filter_trades)){
#       if (filter_trades[j, "Open/Close"] == "Close"){
#         date_time_1 <- filter_trades[j, "Timestamp"]
#         bk_value <- Pnl_df[Pnl_df$DateTime == date_time_1 & Pnl_df$Symbol == stock_list[i], ]["BookValue"]
#         bk_value <- bk_value[nrow(bk_value), 1]
#         cur_value <- as.numeric(filter_trades[j, "Price"])
#         cur_side <- filter_trades[j, "Side"]
#         cur_asset_return <- (cur_value - bk_value)/(bk_value)*100
#         if (cur_side == 1){
#           cur_asset_return <- abs(cur_asset_return)
#         }
#         asset_return <- cur_asset_return + asset_return 
#       }
#     }
#     ret_dataframe <- rbind(ret_dataframe, data.frame(Symbol = stock_list[i], CumAssetReturns = asset_return))
#   }
  
  # calculate annualised portfolio return and period portfolio return 
  
  no_trading_days_yearly <- 250 
  period_portfolio_return <- as.numeric(((PnL_distribution[nrow(PnL_distribution), "PortfolioClose"] - PnL_distribution[1, "PortfolioOpen"]) / 
                                PnL_distribution[nrow(PnL_distribution), "PortfolioOpen"]) * 100)
  
  # annualised_portfolio_return <- as.double(as.integer((1 + period_portfolio_return)) ^ (length(Unique_Dates_Traded)/as.integer(no_trading_days_yearly)))
  
  # calculate annualised portfolio standard deviation from daily returns 
  
  # calculate Sharpe Ratio. Need to access the market dataframe whih stores data of the S&P500
  
  #market_init_price <- marketdata[which(marketdata$Date==init_day), "Date"]
  #market_close_price <- marketdata[which(marketdata$Date==end_day), "Date"]
  #market_return <- ((market_close_price - market_init_price)/market_init_price)*100
  #sharpe_ratio <- (period_portfolio_return - market_return)/stdev
  
  # distribution of cumulative PnL of each stock 
  
  for (i in 1:length(stock_list)){
    stock_name <- stock_list[i] 
    temp_matrix <- subset(Pnl_df, Symbol == stock_name)
    name <- paste(stock_name, "CumPnL")
    name <- paste(name, ".pdf", sep="")
    pdf(name)
    plot(temp_matrix$CumPnLStock, xaxt = "n", type = "l", xlab= "Date", ylab = "CumPnL", main = paste(stock_name, "CumPnL"))
    #axis(1, at=as.numeric(temp_matrix$DateTime), las=2)
    dev.off()
    # axis(1, at = temp_matrix$DateTime)
  }
  
  # distribution of cumulative PnL of Portfolio
  
  name <- paste("CumPnLPortfolio", ".pdf", sep="")
  pdf(name)
  plot(time_list$PnLPortfolio, xaxt = "n", type = "l", xlab="Days", ylab="PnL ($)", main="PnL of Portfolio")
  #axis(1, at=as.numeric(temp_matrix$DateTime), las=2)
  dev.off()
  
  # distribution of trades per day 
  
  name <- paste("TradesPerDay", ".pdf", sep="")
  pdf(name)
  trades_per_day_plot <- plot(Trades_distribution$Day, Trades_distribution$TradeCount, 
                              main="Trades Per Day", xlab="Days", 
                              ylab="Number of Trades")
  axis(2, at = seq(7, max(Trades_distribution$TradeCount, 7))) 
  dev.off()
  
  # distribution of PnL per day 
  
  name <- paste("PnLPerDay", ".pdf", sep="")
  pdf(name)
  PnL_per_day_plot <- plot(PnL_distribution$PnL, xaxt = "n",
                           main="Portfolio PnL Per Day", xlab="Days", 
                           ylab="PnL ($)")
  dev.off()
  
  # distribution of PnL per trade ($ per share)
  
  name <- paste("PnLPerTrade", ".pdf", sep="")
  pdf(name)
  PnL_per_trade_plot <- plot(filter_pnL$PnL, xaxt = "n",
                             main="PnL Per Trade", xlab="Days", 
                             ylab="PnL ($)", type = "l")
  dev.off()
  
  # distribution of PnL per trade ($ per $ invested)
  
  # correlation of cumulative return with market return
  
  # number of stocks in the portfolio 
  
  no_stocks <- length(stock_list)
  
  # summary statistics that pertain to the portfolio
  
  summary_statistics_all <- data.frame(NumberofTradesTotal = total_trades, AvgTradesPerDay = average_daily_trades,
                                       AvgDailyPnL = average_daily_PnL,CumPnLPortfolio = cumulative_pnl_portfolio, 
                                       PctDaysProfitable = percent_profitable_days, AvgPnLAllTrades = average_PnL_all_trades,
                                       PctTradesProfitable = percent_trades_profitable,
                                       PeriodPortfolioReturn = period_portfolio_return, MaxDrawdown = MDD_perc,
                                       NumberofStocksInPortfolio = no_stocks) 
  
  # summary statistics that pertain to the specific stocks
  
  colnames(cumulative_pnl_dataframe) <- c("Symbol", "CumPnL")
  summary_statistics_byStock <- merge(cumulative_pnl_dataframe, average_pnl_dataframe)
  
  # summary statistics that pertain to days
  
  colnames(Trades_distribution) <- c("Date", "TradeCount")
  summary_statistics_byday <- merge(Trades_distribution, PnL_distribution)
  
  return(list(summary_statistics_all, summary_statistics_byStock, summary_statistics_byday))
}