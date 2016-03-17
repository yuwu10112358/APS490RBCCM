# output <- function(tradesbook, positionbook, ask_prices, bid_prices, market_data){
EquityList <- c("tick", "ask", "bid")
env <- global_tables
# tradesbook <- global_tables$tradesbook
# positionbook <- global_tables$positionbook
tradesbook <- read.csv("tradebookipr.csv", header = TRUE)
positionbook <- read.csv("positionbookipr.csv", header = TRUE)
tradesbook$Timestamp <- as.POSIXct(tradesbook$Timestamp)
positionbook$Timestamp <- as.POSIXct(positionbook$Timestamp)
tradesbook$Symbol <- as.character(tradesbook$Symbol)


# manipulate positionbook to make it a dataframe (it is received as a list)
positionbook <- do.call(rbind, positionbook)
positionbook <- cbind(Timestamp = rownames(positionbook), positionbook)
rownames(positionbook) <- 1:nrow(positionbook)
positionbook$Timestamp <- sapply(strsplit(as.character(positionbook$Timestamp),".",fixed = TRUE), "[[", 1)

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
  for (j in 1:nrow(filtered_tradesbook)){
    if (j == nrow(filtered_tradesbook)){
      break
    }
    curr_side <- filtered_tradesbook$Side[j]
    next_side <- filtered_tradesbook$Side[j+1]
    # change the period back to a back slash since the csv changes the period to a back slash 
    curr_pos <- filtered_tradesbook$Open.Close[j]
    next_pos <- filtered_tradesbook$Open.Close[j+1]
    start_time_date <- filtered_tradesbook[j, "Timestamp"]
    end_time_date <- filtered_tradesbook[j + 1, "Timestamp"]
    # re-order so trades are closed before new trades are opened
    if (end_time_date == start_time_date & next_pos == "Close"){
      curr_line <- filtered_tradesbook[j, ] 
      next_line <- filtered_tradesbook[j+1, ] 
      filtered_tradesbook[j, ] <- next_line
      filtered_tradesbook[j+1, ] <- curr_line
    }
    book_value <- positionbook[positionbook$Timestamp == start_time_date & positionbook$Symbol == stock_name, ]["BookValue"] / 
      positionbook[positionbook$Timestamp == start_time_date & positionbook$Symbol == stock_name, ]["Quantity"]
    
    # add the quantity owned at each time step
    if (curr_side == 2){
      totalqty_owned <- totalqty_owned - abs(filtered_tradesbook[j, "Quantity"])
      filtered_tradesbook[j, "QuantityOwned"] <- totalqty_owned
    } else if (curr_side == 1){
      totalqty_owned <- totalqty_owned + abs(filtered_tradesbook[j, "Quantity"])
      filtered_tradesbook[j, "QuantityOwned"] <- totalqty_owned
    }
    
  
    qty_owned <- filtered_tradesbook[j, "QuantityOwned"]
    
    if (start_time_date == end_time_date){
      if (j == (nrow(filtered_tradesbook)-1)){
        break
      }
    }
      
      if (curr_side == 1 && curr_pos == "Open"){
        stock_data <- paste(stock_name,EquityList[2],sep="_")
        stock_matrix <- env[[stock_data]]
        type <- "bid"
      } else if (curr_side == 2 && curr_pos == "Open"){
        stock_data <- paste(stock_name,EquityList[3],sep="_")
        stock_matrix <- env[[stock_data]]
        type <- "ask"
      } # else if (curr_pos == "Close") {
    #next
    #}
      
      for (k in 1:NROW(stock_matrix)){
        if (stock_matrix[k, "Date"] > end_time_date) {
          break
        } else if (stock_matrix[k,"Date"] >= start_time_date){
          # must clarify what price would we realize profits at (i.e what is market value at a given time)
          # add part in future if trading more than one stock, must access the correct market data
          # depending on what the value of stock_name is 
          if (type == "ask") {
            # if we shorted (curr_pos = 2), then the market value is the current ask price at LOW (buy low)
            # assumed that the ask and bid prices are identical to tick (i.e same number of rows) 
            stock_data <- paste(stock_name,EquityList[2],sep="_")
            bidask_matrix <- env[[stock_data]]
            bidask_price <- bidask_matrix[k, "LOW"]
          } else {
            # if we longed (curr_pos = 1), then the market value is the current bid price at HIGH (sell high)
            stock_data <- paste(stock_name,EquityList[3],sep="_")
            bidask_matrix <- env[[stock_data]]
            bidask_price <- bidask_matrix[k, "HIGH"]
          }
#           if (stock_matrix[k, "Date"] == end_time_date) {
#             curr_pos <- "Close"
#             if (curr_side == 1){
#               curr_side == 2
#             } else {
#               curr_side == 1
#             }
#           }
          Pnl_df <- rbind(Pnl_df, data.frame(Symbol = stock_name, DateTime = stock_matrix[k, "Date"],
                                             BidAskPrice = bidask_price, 
                                             BookValue = book_value, Side = curr_side,
                                             Quantity = qty_owned, OpenClose = curr_pos))
        }
      }
      
  }
}

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

# record the cash in the account at each time step, pulling from the positionbook. 

for (i in 1:NROW(Pnl_df)){
  curr_date_time <- Pnl_df[i, "DateTime"]
  
  # start for loop on second element since the first is at time 0 and records the default cash position
  for (j in 2:NROW(positionbook)){
    if (positionbook[j, "Timestamp"] == curr_date_time && positionbook[j, "Symbol"] == "Cash"){
      Pnl_df[i, "Cash"] <- positionbook[j, "BookValue"]
    } 
  }
  # if there is no datetime match in the positions matrix, this means that there was no position
  # taken at the time and therefore cash is just equal to the previous time step (no change)
  if (is.na(Pnl_df[i, "Cash"])){
    Pnl_df[i, "Cash"] <- Pnl_df[i-1, "Cash"]
  } 
}

# # add the last cash position. NEED TO CHANGE IN FUTURE 
# 
# need_row <- Pnl_df[nrow(Pnl_df), ]
# need_row[, "Cash"] <- positionbook[nrow(positionbook), "MarketValue"]
# Pnl_df <- rbind(Pnl_df, need_row)

# find the unique times in which some stock was owned
time_list <- as.data.frame(unique(Pnl_df$DateTime))
# change name of column header
names(time_list)[1] <- "DateTime"

# calculate portfolio values at each unique time step 
for (i in 1:NROW(time_list)){
  filtered_times <- subset(Pnl_df, DateTime == time_list[i,1])
  liquidated_positions <- 0
  if (i == nrow(time_list)){
    time_list[i, "PortfolioValue"] <- filtered_times[nrow(filtered_times), "Cash"]
    break
  }
  #for (j in 1:NROW(filtered_times)){
    stock_traded <- as.character(unique(filtered_times$Symbol))
    for (k in 1:length(stock_traded)){
      tmp <- subset(filtered_times, Symbol == stock_traded[k])
      # if (tmp[nrow(tmp), "Side"] == 2){
        #liquidated_positions <- -(tmp[nrow(tmp), "BidAskPrice"]*(abs(tmp[nrow(tmp), "Quantity"]))) + liquidated_positions
      #} else {
        liquidated_positions <- tmp[nrow(tmp), "BidAskPrice"]*(tmp[nrow(tmp), "Quantity"]) + liquidated_positions
      #}
    }
  #}
  time_list[i, "PortfolioValue"] <- filtered_times[nrow(filtered_times), "Cash"] + liquidated_positions
}

# calculate cumulative portfolio pnl at each time step

for (i in 1:NROW(time_list)){
  curr_pnl <- time_list[i, "PortfolioValue"] - init_cash
  time_list[i, "PnLPortfolio"] <- curr_pnl
}

dt <- c(0, time_list[, "PnLPortfolio"])
time_list[, "CumPnLPortfolio"] <- cumsum(time_list[, "PnLPortfolio"])

# calculate the final cumulative PnL of the portfolio 

cumulative_pnl_portfolio <- time_list[NROW(time_list), "PnLPortfolio"]

# calculate the final cumulative PnL of each stock traded

cumulative_pnl_dataframe <- data.frame(Symbol = as.character(), CumPnL = as.integer())

for (i in 1:length(stock_list)){
  stock_name <- stock_list[i]  
  temp_matrix <- subset(Pnl_df, Symbol == stock_name)
  cum_pnl <- temp_matrix[NROW(temp_matrix), "CumPnLStock"]
  cumulative_pnl_dataframe <- rbind(cumulative_pnl_dataframe, data.frame(Symbol = stock_name,
                                                                         CumPnL = cum_pnl))
}

# calculate the number of trades per day (trade is only when a position is closed)

Unique_Dates_Traded <- as.Date(unique(tradesbook$Timestamp))
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

ret_dataframe <- data.frame(Symbol = as.character(), CumAssetReturns = as.integer())

for (i in 1:length(stock_list)){
  filter_trades <- subset(tradesbook, Symbol == stock_list[i])
  filter_position <- subset(positionbook, Symbol == stock_list[i])
  asset_return <- 0
  for (j in 1:NROW(filter_trades)){
    if (filter_trades[j, "Open/Close"] == "Close"){
      date_time_1 <- filter_trades[j, "Timestamp"]
      bk_value <- Pnl_df[Pnl_df$DateTime == date_time_1 & Pnl_df$Symbol == stock_list[i], ]["BookValue"]
      bk_value <- bk_value[nrow(bk_value), 1]
      cur_value <- as.numeric(filter_trades[j, "Price"])
      cur_side <- filter_trades[j, "Side"]
      cur_asset_return <- (cur_value - bk_value)/(bk_value)*100
      if (cur_side == 1){
        cur_asset_return <- abs(cur_asset_return)
      }
      asset_return <- cur_asset_return + asset_return 
    }
  }
  ret_dataframe <- rbind(ret_dataframe, data.frame(Symbol = stock_list[i], CumAssetReturns = asset_return))
}

# calculate stock price minutely returns 

# calculate annualised portfolio return and period portfolio return 

no_trading_days_yearly <- 250 
period_portfolio_return <- ((PnL_distribution[nrow(PnL_distribution), "PortfolioClose"] - PnL_distribution[1, "PortfolioOpen"]) / 
  PnL_distribution[nrow(PnL_distribution), "PortfolioOpen"]) * 100 

annualised_portfolio_return <- (1 + period_portfolio_return) ^ (length(Unique_Dates_Traded)/no_trading_days_yearly)

# calculate annualised portfolio standard deviation from daily returns 

# calculate Sharpe Ratio. Need to access the market dataframe whih stores data of the S&P500

market_init_price <- marketdata[which(marketdata$Date==init_day), "Date"]
market_close_price <- marketdata[which(marketdata$Date==end_day), "Date"]
market_return <- ((market_close_price - market_init_price)/market_init_price)*100
sharpe_ratio <- (period_portfolio_return - market_return)/stdev

# distribution of cumulative PnL of each stock 

for (i in 1:length(stock_list)){
  stock_name <- stock_list[i] 
  temp_matrix <- subset(Pnl_df, Symbol == stock_name)
  temp_plot <- plot(temp_matrix$DateTime, temp_matrix$CumPnLStock, main=paste(stock_name, "cumulative PnL"),
                    xlab="Days", ylab="Cumulative PnL ($)", type = "l")
  # axis(1, at = temp_matrix$DateTime)
}

# distribution of cumulative PnL of Portfolio

cum_pnl_portfolio_plot <- plot(time_list$DateTime, time_list$PnLPortfolio, main="PnL of Portfolio",
                               xlab="Days", ylab="PnL ($)", type = "l")

# distribution of trades per day 

trades_per_day_plot <- plot(Trades_distribution$Day, Trades_distribution$TradeCount, 
                            main="Trades Per Day", xlab="Days", 
                            ylab="Number of Trades")
axis(2, at = seq(7, max(Trades_distribution$TradeCount, 7))) 

# distribution of PnL per day 

PnL_per_day_plot <- plot(PnL_distribution$Date, PnL_distribution$PnL, 
                         main="Portfolio PnL Per Day", xlab="Days", 
                         ylab="PnL ($)")

# distribution of PnL per trade ($ per share)

PnL_per_trade_plot <- plot(as.POSIXct(as.character(filter_pnL$Timestamp), origin = "1970-01-01"), filter_pnL$PnL, 
                         main="PnL Per Trade", xlab="Days", 
                         ylab="PnL ($)", type = "l")

# distribution of PnL per trade ($ per $ invested)

# correlation of cumulative return with market return

# number of stocks in the portfolio 

no_stocks <- length(stock_list)

# summary statistics that pertain to the portfolio

summary_statistics_all <- data.frame(NumberofTradesTotal = total_trades, AvgTradesPerDay = average_daily_trades,
                                     AvgDailyPnL = average_daily_PnL,
                                     PctDaysProfitable = percent_profitable_days, AvgPnLAllTrades = average_PnL_all_trades,
                                     PctTradesProfitable = percent_trades_profitable,
                                     PeriodPortfolioReturn = period_portfolio_return,
                                     NumberofStocksInPortfolio = no_stocks) 

# summary statistics that pertain to the specific stocks

summary_statistics_byStock <- merge(cumulative_pnl_dataframe, average_pnl_dataframe)


# return(summary_statistics_all, summary_statistics_byStock)

#}