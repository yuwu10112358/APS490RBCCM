# output <- function(tradesbook, positionbook, ask_prices, bid_prices, market_data){
EquityList <- c("tick", "ask", "bid")
env < - global_tables
### TESTED
tradesbook <- global_tables$tradesbook
# manually add a row into the tradebook since the naive strategy doesn't empty inventory at end of session


positionbook <- global_tables$positionbook
### TESTED

### TESTED
# manipulate positionbook to make it a dataframe (it is received as a list)

positionbook <- do.call(rbind, positionbook)
positionbook <- cbind(Timestamp = rownames(positionbook), positionbook)
rownames(positionbook) <- 1:nrow(positionbook)
positionbook$Timestamp <- sapply(strsplit(as.character(positionbook$Timestamp),".",fixed = TRUE), "[[", 1)

ask_prices <- global_tables$ask_price
bid_prices <- global_tables$bid_price
### TESTED

### TESTED
# find the unique stocks that have been traded over the duration 
stock_list <- unique(as.vector(tradesbook$Symbol))

# Set up the data frame which will store the cumulative pnl values for each stock and portfolio 
Pnl_df <- data.frame(Symbol=character(), DateTime=as.Date(character()), BidAskPrice=character(), 
                     BookValue=integer(), Side=integer(), Quantity = integer(),
                     Open.Close = character(), PnLStock = integer(), Cash = integer(),
                     Portfolio = integer(), PnLPortfolio = integer())
### TESTED

### TESTED
# loop through all the stocks that have been traded 
for (i in 1:length(stock_list)){
  stock_name <- stock_list[i] 
  # filter the tradesbook for only the selected stock 
  filtered_tradesbook <- subset(tradesbook, Symbol == stock_name)
  
  # loop through all the trades made in the tradesbook
  for (j in 1:nrow(filtered_tradesbook)){
    curr_side <- filtered_tradesbook$Side[j]
    curr_pos <- filtered_tradesbook$`Open/Close`[j]
    start_time_date_unix <- as.character(filtered_tradesbook[j, "Timestamp"])
    end_time_date_unix <- as.character(filtered_tradesbook[j + 1, "Timestamp"])
    start_time_date <- as.POSIXct(as.numeric(filtered_tradesbook[j, "Timestamp"]), origin="1970-01-01")
    end_time_date <- as.POSIXct(as.numeric(filtered_tradesbook[j + 1, "Timestamp"]), origin="1970-01-01")
    book_value <- positionbook[positionbook$Timestamp == start_time_date_unix & positionbook$Symbol == stock_name, ]["BookValue"]
    qty_owned <- positionbook[positionbook$Timestamp == start_time_date_unix & positionbook$Symbol == stock_name, ]["Quantity"]
    
      if (curr_side == 1 && curr_pos == "Open"){
        # add part in future if trading more than one stock, must access the correct market data
        # depending on what the value of stock_name is 
        stock_data <- paste(stock_name,EquityList[2],sep="_")
        stock_matrix <- env[[stock_data]]
        type <- "bid"
      } else if (curr_side == 2 && curr_pos == "Open"){
        stock_data <- paste(stock_name,EquityList[3],sep="_")
        stock_matrix <- env[[stock_data]]
        type <- "ask"
      } else if (curr_pos == "Close") {
        next
      }
      
      for (i in 1:NROW(stock_matrix)){
        if (stock_matrix[i, "Date"] > end_time_date) {
          break
        } else if (stock_matrix[i,"Date"] >= start_time_date){
          # must clarify what price would we realize profits at (i.e what is market value at a given time)
          # add part in future if trading more than one stock, must access the correct market data
          # depending on what the value of stock_name is 
          if (type == "ask") {
            bidask_price <- ask_prices[i, "LOW"]
          } else {
            bidask_price <- bid_prices[i, "HIGH"]
          }
          if (stock_matrix[i, "Date"] == end_time_date) {
            curr_pos <- "Close"
            if (curr_side == 1){
              curr_side == 2
            } else {
              curr_side == 1
            }
          }
          Pnl_df <- rbind(Pnl_df, data.frame(Symbol = stock_name, DateTime = stock_matrix[i, "Date"],
                                             BidAskPrice = bidask_price, 
                                             BookValue = book_value, Side = curr_side,
                                             Quantity = qty_owned, OpenClose = curr_pos))
        }
      }
      

  }
  
}
### TESTED

### TESTED
# calculate the cumulative PnL of each stock 

for (i in 1:NROW(Pnl_df)){
  curr_symbol <- Pnl_df[i, "Symbol"]
  prev_symbol <- Pnl_df[i-1, "Symbol"]
  curr_side <- Pnl_df[i, "Side"]
  
  # multiply the book value by -1 to obtain a posititve quantity
  if (curr_side == 1){
    curr_pnl <- (Pnl_df[i, "BidAskPrice"] - Pnl_df[i, "BookValue"])*Pnl_df[i, "Quantity"]
  } else {
    book_value <- Pnl_df[i, "BookValue"]*-1
    short_qty <- Pnl_df[i, "Quantity"]*-1
    curr_pnl <- (Pnl_df[i, "BidAskPrice"] - book_value*short_qty)*-1
  }
  
  # find cum. value. Ensure that the cumulative value is only carried from previous if it was the same stock
  if (i == 1) {
    Pnl_df[i, "PnLStock"] <- curr_pnl
  } else {
    if (curr_symbol == prev_symbol){
      prev_pnl <- Pnl_df[i-1, "PnLStock"]
    } else {
      prev_pnl <- 0
    }
    Pnl_df[i, "PnLStock"] <- prev_pnl + curr_pnl
  }
}
### TESTED

### TESTED
# order the Pnl data frame in order of date/time since it is sectioned by symbol

Pnl_df <- Pnl_df[order(Pnl_df$DateTime, decreasing = FALSE), ]

# record the cash in the account at each time step, pulling from the positionbook. 

for (i in 1:NROW(Pnl_df)){
  curr_date_time <- Pnl_df[i, "DateTime"]
  
  # start for loop on second element since the first is at time 0 and records the default cash position
  for (j in 2:NROW(positionbook)){
    if (as.POSIXct(as.numeric(positionbook[j, "Timestamp"]), origin="1970-01-01") == curr_date_time && positionbook[j, "Symbol"] == "Cash"){
      Pnl_df[i, "Cash"] <- positionbook[j, "BookValue"]
    } 
  }
  # if there is no datetime match in the positions matrix, this means that there was no position
  # taken at the time and therefore cash is just equal to the previous time step (no change)
  if (is.na(Pnl_df[i, "Cash"])){
    Pnl_df[i, "Cash"] <- Pnl_df[i-1, "Cash"]
  } 
}
### TESTED

### TESTED
# find the unique times in which some stock was owned
time_list <- as.data.frame(unique(Pnl_df$DateTime))
# change name of column header
names(time_list)[1] <- "DateTime"

# calculate portfolio values at each unique time step 
for (i in 1:NROW(time_list)){
  filtered_times <- subset(Pnl_df, DateTime == time_list[i,1])
  liquidated_positions <- 0
  for (j in 1:NROW(filtered_times)){
    liquidated_positions <- filtered_times[j, "BidAskPrice"]*(as.numeric(filtered_times[j, "Quantity"])) + liquidated_positions
    if (j == NROW(filtered_times)){
      # if (filtered_times[j, "Side"] == 1){
        time_list[i, "PortfolioValue"] <- filtered_times[j, "Cash"] + liquidated_positions
      # } else if(filtered_times[j, "Side"] == 2) {
        # time_list[i, "PortfolioValue"] <- filtered_times[j, "Cash"] - liquidated_positions
      # }
    }
  }
}
### TESTED

### TESTED
# calculate cumulative portfolio pnl at each time step

for (i in 1:NROW(time_list)){
  curr_pnl <- time_list[i, "PortfolioValue"] - init_cash
  time_list[i, "PnLPortfolio"] <- curr_pnl
}

dt <- c(0, time_list[, "PnLPortfolio"])
time_list[, "CumPnLPortfolio"] <- cumsum(time_list[, "PnLPortfolio"])

### TESTED

### TESTED
# calculate the final cumulative PnL of the portfolio 

cumulative_pnl_portfolio <- time_list[NROW(time_list), "PnLPortfolio"]

### TESTED

### TESTED
# calculate the final cumulative PnL of each stock traded

cumulative_pnl_dataframe <- data.frame(Symbol = as.character(), CumPnL = as.integer())

for (i in 1:length(stock_list)){
  stock_name <- stock_list[i]  
  temp_matrix <- subset(Pnl_df, Symbol == stock_name)
  cum_pnl <- temp_matrix[NROW(temp_matrix), "PnLStock"]
  cumulative_pnl_dataframe <- rbind(cumulative_pnl_dataframe, data.frame(Symbol = stock_name,
                                                                         CumPnL = cum_pnl))
}

### TESTED

### TESTED
# calculate the number of trades per day (trade is considered any time a position is opened or closed)

Dates_Traded <- as.Date(strftime(as.POSIXct(as.numeric(tradesbook$Timestamp), origin="1970-01-01"), "%Y-%m-%d"))
Unique_Dates_Traded <- seq(from = min(Dates_Traded), to = max(Dates_Traded), by = "day")
Trade.count <- sapply(Unique_Dates_Traded, FUN = function(X) sum(Dates_Traded == X))
Trades_distribution <- data.frame(Day = Unique_Dates_Traded, TradeCount = Trade.count)
### TESTED

### TESTED
# calculate Portfolio PnL per day

PnL_distribution <- data.frame(Date = as.Date(as.character()), PortfolioOpen = as.integer(),
                               PortfolioClose = as.integer(), PnL = as.integer())
for (i in 1:length(Unique_Dates_Traded)){
  temp_matrix <- subset(time_list, as.Date(time_list$DateTime) == Unique_Dates_Traded[i])
  Portfolio_Open <- temp_matrix[1, "PnLPortfolio"]
  Portfolio_Close <- temp_matrix[NROW(na.omit(temp_matrix)), "PnLPortfolio"]
  PnL <- ((Portfolio_Close - Portfolio_Open) / (Portfolio_Open))
  PnL_distribution <- rbind(PnL_distribution, data.frame(Date = Unique_Dates_Traded[i],
                                                         PortfolioOpen = Portfolio_Open,
                                                         PortfolioClose = Portfolio_Close,
                                                         PnL = PnL))
}
### TESTED

### TESTED
# calculate the average daily Portfolio PnL 

average_daily_PnL <- mean(PnL_distribution$PnL)
### TESTED

### TESTED
# calculate the percent days portfolio is profitable

percent_profitable_days <- (sum(PnL_distribution$PnL > 0)/NROW(PnL_distribution))*100
### TESTED

### TESTED
# calculate the number of trades in total

total_trades <- NROW(tradesbook)
### TESTED

### TESTED
# calculate the average number of trades per day 

average_daily_trades <- mean(Trades_distribution$TradeCount)
### TESTED

### TESTED
# calculate the average PnL of all trades

filter_pnL <- subset(tradesbook, PnL != "")
average_PnL_all_trades <- mean(as.numeric(filter_pnL$PnL))
### TESTED

### TESTED
# calculate the average PnL per trade by stock 

average_pnl_dataframe <- data.frame(Symbol = as.character(), AveragePnLPerTrade = as.integer())

for (i in 1:length(stock_list)){
  stock_name <- stock_list[i] 
  temp_matrix <- subset(tradesbook, Symbol == stock_name)
  avg_pnl <- mean(as.numeric(subset(temp_matrix, PnL != "")$PnL))
  average_pnl_dataframe <- rbind(average_pnl_dataframe, data.frame(Symbol = stock_name,
                                                                   AveragePnLPerTrade = avg_pnl))
}
### TESTED

### TESTED
# calculate the percent of trades that are profitable (percent of closed positions that are proftiable)

percent_trades_profitable <- (sum(filter_pnL$PnL>0)/NROW(filter_pnL))*100
### TESTED

### TESTED
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
### TESTED

# calculate annualised portfolio return. Need to multiply individual stock returns by their weights 



# calculate annualised portfolio return. Need to multiply covariances by their weights 


# calculate Sharpe Ratio. Need to access the market dataframe whih stores data of the S&P500

market_init_price <- marketdata[which(marketdata$Date==init_day), "Date"]
market_close_price <- marketdata[which(marketdata$Date==end_day), "Date"]
market_return <- ((market_close_price - market_init_price)/market_init_price)*100
sharpe_ratio <- (return1 - market_return)/stdev

### TESTED
# distribution of cumulative PnL of each stock 

for (i in 1:length(stock_list)){
  stock_name <- stock_list[i] 
  temp_matrix <- subset(Pnl_df, Symbol == stock_name)
  temp_plot <- plot(temp_matrix$DateTime, temp_matrix$PnLStock, main=paste(stock_name, "cumulative PnL"),
                    xlab="Days", ylab="Cumulative PnL (%)", type = "l")
}
### TESTED

### TESTED
# distribution of cumulative PnL of Portfolio

cum_pnl_portfolio_plot <- plot(time_list$DateTime, time_list$PnLPortfolio, main="Cumulative PnL of Portfolio",
                               xlab="Days", ylab="Cumulative PnL (%)", type = "l")
### TESTED

### TESTED
# distribution of trades per day 

trades_per_day_plot <- plot(Trades_distribution$Day, Trades_distribution$TradeCount, 
                            main="Trades Per Day", xlab="Days", 
                            ylab="Number of Trades")
### TESTED

### TESTED
# distribution of PnL per day 

PnL_per_day_plot <- plot(PnL_distribution$Date, PnL_distribution$PnL, 
                         main="PnL Per Day", xlab="Days", 
                         ylab="PnL (%)")
### TESTED

### TESTED
# distribution of PnL per trade ($ per share)

PnL_per_day_plot <- plot(as.POSIXct(as.numeric(filter_pnL$Timestamp), origin="1970-01-01"), filter_pnL$PnL, 
                         main="PnL Per Trade", xlab="Days", 
                         ylab="PnL (%)")
### TESTED

# distribution of PnL per trade ($ per $ invested)

# correlation of cumulative return with market return

### TESTED
# number of stocks in the portfolio 

no_stocks <- length(stock_list)
### TESTED

# summary statistics that pertain to the portfolio

summary_statistics_all <- data.frame(NumberofTradesTotal = total_trades, AvgTradesPerDay = average_daily_trades,
                                     CumPnLPortfolio = cumulative_pnl_portfolio, AvgDailyPnL = average_daily_PnL,
                                     PctDaysProfitable = percent_profitable_days, AvgPnLAllTrades = average_PnL_all_trades,
                                     PctTradesProfitable = percent_trades_profitable, MaxDrawdown = MDD_perc,
                                     MaxDrawdownPeriod = MDD_period, AnnualizedReturn = annualised_return,
                                     AnnualizedStdev = annualised_stdev, NumberofStocksInPortfolio = no_stocks) 

### TESTED
# summary statistics that pertain to the specific stocks

summary_statistics_byStock <- merge(cumulative_pnl_dataframe, average_pnl_dataframe)
### TESTED

# return(summary_statistics_all, summary_statistics_byStock)

#}