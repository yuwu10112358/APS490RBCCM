# output <- function(tradesbook, positionbook, ask_prices, bid_prices, market_data){

# note to edit portfolio values for shorts
# since I don't have access to the real dataframes, I created them manually
tradesbook <- read.csv("tradesbook.csv", header = TRUE)
positionbook <- read.csv("positionbook.csv", header = TRUE)

# tradesbook <- global_tables$tradesbook
# positionbook <- global_tables$positionbook

# the following are created because the file that data_extraction pulls only carries
# one day's worth of data and I need multiple days. These variables are for testing
# purposes only
ask_prices <- read.csv("ask_AAPL.csv", header = TRUE)
bid_prices <- read.csv("bid_AAPL.csv", header = TRUE)

# ask_prices <- global_tables$ask_price
# bid_prices <- global_tables$bid_price

# convert to date formats
tradesbook$Timestamp <- as.POSIXct(tradesbook$Timestamp)
positionbook$Timestamp <- as.POSIXct(positionbook$Timestamp)

ask_prices$Date <- as.POSIXct(ask_prices$Date)
bid_prices$Date <- as.POSIXct(bid_prices$Date)

# find the unique stocks that have been traded over the duration 
stock_list <- unique(as.vector(tradesbook$Symbol))

# Set up the data frame which will store the cumulative pnl values for each stock and portfolio 
Pnl_df <- data.frame(Symbol=character(), DateTime=as.Date(character()), BidAskPrice=character(), 
                     BookValue=integer(), Side=integer(), QuantityOwned = integer(),
                     Open.Close = character(), PnLStock = integer(), Cash = integer(),
                     Portfolio = integer(), PnLPortfolio = integer())

# loop through all the stocks that have been traded 
for (i in 1:length(stock_list)){
  stock_name <- stock_list[i]  
  filtered_position <- subset(positionbook, Symbol == stock_name)
  filtered_trades <- subset(tradesbook, Symbol == stock_name)
  # 1. filter the position and trades table and 2. merge the two because data will be needed
  # from both tables
  position_trades <- cbind(filtered_position, filtered_trades)

  for (j in 1:NROW(position_trades)){
    curr_side <- position_trades[j, "Side"]
    curr_pos <- position_trades[j, "Open.Close"]
    book_value <- position_trades[j, "BookValue"]
    start_time_date <- position_trades[j, "Timestamp"]
    end_time_date <- position_trades[j + 1, "Timestamp"]
    qty_owned <- position_trades[j, "Quantity.Owned"]
    
    # check if there is quantity of stock owned, if there is then go to the ask/bid price
    # data frames to access the market values for each time period 
    if (qty_owned > 0){
      if ((curr_side == 1 && curr_pos == "Open") || (curr_side == 1 && curr_pos == "Close")){
        # add part in future if trading more than one stock, must access the correct market data
        # depending on what the value of stock_name is 
        # This should be used in real scenarios stock_matrix <- global_tables$ask_price
        stock_matrix <- ask_prices
        type <- "bid"
      } else if ((curr_side == -1 && curr_pos == "Open") || (curr_side == -1 && curr_pos == "Close")){
        # This should be used in real scenarios stock_matrix <- global_tables$bid_price
        stock_matrix <- bid_prices
        type <- "ask"
      }
      for (i in 1:NROW(stock_matrix)){
        if (stock_matrix[i, "Date"] == end_time_date) {
          break
        } else if (stock_matrix[i,"Date"] >= start_time_date){
          # must clarify what price would we realize profits at (i.e what is market value at a given time)
          if (type == "ask") {
            bidask_price <- stock_matrix[i, "LOW"]
          } else {
            bidask_price <- stock_matrix[i, "HIGH"]
          }
          Pnl_df <- rbind(Pnl_df, data.frame(Symbol = stock_name, DateTime = stock_matrix[i, "Date"],
                                             BidAskPrice = bidask_price, 
                                             BookValue = book_value, Side = curr_side,
                                             Quantity = qty_owned, OpenClose = curr_pos))
        }
      }
    } else {
      next
    } 
  }
}

# calculate the cumulative PnL of each stock 

for (i in 1:NROW(Pnl_df)){
  curr_symbol <- Pnl_df[i, "Symbol"]
  prev_symbol <- Pnl_df[i-1, "Symbol"]
  curr_pnl <- (((Pnl_df[i, "BidAskPrice"]-Pnl_df[i, "BookValue"]) / (Pnl_df[i, "BookValue"]))) * 100
  if (i ==1) {
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

# order the Pnl data frame in order of date/time since it is sectioned by symbol

Pnl_df <- Pnl_df[order(Pnl_df$DateTime, decreasing = FALSE), ]

# record the cash in the account at each time step, pulling from the positionbook. 

for (i in 1:NROW(Pnl_df)){
  curr_date_time <- Pnl_df[i, "DateTime"]
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

# find the unique times in which some stock was owned
time_list <- as.data.frame(unique(Pnl_df$DateTime))
# change name of column header
names(time_list)[1] <- "DateTime"

# calculate portfolio values at each unique time step 
for (i in 1:NROW(time_list)){
  filtered_times <- subset(Pnl_df, DateTime == time_list[i,1])
  liquidated_positions <- 0
  for (j in 1:NROW(filtered_times)){
    liquidated_positions <- filtered_times[j, "BidAskPrice"]*filtered_times[j, "Quantity"] + liquidated_positions
    if (j == NROW(filtered_times)){
      if (filtered_times[j, "Side"] == 1){
        time_list[i, "PortfolioValue"] <- filtered_times[j, "Cash"] + liquidated_positions
      } else if(filtered_times[j, "Side"] == 2) {
        time_list[i, "PortfolioValue"] <- liquidated_positions - filtered_times[j, "Cash"]
      }
    }
  }
}

# calculate the portfolio value (non-realized) and cumulative portfolio pnl at each time step

for (i in 1:NROW(Pnl_df)){
  if (Pnl_df[i, "Side"] == 1){
    Pnl_df[i, "Portfolio"] <- Pnl_df[i, "Cash"] + (Pnl_df[i, "BidAskPrice"]*Pnl_df[i, "QuantityOwned"]) 
  } else {
    Pnl_df[i, "Portfolio"] <- Pnl_df[i, "Cash"] - (Pnl_df[i, "BidAskPrice"]*Pnl_df[i, "QuantityOwned"])
  }
  
  # must clarify formula for cumulative PnL
  if (i == 1){
    prev_pnl <- 0
    curr_pnl <- ((Pnl_df[i, "Portfolio"] - init_cash)/init_cash)*100
    Pnl_df[i, "PnLPortfolio"] <- prev_pnl + curr_pnl
  } else {
    prev_pnl <- Pnl_df[i-1, "PnLPortfolio"]
    curr_pnl <- ((Pnl_df[i, "Portfolio"] - Pnl_df[i - 1, "Portfolio"])/Pnl_df[i - 1, "Portfolio"]*100)
    Pnl_df[i, "PnLPortfolio"] <-  prev_pnl + curr_pnl
  }
}

# calculate the final cumulative PnL of the portfolio 

cumulative_pnl_portfolio <- Pnl_df[NROW(Pnl_df), "PnLPortfolio"]

# calculate the final cumulative PnL of each stock traded

cumulative_pnl_dataframe <- data.frame(Symbol = as.character(), CumPnL = as.integer())

for (i in 1:length(stock_list)){
  stock_name <- stock_list[i]  
  temp_matrix <- subset(Pnl_df, Symbol == stock_name)
  cum_pnl <- temp_matrix[NROW(temp_matrix), "PnLStock"]
  cumulative_pnl_dataframe <- rbind(cumulative_pnl_dataframe, data.frame(Symbol = stock_name,
                                                                         CumPnL = cum_pnl))
}

# calculate the number of trades per day 

Dates_Traded <- as.Date(strftime(tradesbook$Timestamp, "%Y-%m-%d"))
Unique_Dates_Traded <- seq(from = min(Dates_Traded), to = max(Dates_Traded), by = "day")
Trade.count <- sapply(Unique_Dates_Traded, FUN = function(X) sum(Dates_Traded == X))
Trades_distribution <- data.frame(Day = Unique_Dates_Traded, TradeCount = Trade.count)

# calculate PnL per day

PnL_distribution <- data.frame(Date = as.Date(as.character()), PortfolioOpen = as.integer(),
                               PortfolioClose = as.integer(), PnL = as.integer())
for (i in 1:length(Unique_Dates_Traded)){
  temp_matrix <- subset(Pnl_df, as.Date(Pnl_df$DateTime) == Unique_Dates_Traded[i])
  Portfolio_Open <- temp_matrix[1, "Portfolio"]
  Portfolio_Close <- temp_matrix[NROW(na.omit(temp_matrix)), "Portfolio"]
  PnL <- 100*((Portfolio_Close - Portfolio_Open) / (Portfolio_Open))
  PnL_distribution <- rbind(PnL_distribution, data.frame(Date = Unique_Dates_Traded[i],
                                                         PortfolioOpen = Portfolio_Open,
                                                         PortfolioClose = Portfolio_Close,
                                                         PnL = PnL))
}

# calculate the average daily PnL 

average_daily_PnL <- mean(PnL_distribution$PnL)

# calculate the percent days profitable

percent_profitable_days <- (sum(PnL_distribution$PnL > 0)/NROW(PnL_distribution))*100

# calculate the number of trades in total

total_trades <- NROW(tradesbook)

# calculate the average number of trades per day 

average_daily_trades <- mean(Trades_distribution$TradeCount)

# calculate the average PnL of all trades

average_PnL_all_trades <- mean(filter_pnL$PnL)

# calculate the average PnL per trade by stock 

average_pnl_dataframe <- data.frame(Symbol = as.character(), AveragePnLPerTrade = as.integer())

for (i in 1:length(stock_list)){
  stock_name <- stock_list[i] 
  temp_matrix <- subset(tradesbook, Symbol == stock_name)
  avg_pnl <- mean(subset(temp_matrix, PnL != "")$PnL)
  average_pnl_dataframe <- rbind(average_pnl_dataframe, data.frame(Symbol = stock_name,
                                                                   AveragePnLPerTrade = avg_pnl))
}

# calculate the percent of trades that are profitable. Here, I am considering a "trade" to be
# only when a position is closed. This is inconsistent from my previous definition of "trade"
# when I calculated number of trades. There, I considered a "trade" to be any time a position
# was open or closed. Definition must be clarified

percent_trades_profitable <- (sum(filter_pnL$PnL>0)/NROW(filter_pnL))*100

# calculate maximum drawdown and maximum drawdown period. Need to confirm that this is the 
# correct way to calculate max drawdown 

maximum_drawdown <- 0
for (i in 1:NROW(Pnl_df)){
  if (i == 1){
    drawdown <- 0
  } else {
    max_date <- Pnl_df[which.max(Pnl_df$Portfolio[1:i-1]), "DateTime"]
    drawdown <- ((Pnl_df[i, "Portfolio"]/max(Pnl_df$Portfolio[1:i-1])) - 1)*100
    if (drawdown < maximum_drawdown){
      maximum_drawdown <- drawdown
      maximum_drawdown_period <- Pnl_df[i, "DateTime"] - max_date
    }
  }
}

# calculate annualised return. Need to experiment more with time series data. Need to know
# how to tell whether a time difference is in minutes, days, months, etc. Also need more
# clarification how to calculate. 

# assume time difference is in years

days_in_year <- 365

init_cash <- init_cash
end_cash <- Pnl_df[NROW(Pnl_df), "Cash"]
return1 <- ((end_cash - init_cash)/(init_cash))*100

init_day <- Pnl_df[1, "DateTime"]
end_day <- Pnl_df[NROW(Pnl_df), "DateTime"]
holding_period <- end_day - init_day

annualised_return <- (days_in_year * (return1))/as.integer(holding_period)

# calculate annualised standard deviation. Need more clarification how to calculate 

stdev <- sd(Pnl_df$Portfolio)
annualised_stdev <- (days_in_year * (stdev))/as.integer(holding_period)

# calculate Sharpe Ratio. Access the market dataframe whih stores data of the S&P500. Need
# to verify calculation.Risk-adjusted risk, Return per unit of risk 

market_init_price <- marketdata[which(marketdata$Date==init_day), "Date"]
market_close_price <- marketdata[which(marketdata$Date==end_day), "Date"]
market_return <- ((market_close_price - market_init_price)/market_init_price)*100
sharpe_ratio <- (return1 - market_return)/stdev

# combine all the single summary metrics into a summary statistics tables

# the followng are summary statistics that pertain to the portfolio. Sharpe ratio not includes,
# must verify the data first. 

summary_statistics_all <- data.frame(NumberofTradesTotal = total_trades, AvgTradesPerDay = average_daily_trades,
                                 CumPnLPortfolio = cumulative_pnl_portfolio, AvgDailyPnL = average_daily_PnL,
                                 PctDaysProfitable = percent_profitable_days, AvgPnLAllTrades = average_PnL_all_trades,
                                 PctTradesProfitable = percent_trades_profitable, MaxDrawdown = maximum_drawdown,
                                 MaxDrawdownPeriod = maximum_drawdown_period, AnnualizedReturn = annualised_return,
                                 AnnualizedStdev = annualised_stdev) 

# the following are summary statistics that pertain to the specific stocks

summary_statistics_byStock <- merge(cumulative_pnl_dataframe, average_pnl_dataframe)

# plot graphs for all the distributions calculated above 

# distribution of cumulative PnL of each stock 

for (i in 1:length(stock_list)){
  stock_name <- stock_list[i] 
  temp_matrix <- subset(Pnl_df, Symbol == stock_name)
  file_name <- paste(stock_name, "cumulativePnL.pdf", sep = "")
  pdf(file_name)
  temp_plot <- plot(temp_matrix$DateTime, temp_matrix$PnLStock, main=paste(stock_name, "cumulative PnL"),
                    xlab="Days", ylab="Cumulative PnL (%)", type = "l")
  dev.off()
}

# distribution of cumulative PnL of Portfolio

pdf("cumulativePnLPortfolio.pdf")
cum_pnl_portfolio_plot <- plot(Pnl_df$DateTime, Pnl_df$PnLPortfolio, main="Cumulative PnL of Portfolio",
                               xlab="Days", ylab="Cumulative PnL (%)", type = "l")
dev.off()

# use R markdown, knitr

# distribution of trades per day 

pdf("TradesPerDay.pdf")
trades_per_day_plot <- plot(Trades_distribution$Day, Trades_distribution$TradeCount, 
                            main="Trades Per Day", xlab="Days", 
                            ylab="Number of Trades", type = "l")
dev.off()

# distribution of PnL per day 

pdf("PnLPerDay.pdf")
PnL_per_day_plot <- plot(PnL_distribution$Date, PnL_distribution$PnL, 
                            main="PnL Per Day", xlab="Days", 
                            ylab="PnL (%)", type = "l")
dev.off()

# distribution of PnL per trade ($ per share)

filter_pnL <- subset(tradesbook, PnL != "")
pdf("PnLPerTrade.pdf")
PnL_per_day_plot <- plot(filter_pnL$Timestamp, filter_pnL$PnL, 
                         main="PnL Per Trade", xlab="Days", 
                         ylab="PnL (%)", type = "l")
dev.off()

# distribution of PnL per trade ($ per $ invested). Need to clarify

# correlation of cumulative return with market return.Need to clarify 
# add number of stocks in the portfolio 

# return(summary_statistics_all, summary_statistics_byStock)

#}