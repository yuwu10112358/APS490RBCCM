stocklist <- c("AC", "BNS", "BMO")
namelist <- c("tick")

# constants
jump <- 13
buy_IPR <- 0.6
sell_IPR <- 0.2


# first day of trading 
start_trade <- which(tick_data$Date == "2015-06-08 09:30:00")
# last day of trading
end_trade <- which(tick_data$Date == "2015-11-24 15:59:00")

for (stock in stocklist){
  # assign each df to a variable for later use 
  for (nme in namelist){
    stock_data <- paste(stock,nme,sep="_")
    assign(paste(stock,nme,sep="-"), global_tables[[stock_data]])
  }
}

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

IPR_df <- data.frame(Date = as.character(), Symbol = as.character(), IPR = as.integer())

# loop through each minute (a) 
for (a in start_date:end_date){
  for (stock in stocklist){
    stock_data <- paste(stock,namelist[1],sep="_")
    tick_data <- env[[stock_data]]
    next_date <- tick_data[a, "Date"] + 1740
    P_asterix <- tick_data[which(tick_data$Date == next_date), "LAST_PRICE"]
    P_asterix_j_date <-  tick_data[which(tick_data$Date == next_date) - jump, "LAST_PRICE"]
    start_row <- 1
    end_row <- which(tick_data$Date == next_date)
    price_estimates <- data.frame(Price = tick_data[start_row:end_row, "LAST_PRICE"])
    ret <- return_and_stdev(price_estimates)$return
    stdev <- return_and_stdev(price_estimates)$stdev
    z <- (log(P_asterix/P_asterix_j_date) - jump * ret) / (sqrt(jump * stdev))
    IPR <- pnorm(z)
    IPR_df <- rbind(IPR_df, data.frame(Date = next_date + 60, Symbol = stock, IPR = z))
    if (stock == stock_list[length(stock_list)]){
      temp_dframe <- subset(IPR_df, Date == next_date)
      # order the dataframe based on IPR values 
      temp_dframe <- temp_dframe[order(temp_dframe$IPR),]
      # assign percentages to each stock for cash allocations (HARD-CODED)
      temp_dframe[1, "Pct"] <- 0.5
      temp_dframe[2, "Pct"] <- 0.3
      temp_dframe[3, "Pct"] <- 0.2
      # create buy or sell orders
      for (b in length(temp_dframe)){
        if (temp_dframe[b, "IPR"] >= buy_IPR){
          # send an order to the market with buy and using the existing cash * appropriate Pct
        } else if (temp_dframe[b, "IPR"] <= sell_IPR){
          # send an order to the market with sell and using the existing cash * appropriate Pct
        }
      }
    }
  }
  a <- a + 29
}


  
  



