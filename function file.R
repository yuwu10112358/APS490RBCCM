# constants
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

IPR_df <- data.frame(Date = as.character(), Symbol = as.character(), IPR = as.integer())
Stocks <- c("AC", "BNS", "BMO")
EquityList <- c("tick", "ask", "bid")
a <- 1
# only trading every 30 minutes, change this value when trading time is different 
end_a <- nrow(list_dates)*12*30

# loop through each minute (a) but only calculate IPR every 30 minutes  
while (a < end_a){
  print(a)
  for (stock in Stocks){
    stock_data <- paste(stock,EquityList[1],sep="_")
    tick_data <- env[[stock_data]]
    if (strftime(tick_data[a, "Date"], format="%H:%M:%S") == "15:30:00"){
      a <- a + 1
      break
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
      temp_dframe <- subset(IPR_df, Date == "2015-05-13 10:00:00")
      # order the dataframe based on IPR values 
      temp_dframe <- temp_dframe[order(temp_dframe$IPR),]
      # create buy or sell orders
      for (b in length(temp_dframe)){
        if (temp_dframe[b, "IPR"] >= 0.5){
          # assign percentages to each stock for cash allocations
          cash_alloc <- -(temp_dframe[b, "IPR"]) + 0.5
          # send an order to the market with sell and using the existing cash * appropriate Pct
        } else if (temp_dframe[b, "IPR"] < 0.5){
          # assign percentages to each stock for cash allocations
          cash_alloc <- -(temp_dframe[b, "IPR"]) + 0.5
          # send an order to the market with buy and using the existing cash * appropriate Pct
        }
      }
    }
  }
  a <- a + 30
}


