
obtainthreshold <- function(env, Stocks, startindx, lookback, jump){
  
  longdur = 60
  shortdur = 40
  iterations = 50

  results = data.frame(Date = as.character(), threshold = as.double(), error = as.double(), difference = as.double())

  IPR_df <- data.frame(Date = as.character(), Symbol = as.character(), IPR = as.integer(), Diff = as.double())
  
  #Stocks <- c("AC", "BNS", "BMO")
  EquityList <- c("tick", "ask", "bid")
  
  stock_data <- paste(Stocks[1],EquityList[1],sep="_")
  # Define time period
  starttime = startindx - lookback
  totaltime <- env[[stock_data]][["Date"]][(startindx - lookback):(startindx-1)]
  actiontime <- totaltime[seq(1,lookback,30)] # Times to perform active portion
  actiontime <- actiontime[-c(which(strftime(actiontime, format="%H:%M:%S") == "09:30:00",arr.ind=TRUE))]
  actiontime <- actiontime[-c(which(strftime(actiontime, format="%H:%M:%S") == "10:00:00",arr.ind=TRUE))]
  looprow <- length(actiontime) # Assuming starttime and endtime are times in seconds
  for (j in 1:looprow) {
    IPR_df <- data.frame()
    for (stock in Stocks){
      # Calculate IPR and difference 
      stock_data <- paste(stock,EquityList[1],sep="_")
      tick_data <- env[[stock_data]]
      currtime <- starttime+30*j + 30 ####################### SUPER HARD CODE ALERTTTT ##############
#       P_asterix <- tick_data[currtime-1, "LAST_PRICE"]
#       P_asterix_j_date <-  tick_data[currtime - jump-1, "LAST_PRICE"]
#       start_row <- 1
#       price_estimates <- data.frame(Price = tick_data[starttime:currtime, "LAST_PRICE"])
#       ret1 <- return_and_stdev(price_estimates)$ret
#       stdev <- return_and_stdev(price_estimates)$stdev
#       z <- (log(P_asterix/P_asterix_j_date) - jump * ret1) / (sqrt(jump * stdev))
      weights <- c(1/3,1/3,1/3)
      # IPR <- lowpassfilter(tick_data, currtime, jump, start_row, weights, 30)
      #start1 <- Sys.time()
      IPR <- IPRcalc(tick_data, currtime, jump, starttime)
      diff = (SMA(currtime, shortdur,tick_data$LAST_PRICE) - SMA(currtime, longdur,tick_data$LAST_PRICE))/tick_data$LAST_PRICE[currtime]
      # diff = (SMA(currtime, shortdur,tick_data$LAST_PRICE) - SMA(currtime, longdur,tick_data$LAST_PRICE))
      IPR_df <- rbind(IPR_df, data.frame(Date = actiontime[j], Symbol = stock, IPR = IPR, Diff = diff))
      #print ("IPR Calculation:")
      #print (Sys.time() - start1)
    }
    # Allocate based on allocation function
    threshold <- 0.5
    direct = 1
    prevreturn = Inf
    for (k in 1:iterations){
      returns <- data.frame(Symbol = as.character(), return1 = as.double(), wdiff = as.double())
      currcash = 100000
      pastportvalue = 0 
      futureportvalue = 0
      allocs = data.frame()
      start1 <- Sys.time()
      for (b in 1:nrow(IPR_df)){
        # Determine direction
        stock = as.character(IPR_df$Symbol[b])
        stock_data <- paste(stock,EquityList[1],sep="_")
        tick_data <- env[[stock_data]]
        
        if (IPR_df[b, "IPR"] < threshold){
          cashalloc = -(0.5/threshold)*(IPR_df[b, "IPR"]) + 0.5
        } else {
          cashalloc = -(0.5/(1-threshold))*(IPR_df[b, "IPR"]) + (-0.5*(1+1/(threshold-1)))
        }
        
        allocs = rbind(allocs, data.frame(Symbol = stock, Allocation = cashalloc))
        allocvalue = cashalloc * currcash
        futurequant<- floor(allocvalue/tick_data[currtime,"LAST_PRICE"])
        totalalloc = futurequant*tick_data[currtime,"LAST_PRICE"]
        stockreturn = tick_data[currtime + 30,"LAST_PRICE"]/ tick_data[currtime,"LAST_PRICE"]
        finalreturn = totalalloc * (stockreturn - 1)
        returns <- rbind(returns, data.frame(Symbol = stock, return1 = finalreturn, wdiff = cashalloc * IPR_df[b,"Diff"]))
      }
      #print ("Allocation Calculation")
      #print (Sys.time() - start1)
      bound <- 5
      portreturn = sum(returns["return1"])

      if (abs(sum(returns["return1"])) < bound) {
          results<-rbind(results,data.frame(Date = actiontime[j], threshold = threshold, error = portreturn, difference = sum(returns["wdiff"])))
          break
      } else {
        increment = log(abs(sum(returns["return1"])))/100
        if (abs(prevreturn) - abs(portreturn) > 0) {
          threshold <- threshold + direct * increment
        } else {
          direct = -direct
          threshold <- threshold + direct * increment
        }
        prevreturn = portreturn
      }
      if (k == iterations){
        results<-rbind(results,data.frame(Date = actiontime[j], threshold = threshold, error = portreturn, difference = sum(returns["wdiff"])))
      }
    }
  }
  results <- results[(-c(which(results$difference < (quantile(results$difference)[2] - IQR(results$difference)) | 
                                results$difference > (quantile(results$difference)[4] + IQR(results$difference)),arr.ind=TRUE))),]
  results <- results[(-c(which(abs(results$error) > bound,arr.ind=TRUE))),]
  reg <- lm(results$threshold~results$difference)
  #plot(results$difference, results$threshold)
  #abline(reg)
  return (reg$coefficients)
}