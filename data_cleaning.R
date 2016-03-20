
import_data <- function(env, Stocks){

  EquityList <- c("tick", "ask", "bid")

  for (s in Stocks){
    for (suffix in EquityList){
      env[[paste (s,suffix, sep = "_")]] <- read.csv(paste (s,"_",suffix,".csv", sep = ""))
      env[[paste (s,suffix, sep = "_")]]$Date <- as.POSIXct(env[[paste (s,suffix, sep = "_")]]$Date)
    }
  }

  #env[["SPTSX_ask"]] <- read.csv("SPTSX_ask.csv")
  #env[["SPTSX_ask"]]$Date <- as.POSIXct(env[["SPTSX_ask"]]$Date)
  #env[["SPTSX_bid"]] <- read.csv("SPTSX_bid.csv")
  #env[["SPTSX_bid"]]$Date <- as.POSIXct(env[["SPTSX_bid"]]$Date)
  #env[["SPTSX_tick"]] <- read.csv("SPTSX_tick.csv")
  #env[["SPTSX_tick"]]$Date <- as.POSIXct(env[["SPTSX_tick"]]$Date)
  
  env[["SPX_ask"]] <- read.csv("SPX_ask.csv")
  env[["SPX_ask"]]$Date <- as.POSIXct(env[["SPX_ask"]]$Date)
  env[["SPX_bid"]] <- read.csv("SPX_bid.csv")
  env[["SPX_bid"]]$Date <- as.POSIXct(env[["SPX_bid"]]$Date)
  env[["SPX_tick"]] <- read.csv("SPX_tick.csv")
  env[["SPX_tick"]]$Date <- as.POSIXct(env[["SPX_tick"]]$Date)
  

  # removes N/A fields and only keeps times when the market is open 
  for (i in Stocks){
    for (Name in EquityList){
      nm <- paste(i,Name,sep="_")
      Opentime <- as.POSIXct("2000-01-01 09:30:00", tz = "EST")
      Opentime <-strftime(Opentime, format="%H:%M:%S")
      Closetime <- as.POSIXct("2000-01-01 15:59:00", tz = "EST")
      Closetime <-strftime(Closetime, format="%H:%M:%S")
      row_to_keep <- logical(length = nrow(env[[nm]]) )
      maxrow <- nrow(env[[nm]])
      env[[nm]] = env[[nm]][complete.cases(env[[nm]][1:maxrow,] ) ,]
      for (j in 1:nrow(env[[nm]])){ 
        temp <- strftime(env[[nm]][j,1], format="%H:%M:%S")
        if ( temp >= Opentime && temp <= Closetime){
          row_to_keep[j] <- TRUE
        } else{
          row_to_keep[j] <- FALSE
        }
      }
      env[[nm]] = env[[nm]][row_to_keep,]
    }
  }
  
  # fills in missing times with the previous minute's market information 
  
  for (stock in Stocks){
    for (name in EquityList){
      nm <- paste(stock,name,sep="_")
      col_nms <- colnames(env[[nm]])
      list_times_vec <- data.frame()
      env[["list_dates"]] <- as.data.frame(unique(as.Date(env[[nm]][, "Date"])))
      list_dates <- env[["list_dates"]]
      for (i in 1:nrow(list_dates)){
        st <- as.POSIXct(paste(list_dates[i,1], "09:30:00"), origin = "1970-01-01")
        en <- as.POSIXct(paste(list_dates[i,1], "15:59:00"), origin = "1970-01-01")
        list_times_vec <- rbind(list_times_vec, data.frame(seq(from = st, to = en, by = "min")))
      }
      dates_list <- list_times_vec
      mtrix <- matrix(0, nrow(list_times_vec), (ncol(env[[nm]])-1))
      list_times_vec <- cbind(list_times_vec, mtrix)
      colnames(list_times_vec) <- col_nms
      
      testy <- merge(list_times_vec, env[[nm]], by = "Date", all = TRUE)
      testy <- testy[,9:15]
      
      na_rows <- rownames(subset(testy,is.na(testy$OPEN.y)))
      
      for (rw in na_rows){
        rw <- as.integer(rw)
        if (rw == 1){
          if(strftime(env[[nm]][rw,"Date"], format="%H:%M:%S") != "09:30:00"){
            testy[rw,1:4] <- env[[nm]][1,2:5]
            next
          } 
        } else {
          testy[rw,1:4] <- testy[rw-1,1:4]
        }
        
      }
      testy[is.na(testy)] <- 0
      new_data_final <- cbind(dates_list, testy)
      env[[nm]] <- new_data_final
      colnames(env[[nm]]) <- col_nms
    }
  }
  
  # assign each df in the environment to a variable 
  
  for (stock in Stocks){
    for (name in EquityList){
      stock_data <- paste(stock,name,sep="_")
      assign(paste(stock,name,sep="_"), env[[stock_data]])
    }
  }
}


