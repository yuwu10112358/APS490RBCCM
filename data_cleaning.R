
import_data <- function(env){
  env[["AC_ask"]] <- read.csv("AC_ask.csv")
  env[["AC_ask"]]$Date <- as.POSIXct(env[["AC_ask"]]$Date)
  env[["AC_bid"]] <- read.csv("AC_bid.csv")
  env[["AC_bid"]]$Date <- as.POSIXct(env[["AC_bid"]]$Date)
  env[["AC_tick"]] <- read.csv("AC_tick.csv")
  env[["AC_tick"]]$Date <- as.POSIXct(env[["AC_tick"]]$Date)
  
  env[["BNS_ask"]] <- read.csv("BNS_ask.csv")
  env[["BNS_ask"]]$Date <- as.POSIXct(env[["BNS_ask"]]$Date)
  env[["BNS_bid"]] <- read.csv("BNS_bid.csv")
  env[["BNS_bid"]]$Date <- as.POSIXct(env[["BNS_bid"]]$Date)
  env[["BNS_tick"]] <- read.csv("BNS_tick.csv")
  env[["BNS_tick"]]$Date <- as.POSIXct(env[["BNS_tick"]]$Date)
  
  env[["BMO_ask"]] <- read.csv("BMO_ask.csv")
  env[["BMO_ask"]]$Date <- as.POSIXct(env[["BMO_ask"]]$Date)
  env[["BMO_bid"]] <- read.csv("BMO_bid.csv")
  env[["BMO_bid"]]$Date <- as.POSIXct(env[["BMO_bid"]]$Date)
  env[["BMO_tick"]] <- read.csv("BMO_tick.csv")
  env[["BMO_tick"]]$Date <- as.POSIXct(env[["BMO_tick"]]$Date)
  
  Stocks <- c("AC", "BNS", "BMO")
  EquityList <- c("tick", "ask", "bid")
  
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
        testy[rw,1:4] <- testy[rw-1,1:4]
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


