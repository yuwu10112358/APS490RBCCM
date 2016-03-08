source('hmm_v2.r')
source('constants.r')
source('backtest_lib.r')
test_HMMM <- function (env, symbol, time_interval, num_states){
#     env <- global_tables
#     symbol = "BNS"
#     time_interval = 5
#     num_states = 4
    
  datatable_name <- paste(symbol, Con_Data_Tick_Suffix, sep = "")
  timestamp <- env[[datatable_name]][[Con_Data_ColName_Date]]
  value <- env[[datatable_name]][[Con_Data_ColName_Value]]
  volume <- env[[datatable_name]][[Con_Data_ColName_Volume]]
  
  #timestamp converted into integer represents seconds
  #9:30 EDT mod 86400 = 48600, 9:30 EST mod 86400 = 48600+ 3600
  #trading day is 23400s long
  
  
  #split the data into days
  #assume there is always data at 15:59
  
  cat('Retrieving data \n')
  time_since_open <- rep(0, length(timestamp))
  num_of_minutes <- 390
   
  
  for (i in 1: length(timestamp))
      time_since_open[i] <- get_time_since_open(timestamp[i])
    
  N <- sum(time_since_open == 0)
  start_of_day_indices <- (1:length(timestamp))[time_since_open == 0]
  num_of_lines_per_day <- start_of_day_indices[2] - start_of_day_indices[1]
  
  value_by_day <- matrix(-1, nrow = N, ncol = num_of_minutes)
  volume_by_day <- matrix(-1, nrow = N, ncol = num_of_minutes)
  
  
  for (i in 1:N){
    value_by_day[i,] = value[(i - 1) * num_of_lines_per_day + (1:num_of_minutes)]
    volume_by_day[i,] = volume[(i - 1) * num_of_lines_per_day + (1:num_of_minutes)]
  }
  
  #time_interval = 5 #denote minutes
  
  Tnum = 23400 / (time_interval * 60)
  t_marker <- (time_interval * 60) * (1: Tnum)
  
  VWAP <- matrix(0, nrow = N, ncol = Tnum + 1)
  interval_volume = matrix(0, nrow = N, ncol = Tnum)
  VWAP[,1] = env[[datatable_name]][[Con_Data_ColName_Open]][time_since_open == 0]
  for (n in 1:N){
    interval_volume[n,] <- colSums(matrix(volume_by_day[n,], nrow = time_interval, ncol = Tnum))
    VWAP[n,2:(Tnum + 1)] <- colSums(matrix(value_by_day[n,], nrow = time_interval, ncol = Tnum)) / interval_volume[n,]
    #correct NaN prices. This is caused by the interval volume being 0. In this case VWAP is the same 
    #as the last time period.
    for(t in 2:(Tnum + 1)){
      if (is.nan(VWAP[n, t])){
        VWAP[n, t] = VWAP[n, t - 1]
      }
    }
  }
  
  
  p_increments <- VWAP[,2:(Tnum + 1)] - VWAP[,1:Tnum]
  
  # remove outliers 
  
  training_days <- c(1:60)
  
  #qqnorm(as.vector(log(interval_volume[1:60,])))
  p_increments_training <- p_increments[training_days,]
  volume_training <- interval_volume[training_days,]
  num_var <- 2
  cat('get parameter estimates \n')
  
  estimates <- get_params_estimates(p_increments_training, volume_training, num_var, num_states)
  
  A <- matrix(data.matrix(estimates[["A"]]), nrow = num_states, ncol = num_states)
  mu <- data.matrix(estimates[["mu"]])
  cov_mat <- array(data.matrix(estimates[["cov_matrix"]]), c(num_var, num_var, num_states))
  
  
  start_time <- timestamp[1]
  start_time <- as.POSIXct('2015-05-13 9:30:00 EDT')
  end_time <- as.POSIXct('2015-09-18 15:59:00 EDT')
  
  cat('run performance testing \n')
  system.time({predictions <- performance_test(start_time, end_time, env, symbol, time_interval, num_states, num_var, A, mu, cov_mat)})
  actual_directions <- matrix(NA, N, Tnum)
  for (i in 1:N){
    for (t in 1:Tnum){
      if (p_increments[i,t] > 0){
        actual_directions[i,t] = TRUE
      }
      else if(p_increments[i,t] < 0){
        actual_directions[i,t] = FALSE
      }
      else{
        
      }
    }
  }
  
  comparison <- predictions[,2:Tnum] == actual_directions[1:nrow(predictions),2:Tnum]
  return(sum(comparison[!is.na(comparison)]) / (nrow(comparison) * ncol(comparison)))
}

get_params_estimates <- function (p_increments_training, volume_training, num_var, num_states)
{

  remove_pct <- 0.05
  
  Tnum <- ncol(p_increments_training)
  
  p_inc_filter <- quantile(as.vector(p_increments_training), c(remove_pct * (1/Tnum), 1 - remove_pct * (1/Tnum)))
  vol_filter <- quantile(as.vector(volume_training), c(remove_pct * (1/Tnum), 1 - remove_pct * (1/Tnum)))
    
  rows_to_remove <- (apply(p_increments_training, 1, max) > p_inc_filter[2]) | 
                                    (apply(p_increments_training, 1, min) < p_inc_filter[1])
   
  rows_to_remove2 <- (apply(volume_training, 1, max) > vol_filter[2]) |
                                    (apply(volume_training, 1, min) < vol_filter[1])
  p_increments_aft_filter <- p_increments_training[!(rows_to_remove | rows_to_remove2),]
  volume_aft_filter <- volume_training[!(rows_to_remove | rows_to_remove2),]
  
  rand_size = nrow(p_increments_aft_filter)
  num_estimates <- 1
  
  #order of state parameters: p_increments then log volume
  #num_estimates is always the last dimension
  
  A_estimates <- array(0, c(num_states, num_states, num_estimates))
  mu_estimates <- array(0, c(num_var, num_states, num_estimates))
  cov_mat_estimates <- array(0, c(num_var, num_var, num_states, num_estimates))
  
  for (i in 1: num_estimates){
    tr_indices <- sample(1:nrow(p_increments_aft_filter), rand_size)
    test_data = array(0, c(num_var, Tnum, rand_size))
    test_data[1,,] <- t(p_increments_aft_filter[tr_indices,])
    test_data[2,,] <- t(log(volume_aft_filter[tr_indices,] + 1))
    estimate <- EM(test_data, num_states)
    A_estimates[,,i] <- data.matrix(estimate[["A"]])
    mu_estimates[,,i] <- data.matrix(estimate[["mu"]])
    cov_mat_estimates[,,,i] <- array(data.matrix(estimate[["covariance matrix"]]), c(num_var, num_var, num_states))
  }
  
  rtn <- list(data.frame(A_estimates), data.frame(mu_estimates),
              data.frame(cov_mat_estimates))
  names(rtn) <- c('A', 'mu', 'cov_matrix')
  return(rtn)
}

performance_test <- function(start_time, end_time, env, symbol, time_interval, num_states, num_var, A, mu, cov_mat){

  #if for a symbol the quotes at a particular time is not available, then
  #it is filled in by the data of the last available minute (e.g, if at 9:33 the data
  #is missing, but at 9:32 it is available, then the quote at 9:32 is taken to be the same
  # as the quote of 9:32)
  
  #strategy: each minute trying to predict the price movement for next minute. If predicts going
  #up then buy mkt order now and sell mkt order 1 interval later
  #if predicts going down then sell order now and buy 1 interval later
  
  current_time <- start_time
  if (sum(is.na(getquotes(env, symbol, start_time))) != 0){
    cat('Error: data not available at start time \n')
    return
  }
  else if(sum(is.na(getquotes(env, symbol, end_time))) != 0){
    cat('Error: data not available at end time \n')
    return
  }
  
  Tnum <- 390 / time_interval
  VWAP_prices <- vector()
  interval_volume <- vector()
  cumul_volume <- 0
  cumul_value <- 0
  orderID <- 0
  predicted_price_direction <- NA
  prediction_list <- matrix(NA, 0, Tnum)
  p_increment_list <- matrix(NA, 0, Tnum)
  
  current_shares_to_trade <- 0
  future_shares_to_trade <- 0
  lot_size <- 100
  
  eod_value <- vector()
  execution_dates <- vector()
  while (current_time <= end_time){
    
    quotes <- getquotes(env, symbol, current_time)
    if (sum(is.na(quotes)) != 0){
      current_time <- current_time + 60
      next
    }
    response <- update_pendingorderbook(env, current_time, symbol)
    if (nrow(response)!=0){
      passive_processing(response)
    }
    
    time_since_open <- get_time_since_open(current_time)
    cumul_value <- cumul_value + quotes[,Con_Data_ColName_LastValue]
    cumul_volume <- cumul_volume + quotes[,Con_Data_ColName_LastVolume]
    if (time_since_open %% (time_interval* 60) == 0){
      current_shares_to_trade <- future_shares_to_trade
      future_shares_to_trade <- 0
      #Active processing
      if (time_since_open == 0){
        VWAP_prices <- quotes[,Con_FieldName_CurrentTick]
        interval_volume <- vector()
        predicted_price_direction <- NA
        cat('testing ', strftime(current_time, format = "%F"), '\n')
      }
      else if (time_since_open == 23400){
        last_interval_VWAP_price <- cumul_value / cumul_volume
        VWAP_prices <- append(VWAP_prices, last_interval_VWAP_price)
        p_increment <- VWAP_prices[2:length(VWAP_prices)] - VWAP_prices[1:(length(VWAP_prices) - 1)]
        
        prediction_list <- rbind(prediction_list, predicted_price_direction)
        p_increment_list <- rbind(p_increment_list, p_increment)
        
        current_time = current_time + 60 * 60 * 16
      }
      else{
        #price prediction
        if (cumul_volume == 0){
          last_interval_VWAP_price <- 0
        }
        else{
          last_interval_VWAP_price <- cumul_value / cumul_volume
        }
        last_interval_volume <- cumul_volume
        VWAP_prices <- append(VWAP_prices, last_interval_VWAP_price)
        interval_volume <- append(interval_volume, last_interval_volume)
        p_increment <- VWAP_prices[2:length(VWAP_prices)] - VWAP_prices[1:(length(VWAP_prices) - 1)]
        Tnum <- length(interval_volume)
        test_data <- array(0, c(num_var, Tnum, 1))
        test_data[1,,] <- p_increment
        test_data[2,,] <- log(interval_volume + 1)
        log_alpha <- calc_forward(num_states, 1, Tnum, num_var, A, mu, cov_mat, test_data)
        last_period_alpha <- log_alpha[,length(interval_volume), 1]
        alpha <- exp(last_period_alpha - max(last_period_alpha))
        states_prob <- alpha / sum(alpha)
        next_period_prob <- t(t(states_prob) %*% A)
        expected_mu <- sum(next_period_prob * mu[1,]) #only calculating expected p_increment for next time period
        if (current_time > as.POSIXct('2015-05-25 10:00:00 EDT')){
          l1 <- 0
        }
        if (expected_mu > 0){
          predicted_price_direction <- append(predicted_price_direction, TRUE)
          current_shares_to_trade <- current_shares_to_trade + lot_size
          future_shares_to_trade <- future_shares_to_trade - lot_size
        }
        else if (expected_mu < 0){
          predicted_price_direction <- append(predicted_price_direction, FALSE)
          current_shares_to_trade <- current_shares_to_trade -lot_size
          future_shares_to_trade <- future_shares_to_trade + lot_size
        }
        else{
          predicted_price_direction <- append(predicted_price_direction, NA)
        }
      }
      
      if (current_shares_to_trade != 0){
        side <- (current_shares_to_trade > 0) * Con_Side_Buy + (current_shares_to_trade < 0) * Con_Side_Sell
        orders = data.frame(matrix(NA, 1, length(order_msg_spec)))
        colnames(orders) <- order_msg_spec
        orders[,Con_FieldName_MsgType] = Con_MsgType_New
        orders[,Con_FieldName_OrdID] = orderID
        orderID = orderID + 1
        orders[,Con_FieldName_Sym] = symbol
        orders[,Con_FieldName_Qty] = abs(current_shares_to_trade)
        orders[,Con_FieldName_Side] = side
        orders[,Con_FieldName_OrdType] = Con_OrdType_Mkt
        response <- handle_orders(orders, symbol, env, current_time)
        if (nrow(response)!=0){
          passive_processing(response)
        }
      }
      cumul_volume <- 0
      cumul_value <- 0
      
    }
    
    if (time_since_open == 23340){
      #liquidate current position at 15:59
      positionbook <- env[[Con_GlobalVarName_PositionBook]]
      last_pos <- positionbook[[length(positionbook)]]
      if (nrow(last_pos) > 1){
        outstanding_entries <- last_pos[last_pos[,Con_FieldName_Sym] != Con_Sym_Cash,]
        clearing_order_side <- rep(0, nrow(outstanding_entries))
        for (i in 1: nrow(outstanding_entries)){
          if (outstanding_entries[i,Con_FieldName_Qty] < 0){
            clearing_order_side[i] <- Con_Side_Buy
          }
          else{
            clearing_order_side[i] <- Con_Side_Sell
          }
        }
        orders = data.frame(matrix(NA, nrow(outstanding_entries), length(order_msg_spec)))
        colnames(orders) <- order_msg_spec
        orders[,Con_FieldName_MsgType] = Con_MsgType_New
        orders[,Con_FieldName_OrdID] = orderID + 0:(nrow(outstanding_entries) - 1)
        orderID = orderID + nrow(outstanding_entries)
        orders[,Con_FieldName_Sym] = outstanding_entries[,Con_FieldName_Sym]
        orders[,Con_FieldName_Qty] = abs(outstanding_entries[,Con_FieldName_Qty])
        orders[,Con_FieldName_Side] = clearing_order_side
        orders[,Con_FieldName_OrdType] = Con_OrdType_Mkt
        response <- handle_orders(orders, symbol, env, current_time)
        if (nrow(response)!=0){
          passive_processing(response)
        }
      }
      last_pos <- env[[Con_GlobalVarName_PositionBook]][[length(env[[Con_GlobalVarName_PositionBook]])]]
      eod_value <- append(eod_value, last_pos[last_pos[,Con_FieldName_Sym] == Con_Sym_Cash, Con_FieldName_MktVal])
      execution_dates <- append(execution_dates, current_time)
      current_shares_to_trade <- 0
      future_shares_to_trade <- 0
    }
    
    current_time <- current_time + 60
    
  }
  return(prediction_list)
  
}


passive_processing <- function(response){
  
}
