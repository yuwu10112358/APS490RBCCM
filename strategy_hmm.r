source('hmm_v2.r')
source('constants.r')
source('backtest_lib.r')


test_HMMM <- function (env, symbols, time_interval, num_states){
  
#     env <- global_tables
#     symbol = "BNS"
#     time_interval = 5
#     num_states = 4


  N <- 135 #number of days where data is available
  
  num_sym <- length(symbols)
  Tnum = 23400 / (time_interval * 60)
  
  N_training = 60
  training_days <- c(1:N_training)
  
  num_var <- 3
  
  interval_volume_lamdas <- rep(0, num_sym)
  p_absolute_lamdas <- rep(0, num_sym)
  
  delta_p <- array(0, c(num_sym, N, Tnum))
  training_data <- array(0, c(num_var * num_sym, N_training, Tnum))
  for (sym in 1:num_sym){
    symbol <- symbols[sym]
    train_data <- get_data(env, symbol, time_interval)
    p_increments <- data.matrix(train_data[["p_increments"]])
    interval_volume <- data.matrix(train_data[["interval_volume"]])
    p_absolute <- data.matrix(train_data[["p_absolute"]])
    interval_volume_lamdas[sym] <- train_data[["Boxcoxlambda_interval_volume"]]
    p_absolute_lamdas[sym] <- train_data[["Boxcoxlambda_p_absolute"]]
    delta_p[sym,,] <- p_increments
    
    #qqnorm(as.vector(log(interval_volume[1:60,])))
    p_increments_training <- p_increments[training_days,]
    volume_training <- interval_volume[training_days,]
    p_absolute_training <- p_absolute[training_days,]
    
    training_data[(sym - 1) * num_var + 1,,] <- p_increments_training
    training_data[(sym - 1) * num_var + 2,,] <- volume_training
    training_data[(sym - 1) * num_var + 3,,] <- p_absolute_training
  }
  
  cat('get parameter estimates \n')
  
  estimates <- get_params_estimates(training_data, num_var * num_sym, num_states)
  
  A <- matrix(data.matrix(estimates[["A"]]), nrow = num_states, ncol = num_states)
  mu <- data.matrix(estimates[["mu"]])
  cov_mat <- array(data.matrix(estimates[["cov_matrix"]]), c(num_var * num_sym, num_var * num_sym, num_states))
  
  

  start_time <- as.POSIXct('2015-05-13 9:30:00 EDT')
  end_time <- as.POSIXct('2015-05-15 16:00:00 EDT')


  
  cat('run performance testing \n')
  system.time({performance_results <- performance_test(start_time, end_time, env, symbols, time_interval, num_states, num_var, A, mu, cov_mat, interval_volume_lamdas, p_absolute_lamdas)})
  
  predictions_temp <- data.matrix(performance_results[["predictions"]])
  eod_values <- data.matrix(performance_results[["eod_values"]])
  num_days <- length(eod_values)
  predictions <- array(predictions_temp, c(num_sym, num_days, Tnum))
  accuracy <- rep(0, num_sym)
  for (sym in 1:num_sym){
    prediction_sym <- predictions[sym,,]
    actual_directions <- matrix(NA, nrow(prediction_sym), Tnum)
    for (i in 1:nrow(prediction_sym)){
      for (t in 1:Tnum){
        if (delta_p[sym,i,t] > 0){
          actual_directions[i,t] = TRUE
        }
        else if(delta_p[sym,i,t] < 0){
          actual_directions[i,t] = FALSE
        }
        else{
          
        }
      }
    }
    temp_ind <- !is.na(prediction_sym)
    actual_d2 <- actual_directions[1:nrow(prediction_sym),]
    comparison <- prediction_sym[temp_ind] == actual_d2[temp_ind]
    accuracy[sym] <- sum(comparison[!is.na(comparison)]) / length(comparison)
  }
  
  eod_vals <- performance_results[["eod_values"]]
  rtn_lst <- list(accuracy, eod_vals, A, mu, cov_mat)
  names(rtn_lst) <- c("accuracy", "eod_results",'A', 'mu', 'cov_matrix')
  return(rtn_lst)
}


get_data <- function(env, symbol, time_interval)
{
  datatable_name <- paste(symbol, Con_Data_Tick_Suffix, sep = "")
  timestamp <- env[[datatable_name]][[Con_Data_ColName_Date]]
  value <- env[[datatable_name]][[Con_Data_ColName_Value]]
  volume <- env[[datatable_name]][[Con_Data_ColName_Volume]]
  
  #timestamp converted into integer represents seconds
  #9:30 EDT mod 86400 = 48600, 9:30 EST mod 86400 = 48600+ 3600
  #trading day is 23400s long
  
  
  #split the data into days
  #assume there is always data at 15:59
  
  cat('Retrieving data for', symbol, '\n')
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
  
  Boxcoxlambda_interval_volume <- BoxCox.lambda(interval_volume, method=c("guerrero"),lower=-5, upper=5)
  interval_volume<- BoxCox(interval_volume,lambda = Boxcoxlambda_interval_volume)
  
  p_increments <- VWAP[,2:(Tnum + 1)] - VWAP[,1:Tnum]
  #Boxcoxlambda_p_increments <-BoxCox.lambda(p_increments,method=c("guerrero"),lower=-5, upper=5)
  #p_increments<- BoxCox(p_increments,lambda = Boxcoxlambda_p_increments)
  
  p_absolute <- abs(VWAP[,2:(Tnum + 1)] - VWAP[,1:Tnum])
  Boxcoxlambda_p_absolute <-BoxCox.lambda(p_absolute,method=c("guerrero"),lower=-5, upper=5)
  p_absolute<- BoxCox(p_absolute,lambda = Boxcoxlambda_p_absolute)
  
  df_p_increments <- data.frame(p_increments)
  df_interval_volume <- data.frame(interval_volume)
  df_p_absolute <- data.frame(p_absolute)
  rtn_lst <- list(df_p_increments, df_interval_volume, df_p_absolute, Boxcoxlambda_interval_volume, Boxcoxlambda_p_absolute)
  names(rtn_lst) <- c('p_increments', 'interval_volume', 'p_absolute','Boxcoxlambda_interval_volume', 'Boxcoxlambda_p_absolute')
  return(rtn_lst)
}

get_params_estimates <- function (training_data, num_var, num_states)
{
  #training data is v X N X T matrix, where v is the number of state_variables, N is the number of time series, and T is the number of timesteps
  remove_pct <- 0.02
  
  Tnum <- dim(training_data)[3]
  N <- dim(training_data)[2]
  
  rows_to_remove <- rep(FALSE, N)
  
  for (i in 1:num_var){
    filter <- quantile(as.vector(training_data[i,,]), c(remove_pct * (1/Tnum), 1 - remove_pct * (1/Tnum)))
    rows_to_remove <- rows_to_remove | (apply(training_data[i,,], 1, max) > filter[2]) | 
      (apply(training_data[i,,], 1, min) < filter[1])
  }
  data_aft_filter <- training_data[,!rows_to_remove,]
  rand_size = dim(data_aft_filter)[2]
  num_estimates <- 1

  
  #order of state parameters: p_increments then log volume then p_absolute
  #num_estimates is always the last dimension
  
  A_estimates <- array(0, c(num_states, num_states, num_estimates))
  mu_estimates <- array(0, c(num_var, num_states, num_estimates))
  cov_mat_estimates <- array(0, c(num_var, num_var, num_states, num_estimates))
  
  for (i in 1: num_estimates){
    tr_indices <- sample(1:dim(data_aft_filter)[2], rand_size)
    test_data = array(0, c(num_var, Tnum, rand_size))
    for (j in 1:num_var){
      test_data[j,,] <- t(data_aft_filter[j,tr_indices,])
    }
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

performance_test <- function(start_time, end_time, env, symbols, time_interval, num_states, num_var, A, mu, cov_mat, interval_volume_lamdas, p_absolute_lamdas){
  #if for a symbol the quotes at a particular time is not available, then
  #it is filled in by the data of the last available minute (e.g, if at 9:33 the data
  #is missing, but at 9:32 it is available, then the quote at 9:32 is taken to be the same
  # as the quote of 9:32)
  
  #strategy: each minute trying to predict the price movement for next minute. If predicts going
  #up then buy mkt order now and sell mkt order 1 interval later
  #if predicts going down then sell order now and buy 1 interval later
  max_day_num <- 150
  
  num_sym = length(symbols)
  max_exp_p_increment <- rep(0, num_sym)
  min_exp_p_increment <- rep(0, num_sym)
  for (sym in 1:num_sym){
    max_exp_p_increment[sym] <- max(A %*% mu[(sym - 1) * num_var + 1,])
    min_exp_p_increment[sym] <- min(A %*% mu[(sym - 1) * num_var + 1,])
  }
  
  threshold_factor <- 0.6
  
  current_time <- start_time
  if (sum(is.na(getquotes(env, symbols, start_time))) != 0){
    cat('Error: data not available at start time \n')
    return
  }
  else if(sum(is.na(getquotes(env, symbols, end_time))) != 0){
    cat('Error: data not available at end time \n')
    return
  }
  
  Tnum <- 390 / time_interval
  VWAP_prices <- matrix(NA, nrow = num_sym, ncol = Tnum + 1)
  interval_volume <- matrix(NA, nrow = num_sym, ncol = Tnum)
  cumul_volume <- rep(0, num_sym)
  cumul_value <- rep(0, num_sym)
  cumul_volatility <- rep(0, num_sym)
  
  orderID <- 0
  predicted_price_direction <- NA
  prediction_list <- array(NA, c(num_sym, max_day_num, Tnum))
#   p_increment_list <- matrix(NA, 0, Tnum)
#   p_absolute_list <- matrix(NA, 0, Tnum)
  
  current_shares_to_trade <- rep(0, num_sym)
  future_shares_to_trade <- rep(0, num_sym)
  lot_size <- 100
  
  eod_value <- vector()
  execution_dates <- vector()
  
  intraday_counter <- 1
  day_counter <- 1
  
  while (current_time <= end_time){
    
    quotes <- getquotes(env, symbols, current_time)
    if (sum(is.na(quotes)) != 0){
      current_time <- current_time + 60
      next
    }
    response <- update_pendingorderbook(env, current_time, symbols)
    if (nrow(response)!=0){
      passive_processing(response)
    }
    
    time_since_open <- get_time_since_open(current_time)
    cumul_value <- cumul_value + quotes[,Con_Data_ColName_LastValue]
    cumul_volume <- cumul_volume + quotes[,Con_Data_ColName_LastVolume]
    cumul_volatility <- cumul_volatility + quotes[,Con_Data_ColName_LastValue]
    
    if (time_since_open %% (time_interval* 60) == 0){
      current_shares_to_trade <- future_shares_to_trade
      future_shares_to_trade <- rep(0, num_sym)
      #Active processing
      if (time_since_open == 0){
        VWAP_prices <- matrix(NA, nrow = num_sym, ncol = Tnum + 1)
        VWAP_prices[,1] <- quotes[,Con_FieldName_CurrentTick]
        interval_volume <- matrix(NA, nrow = num_sym, ncol = Tnum)
        predicted_price_direction <- matrix(NA, nrow = num_sym, ncol = Tnum)
        cat('testing ', strftime(current_time, format = "%F"), '\n')
        intraday_counter = 0
      }
      else if (time_since_open == 23400){
        last_interval_VWAP_price <- cumul_value / cumul_volume
        VWAP_prices[,intraday_counter + 1] <- last_interval_VWAP_price
        p_increments <- VWAP_prices[,2:(intraday_counter + 1)] - VWAP_prices[,1:intraday_counter]
        p_absolute <- abs(VWAP_prices[,2:(intraday_counter + 1)] - VWAP_prices[,1:intraday_counter])
        prediction_list[,day_counter,] <- predicted_price_direction
#         p_increment_list <- rbind(p_increment_list, p_increments)
#         p_absolute_list <- rbind(p_absolute_list, p_absolute)
        current_time = current_time + 60 * 60 * 16
        day_counter = day_counter + 1
        
      }
      else{
        #price prediction for each symbol
        last_interval_VWAP_price <- matrix(0, nrow = num_sym, ncol = 1)
        for (sym in 1:num_sym){
          
          if (cumul_volume[sym] == 0){
            last_interval_VWAP_price[sym,] <- 0
          }
          else{
            last_interval_VWAP_price[sym,] <- cumul_value[sym] / cumul_volume[sym]
          }
        }
        last_interval_volume <- cumul_volume
        VWAP_prices[,intraday_counter + 1] <- last_interval_VWAP_price
        
        for (sym in 1:num_sym){  
          last_interval_volume[sym] <- BoxCox(last_interval_volume[sym],lambda = interval_volume_lamdas[sym])
        }
        interval_volume[,intraday_counter] <- last_interval_volume
          
        p_increments <- VWAP_prices[,2:(intraday_counter + 1)] - VWAP_prices[,1:intraday_counter]
        dim(p_increments) <- c(num_sym, intraday_counter)
          #p_increments<- BoxCox(p_increments,lambda=Boxcoxlambda_p_increments)
          
        p_absolute <- abs(VWAP_prices[,2:(intraday_counter + 1)] - VWAP_prices[,1:intraday_counter])
        dim(p_absolute) <- c(num_sym, intraday_counter)
        test_data <- array(0, c(num_var * num_sym, intraday_counter, 1))
        for (sym in 1:num_sym){  
          p_absolute[sym,]<- BoxCox(p_absolute[sym,],lambda = p_absolute_lamdas[sym])
          
          test_data[(num_sym - 1) * num_var + 1,,] <- p_increments[sym,]
          test_data[(num_sym - 1) * num_var + 2,,] <- interval_volume[sym,1:intraday_counter]
          test_data[(num_sym - 1) * num_var + 3,,] <- p_absolute[sym,]
        }
        log_alpha <- calc_forward(num_states, 1, intraday_counter, num_var * num_sym, A, mu, cov_mat, test_data)
        last_period_alpha <- log_alpha[,intraday_counter, 1]
        alpha <- exp(last_period_alpha - max(last_period_alpha))
        states_prob <- alpha / sum(alpha)
        next_period_prob <- t(t(states_prob) %*% A)
        next_prediction <- matrix(NA, nrow = num_sym, ncol = 1)
        for (sym in 1:num_sym){  
          expected_mu <- sum(next_period_prob * mu[(num_sym - 1) * num_var + 1,]) #only calculating expected p_increments for next time period
          
          if (expected_mu > max_exp_p_increment[sym] * threshold_factor){
            next_prediction[sym] <- TRUE
            current_shares_to_trade[sym] <- current_shares_to_trade[sym] + lot_size
            future_shares_to_trade[sym] <- future_shares_to_trade[sym] - lot_size
          }
          else if (expected_mu < min_exp_p_increment[sym] * threshold_factor){
            next_prediction[sym] <- FALSE
            current_shares_to_trade[sym] <- current_shares_to_trade[sym] - lot_size
            future_shares_to_trade[sym] <- future_shares_to_trade[sym] + lot_size
          }
        }
        predicted_price_direction[,intraday_counter+1] <- next_prediction
      }
      new_orders = data.frame(matrix(NA, 0, length(order_msg_spec)))
      colnames(new_orders) <- order_msg_spec
      for(sym in 1:num_sym){
        if (current_shares_to_trade[sym] != 0){
          side <- (current_shares_to_trade[sym] > 0) * Con_Side_Buy + (current_shares_to_trade[sym] < 0) * Con_Side_Sell
          orders = data.frame(matrix(NA, 1, length(order_msg_spec)))
          colnames(orders) <- order_msg_spec
          orders[,Con_FieldName_MsgType] = Con_MsgType_New
          orders[,Con_FieldName_OrdID] = orderID
          orderID = orderID + 1
          orders[,Con_FieldName_Sym] = symbols[sym]
          orders[,Con_FieldName_Qty] = abs(current_shares_to_trade[sym])
          orders[,Con_FieldName_Side] = side
          orders[,Con_FieldName_OrdType] = Con_OrdType_Mkt
          new_orders <- rbind(new_orders, orders)
        }
      }
      response <- handle_orders(new_orders, symbols, env, current_time)
      if (nrow(response)!=0){
        passive_processing(response)
      }
      
      cumul_volume <- rep(0, num_sym)
      cumul_value <- rep(0, num_sym)
      cumul_volatility <- rep(0, num_sym)
      intraday_counter = intraday_counter + 1
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
        response <- handle_orders(orders, symbols, env, current_time)
        if (nrow(response)!=0){
          passive_processing(response)
        }
      }
      last_pos <- env[[Con_GlobalVarName_PositionBook]][[length(env[[Con_GlobalVarName_PositionBook]])]]
      eod_value <- append(eod_value, last_pos[last_pos[,Con_FieldName_Sym] == Con_Sym_Cash, Con_FieldName_MktVal])
      execution_dates <- append(execution_dates, current_time)
      current_shares_to_trade <- rep(0, num_sym)
      future_shares_to_trade <- rep(0, num_sym)
    }
    
    current_time <- current_time + 60
    
  }
  valid_predictions <- prediction_list[,1:(day_counter-1),]
  dim(valid_predictions) <- c(num_sym, day_counter - 1, Tnum)
  predictions <- data.frame(valid_predictions)
  eod_vals <- data.frame(eod_value)
  rtn_list <- list(predictions, eod_vals)
  names(rtn_list) <- c('predictions', 'eod_values')
  return(rtn_list)
  
}


passive_processing <- function(response){
  
}
