source('hmm.r')
source('constants.r')
source('backtest_lib.r')
test_HMMM <- function (env, symbol, time_interval, num_states){
#     env <- global_tables
#     symbol = "BNS"
#     time_interval = 5
  datatable_name <- paste(symbol, Con_Data_Tick_Suffix, sep = "")
  timestamp <- env[[datatable_name]][[Con_Data_ColName_Date]]
  value <- env[[datatable_name]][[Con_Data_ColName_Value]]
  volume <- env[[datatable_name]][[Con_Data_ColName_Volume]]
  
  #timestamp converted into integer represents seconds
  #9:30 EDT mod 86400 = 48600, 9:30 EST mod 86400 = 48600+ 3600
  #trading day is 23400s long
  
  
  #split the data into days
  #assume there is always data at 15:59
  
  time_since_open <- rep(0, length(timestamp))
  
  for (i in 1: length(timestamp))
      time_since_open[i] <- get_time_since_open(timestamp[i])
    
  
  N <- sum(time_since_open == 23340)
  value_by_day <- matrix(-1, nrow = N, ncol = 390)
  volume_by_day <- matrix(-1, nrow = N, ncol = 390)
  time_by_day <- matrix(-1, nrow = N, ncol = 390)
  
  current_day_index <- 1
  day_index <- 1
  
  for (i in 1:length(timestamp)){
    value_by_day[day_index, current_day_index] = value[i]
    volume_by_day[day_index, current_day_index] = volume[i]
    time_by_day[day_index, current_day_index] = time_since_open[i]
    current_day_index = current_day_index + 1
    if (time_since_open[i] == 23340){
      day_index = day_index + 1
      current_day_index = 1
    }
  }
  
  #time_interval = 5 #denote minutes
  
  Tnum = 23400 / (time_interval * 60)
  t_marker <- (time_interval * 60) * (1: Tnum)
  
  VWAP <- matrix(0, nrow = N, ncol = Tnum + 1)
  interval_volume = matrix(0, nrow = N, ncol = Tnum)
  VWAP[,1] = env[[datatable_name]][[Con_Data_ColName_Open]][time_since_open == 0]
  for (n in 1:N){
    cumul_volume <- 0
    cumul_value <- 0
    t_marker_iterator <- 1
    for (t in 1:390){
      if (time_by_day[n, t] >= t_marker[t_marker_iterator]){
        interval_volume[n, t_marker_iterator] = cumul_volume
        if (cumul_volume == 0){
          VWAP[n, t_marker_iterator + 1] = 0
        }
        else{
          VWAP[n, t_marker_iterator + 1] = cumul_value / cumul_volume
        }
        t_marker_iterator = t_marker_iterator + 1
        cumul_value = 0
        cumul_volume = 0
      }
      cumul_value = cumul_value + value_by_day[n, t]
      cumul_volume = cumul_volume + volume_by_day[n, t]
      if (time_by_day[n, t] == 23340){
        interval_volume[n, t_marker_iterator] = cumul_volume
        if (cumul_volume == 0){
          VWAP[n, t_marker_iterator + 1] = 0
        }
        else{
          VWAP[n, t_marker_iterator + 1] = cumul_value / cumul_volume
        }
        break
      }
    }
    
  }
  p_increments <- VWAP[,2:(Tnum + 1)] - VWAP[,1:Tnum]
  
  # remove outliers 
  
  training_days <- c(1:60)
  
  #qqnorm(as.vector(log(interval_volume[1:60,])))
  p_increments_training <- p_increments[training_days,]
  volume_training <- interval_volume[training_days,]
  
  estimates <- get_params_estimates(p_increments_training, volume_training, num_states)
  
  A <- data.matrix(estimates[["A"]])
  mu <- data.matrix(estimates[["mu"]])
  sigma_mu <- data.matrix(estimates[["sigma_mu"]])
  eta <- data.matrix(estimates[["eta"]])
  sigma_eta <- data.matrix((estimates[["sigma_eta"]]))
  
  performance_test(env, symbol, num_states, A, mu, sigma_mu, eta, sigma_eta)
}

get_params_estimates <- function (p_increments_training, volume_training, num_states)
{
  remove_pct <- 0.05
  
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
  A_estimates <- array(0, c(num_estimates, num_states, num_states))
  mu_estimates <- matrix(0, nrow = num_estimates, ncol = num_states)
  sigma_mu_estimates <- matrix(0, nrow = num_estimates, ncol = num_states)
  eta_estimates <- matrix(0, nrow = num_estimates, ncol = num_states)
  sigma_eta_estimates <- matrix(0, nrow = num_estimates, ncol = num_states)
  for (i in 1: num_estimates){
    tr_indices <- sample(1:nrow(p_increments_aft_filter), rand_size)
    estimate <- EM(p_increments_aft_filter[tr_indices,], log(volume_aft_filter[tr_indices,]), num_states)
    A_estimates[i,,] <- data.matrix(estimate[["A"]])
    mu_estimates[i,] <- data.matrix(estimate[["mu"]])
    sigma_mu_estimates[i,] <- data.matrix(estimate[["sigma_mu"]])
    eta_estimates[i,] <- data.matrix(estimate[["eta"]])
    sigma_eta_estimates[i,] <- data.matrix(estimate[["sigma_eta"]])
  }
  
  rtn <- list(data.frame(A_estimates), data.frame(mu_estimates),
              data.frame(sigma_mu_estimates), data.frame(eta_estimates), 
              data.frame(sigma_eta_estimates))
  names(rtn) <- c('A', 'mu', 'sigma_mu', 'eta', 'sigma_eta')
  return(rtn)
}

performance_test <- function(start_time, end_time, env, symbol, time_interval, num_states, A, mu, sigma_mu, eta, sigma_eta){

  #if for a symbol the quotes at a particular time is not available, then
  #it is filled in by the data of the last available minute (e.g, if at 9:33 the data
  #is missing, but at 9:32 it is available, then the quote at 9:32 is taken to be the same
  # as the quote of 9:32)
  
  current_time <- start_time
  if (sum(is.na(getquotes(env, symbol, start_time))) != 0){
    cat('Error: data not available at start time /n')
    return
  }
  else if(sum(is.na(getquotes(env, symbol, end_time))) != 0){
    cat('Error: data not available at end time /n')
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
      #Active processing
      if (time_since_open == 0){
        VWAP_prices <- quotes[,Con_FieldName_CurrentTick]
        interval_volume <- vector()
        predicted_price_direction <- NA
      }
      else if (time_since_open == 23400){
        last_interval_VWAP_price <- cumul_value / cumul_volume
        VWAP_prices <- append(VWAP_prices, last_interval_VWAP_price)
        p_increment <- VWAP_prices[2:length(VWAP_prices)] - VWAP_prices[1:(length(VWAP_prices) - 1)]
        
        prediction_list <- rbind(prediction_list, predicted_price_direction)
        p_increment_list <- rbind(p_increment_list, p_increment)
        
      }
      else{
        #price prediction
        last_interval_VWAP_price <- cumul_value / cumul_volume
        last_interval_volume <- cumul_volume
        VWAP_prices <- append(VWAP_prices, last_interval_VWAP_price)
        interval_volume <- append(interval_volume, last_interval_volume)
        p_increment <- VWAP_prices[2:length(VWAP_prices)] - VWAP_prices[1:(length(VWAP_prices) - 1)]
        log_alpha <- calc_forward(1, length(interval_volume), A, p_increment, mu, sigma_mu, interval_volume, eta, sigma_eta)
        last_period_alpha <- log_alpha[,length(interval_volume), 1]
        alpha <- exp(last_period_alpha - max(last_period_alpha))
        states_prob <- alpha / sum(alpha)
        next_period_prob <- t(states_prob) %*% A
        expected_mu <- next_period_prob * mu
        if (mu > 0){
          predicted_price_direction <- append(predicted_price_direction, TRUE)
        }
        else if (mu < 0){
          predicted_price_direction <- append(predicted_price_direction, FALSE)
        }
        else{
          predicted_price_direction <- append(predicted_price_direction, NA)
        }
      }
      cumul_volume <- 0
      cumul_value <- 0
      
    }
    
    if (time_since_open == 23340){
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
    }
    
    current_time <- current_time + 60
    
  }
  
  
}


passive_processing <- function(response){
  
}
