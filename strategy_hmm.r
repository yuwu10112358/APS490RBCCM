source('hmm.r')
source('constants.r')


get_params_estimates <- function (env, symbol)
{
  env <- global_tables
  symbol <- "BNS"
  
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
  
  for (i in 1: length(timestamp)){
    if (strftime(timestamp[i], format = "%Z") == "EDT")
      time_since_open[i] <- (as.numeric(timestamp[i]) %% 86400) - 48600
    else
      time_since_open[i] <- (as.numeric(timestamp[i]) %% 86400) - 52200
  
  }
  
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
  
  time_interval = 5 #denote minutes
  
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
  estimates <- EM(p_increments, log(interval_volume))
  
}