filter_pnL <- subset(tradesbook, Symbol == "AAPL")
filter_pnL

for (i in 1:length(stock_list)) {
  filter_trades <- subset(tradesbook, Symbol == stock_list[i])
  filter_position <- subset(positionbook, Symbol == stock_list[i])
  asset_return <- 0
  for (j in 1:NROW(filter_trades)){
    if (filter_trades[j, "Open/Close"] == "Close"){
      date_time_1 <- as.character(filtered_trades[j, "Timestamp"])
      bk_value <- positionbook[positionbook$Timestamp == date_time_1 & positionbook$Symbol == stock_list[i], ]["BookValue"]
      cur_value <- filtered_trades[j, "Price"]
      asset_return <- (cur_value - bk_value)/(bk_value)*100 + asset_return 
    }
  }
}

as.character(filtered_tradesbook[j, "Timestamp"])