cumulative_pnl_dataframe <- data.frame(Symbol = as.character(), CumAssetReturns = as.integer())

for (i in 1:length(stock_list)){
  filter_trades <- subset(tradesbook, Symbol == stock_list[i])
  filter_position <- subset(positionbook, Symbol == stock_list[i])
  asset_return <- 0
  for (j in 1:NROW(filter_trades)){
    if (filter_trades[j, "Open/Close"] == "Close"){
      date_time_1 <- as.POSIXct(as.numeric(filtered_tradesbook[j, "Timestamp"]), origin="1970-01-01")
      bk_value <- abs(as.numeric(Pnl_df[Pnl_df$DateTime == date_time_1 & Pnl_df$Symbol == stock_list[i], ]["BookValue"]))
      cur_value <- as.numeric(filter_trades[j, "Price"])
      cur_side <- filter_trades[j, "Side"]
      cur_asset_return <- (cur_value - bk_value)/(bk_value)*100
      if (cur_side == 1){
        cur_asset_return <- abs(cur_asset_return)
      }
      asset_return <- cur_asset_return + asset_return 
    }
  }
  cumulative_pnl_dataframe <- rbind(cumulative_pnl_dataframe, data.frame(Symbol = stock_list[i],
                                                                         CumAssetReturns = asset_return))
}

cumulative_pnl_dataframe
