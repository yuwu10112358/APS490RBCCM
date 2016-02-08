rm(list = ls())
source('constants.r')
source('backtest_lib.r')
source('data_cleaning.r')
library(XLConnect)
#library(knitr)




# notes 2015-11-20

#1. ETL (cleaning, organizing section), factor in for missing data, outliers, etc. 
# -> think of the opportunistic time intervals to trade (don't decide arbritarily, decide based on
# intelligence)
# -> separate the strategy from the market module in the flow chart 
# -> market sends back fills and acknowledgements (assume we don't need this)
# -> strategy will act only upon "fill" (may not need to do this) and "timer"
# -> market reacts to order,replace,cancel and market data 
# -> change active and passive to fill and timer 
# -> highlight strategy with more detail 

#knitr::spin


init_cash = 100000
#global_variables: position matrices, trade matrices, ourdata, order book

#order format: msgtype, symbol, price, quantity, side, ordtype, orderID, time
#execution message format: orderID, Execstatus, symbol, quantity, avg price, side, time
#Execstatus can be the following: filled, replaced, cancelled, replacereject?, cancelreject

#order book format: orderID, time, symbol, price, quantity, side, ordtype
#trade matrix format: time, symbol, side, quantity, price, open/close, pnl
#position matrix: time, asset, #of shares, book value, market value,
datafile_name = "../data/TSXdatafile.xls"
global_tables = new.env()

global_tables[[Con_GlobalVarName_LOB]]<- data.frame(matrix(0, 0, length(orderbook_spec)))
colnames(global_tables[[Con_GlobalVarName_LOB]]) <- orderbook_spec

#the position book is a list of data frames
init_pos <- data.frame(matrix(0, 1, length(positionbook_spec)))
colnames(init_pos) <- positionbook_spec
init_pos[,Con_FieldName_Sym] = Con_Sym_Cash
init_pos[,c(Con_FieldName_Qty, Con_FieldName_BookVal, Con_FieldName_MktVal)] = init_cash

global_tables[[Con_GlobalVarName_PositionBook]] <- list(init_pos)
names(global_tables[[Con_GlobalVarName_PositionBook]])[1] = 0

global_tables[[Con_GlobalVarName_TradesBook]] <- data.frame(matrix(0, 0, length(tradesbook_spec)))
colnames(global_tables[[Con_GlobalVarName_TradesBook]]) <- tradesbook_spec

global_tables[[Con_GlobalVarName_MktPrice]] <- list(vector())
global_tables[[Con_GlobalVarName_BidPrice]] <- list(vector())
global_tables[[Con_GlobalVarName_AskPrice]] <- list(vector())

import_data(global_tables)

# output <- output(global_tables$tradesbook, global_tables$positionbook, global_tables$ask_price, 
#                   global_tables$bid_price, globa_tables$market_data)




