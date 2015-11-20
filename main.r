source('constants.r')
source('strategy_lib.r')
source('backtest_lib.r')
library(XLConnect)
library(knitr)

#knitr::spin


init_cash = 100000
#global_variables: position matrices, trade matrices, ourdata, order book

#order format: msgtype, symbol, price, quantity, side, ordtype, orderID, time
#execution message format: orderID, Execstatus, symbol, quantity, avg price, side, time
#Execstatus can be the following: filled, replaced, cancelled, replacereject?, cancelreject

#order book format: orderID, time, symbol, price, quantity, side, ordtype
#trade matrix format: time, symbol, side, quantity, price, open/close, pnl
#position matrix: time, asset, #of shares, book value, market value,
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

data_extraction(global_tables, Con_GlobalVarName_MktPrice, Con_GlobalVarName_BidPrice, Con_GlobalVarName_AskPrice)
strategy_naive(global_tables[[Con_GlobalVarName_MktPrice]][["Date"]][1],
               global_tables[[Con_GlobalVarName_MktPrice]][["Date"]][length(global_tables[[Con_GlobalVarName_MktPrice]][["Date"]])], 
               "AAPL", global_tables, bid= Con_GlobalVarName_BidPrice, 
               ask = Con_GlobalVarName_AskPrice, 
               mktprice = Con_GlobalVarName_MktPrice,
               positionbook = Con_GlobalVarName_PositionBook,
               pendingbook = Con_GlobalVarName_LOB)
output <- output()

# yay
