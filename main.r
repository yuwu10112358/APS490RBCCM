source('constants.r')
source('strategy_lib.r')
source('backtest_lib.r')

init_cash = 100000
#global_variables: position matrices, trade matrices, ourdata, order book

#order format: msgtype, symbol, price, quantity, side, ordtype, orderID, time
#execution message format: orderID, Execstatus, symbol, quantity, avg price, side, time
#Execstatus can be the following: filled, replaced, cancelled, replacereject?, cancelreject

#order book format: orderID, time, symbol, price, quantity, side, ordtype
#trade matrix format: time, symbol, side, quantity, price, open/close, pnl
#position matrix: time, asset, #of shares, book value, market value,
global_tables = new.env()
global_tables$orderbook <- data.frame(matrix(0, 0, length(orderbook_spec)))
colnames(global_tables$orderbook) <- orderbook_spec

init_pos <- data.frame(matrix(0, 1, length(positionbook_spec)))
colnames(init_pos) <- positionbook_spec
init_pos[,Con_FieldName_Sym] = Con_Sym_Cash
init_pos[,c(Con_FieldName_Qty, Con_FieldName_BookVal, Con_FieldName_MktVal)] = init_cash
global_tables$positionbook <- list(init_pos)
names(global_tables$positionbook)[1] = 0

global_tables$tradesbook <- data.frame(matrix(0, 0, length(tradesbook_spec)))
colnames(global_tables$tradesbook) <- tradesbook_spec

inputData()
strategy_naive()
output <- output()

yay
