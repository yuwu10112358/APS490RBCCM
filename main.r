source('strategy_lib.r')
source('backtest_lib.r')

#global_variables: position matrices, trade matrices, ourdata, order book

#order format: msgtype, symbol, price, quantity, side, ordtype, orderID, time
#execution message format: orderID, Execstatus, symbol, quantity, avg price, side, time
#Execstatus can be the following: filled, replaced, cancelled, replacereject?, cancelreject

#order book format: orderID, time, symbol, price, quantity, side, ordtype
#trade matrix format: time, symbol, side, quantity, price, open/close, pnl
#position matrix: time, asset, #of shares, book value, market value,

inputData()
strategy_naive()
output <- output()

yay
