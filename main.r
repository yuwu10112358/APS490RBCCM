source('strategy_lib.r')
source('backtest_lib.r')

#global_variables: pnl matrices, trade matrices, ourdata, order book
#trading matrix columns: datetime, cash, position, total value
#
inputData()
strategy_naive()
output <- output()

yay
