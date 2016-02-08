source('backtest_lib.r')
source('constants.r')

env = global_tables
symbol = 'BNS'
symbol2 = 'BMO'
bid_table <- paste(symbol, "_bid", sep = "")
ask_table <- paste(symbol, "_ask", sep = "")
tick_table <- paste(symbol, "_tick", sep = "")
timestamp = env[[tick_table]][["Date"]][170]
orderID = 0

#submit a mkt new order
orders = data.frame(matrix(NA, 1, length(order_msg_spec)))
colnames(orders) <- order_msg_spec
orders[,Con_FieldName_MsgType] = Con_MsgType_New
orders[,Con_FieldName_OrdID] = orderID
orderID = orderID + 1
orders[,Con_FieldName_Sym] = symbol
orders[,Con_FieldName_Qty] = 100
orders[,Con_FieldName_Side] = Con_Side_Buy
orders[,Con_FieldName_OrdType] = Con_OrdType_Mkt
response <- handle_orders(orders, symbol, env, timestamp)

#submit mkt orders with different symbols
orders = data.frame(matrix(NA, 2, length(order_msg_spec)))
colnames(orders) <- order_msg_spec
orders[,Con_FieldName_MsgType] = Con_MsgType_New
orders[,Con_FieldName_OrdID] = orderID + 0:1
orderID = orderID + 2
orders[,Con_FieldName_Sym] = c(symbol, symbol2)
orders[,Con_FieldName_Qty] = 100
orders[,Con_FieldName_Side] = Con_Side_Buy
orders[,Con_FieldName_OrdType] = Con_OrdType_Mkt
response <- handle_orders(orders, c(symbol, symbol2), env, timestamp)

#submit lmt orders with different symbols
#buy BNS at 65, sell BMO at 78
timestamp = env[[tick_table]][["Date"]][2]
orders = data.frame(matrix(NA, 2, length(order_msg_spec)))
colnames(orders) <- order_msg_spec
orders[,Con_FieldName_MsgType] = Con_MsgType_New
orders[,Con_FieldName_OrdID] = orderID + 0:1
orderID = orderID + 2
orders[,Con_FieldName_Sym] = c('BNS', 'BMO')
orders[,Con_FieldName_Qty] = 100
orders[,Con_FieldName_Price] = c(65, 78)
orders[,Con_FieldName_Side] = c(Con_Side_Buy, Con_Side_Sell)
orders[,Con_FieldName_OrdType] = Con_OrdType_Limit
response <- handle_orders(orders, c('BNS', 'BMO'), env, timestamp)

for (t in 3:100){
  response <- update_pendingorderbook(env, env[[tick_table]][["Date"]][t], c('BNS', 'BMO'))
}

