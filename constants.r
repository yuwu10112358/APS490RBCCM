#order format: msgtype, symbol, price, quantity, side, ordtype, orderID, time
#execution message format: orderID, Execstatus, symbol, quantity, avg price, side, time
#Execstatus can be the following: filled, replaced, cancelled, replacereject?, cancelreject

#order book format: orderID, time, symbol, price, quantity, side, ordtype
#trade matrix format: time, symbol, side, quantity, price, open/close, pnl
#position matrix: time, asset(symbol), #of shares, book value, market value,

Con_FieldName_MsgType = "Msgtype"
Con_FieldName_Sym = "Symbol"
Con_FieldName_Price = "Price"
Con_FieldName_Qty = "Quantity"
Con_FieldName_Side = "Side"
Con_FieldName_OrdType = "OrdType"
Con_FieldName_OrdID = "OrdID"
Con_FieldName_Time = "Timestamp"
Con_FieldName_ExecStatus = "ExecStatus"
Con_FieldName_AvgPrice = "AvgPrice"
Con_FieldName_BookVal = "BookValue"
Con_FieldName_MktVal = "MarketValue"
Con_FieldName_OpenClose = "Open/Close"
Con_FieldName_Pnl = "PnL"


Con_ExecStatus_filled <- 2
Con_Side_Buy <- 1
Con_Side_Sell <- 2
Con_MsgType_New <- "D"
Con_MsgType_Replace <- "G"
Con_MsgType_Cancel <- "F"
Con_OrdType_Mkt <- 1
Con_OrdType_Limit <- 2
Con_Sym_Cash <- "Cash"
Con_Sym_Portfolio<- "Portfolio"
Con_PriceCol <- 2

order_msg_spec <- c(Con_FieldName_MsgType, 
                    Con_FieldName_Sym, 
                    Con_FieldName_Price,
                    Con_FieldName_Qty,
                    Con_FieldName_Side,
                    Con_FieldName_OrdType,
                    Con_FieldName_OrdID,
                    Con_FieldName_Time)

exec_msg_spec <- c(Con_FieldName_OrdID,
                   Con_FieldName_ExecStatus, 
                   Con_FieldName_Sym, 
                   Con_FieldName_Qty, 
                   Con_FieldName_AvgPrice, 
                   Con_FieldName_Side,
                   Con_FieldName_Time)

orderbook_spec <- c(Con_FieldName_OrdID,
                    Con_FieldName_Time,
                    Con_FieldName_Sym,
                    Con_FieldName_Price,
                    Con_FieldName_Qty,
                    Con_FieldName_Side,
                    Con_FieldName_OrdType)

positionbook_spec <- c(Con_FieldName_Sym,
                       Con_FieldName_Qty,
                       Con_FieldName_BookVal,
                       Con_FieldName_MktVal)

tradesbook_spec <- c(Con_FieldName_Time,
                     Con_FieldName_Sym,
                     Con_FieldName_Side,
                     Con_FieldName_Qty,
                     Con_FieldName_Price,
                     Con_FieldName_OpenClose,
                     Con_FieldName_Pnl)
