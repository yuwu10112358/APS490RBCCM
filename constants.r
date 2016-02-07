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
Con_FieldName_CurrentBid = "CurrentBid"
Con_FieldName_CurrentAsk = "CurrentAsk"
Con_FieldName_CurrentTick = "CurrentTick"
Con_FieldName_LastHighestBid = "HighestBid"
Con_FieldName_LastLowestAsk = "LowestAsk"
Con_Data_ColName_LastNumTicks = "NumTicks"
Con_Data_ColName_LastVolume = "Volume"
Con_Data_ColName_LastValue = "Value"

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
Con_OpenClose_Open <- "Open"
Con_OpenClose_Close <- "Close"


Con_PriceCol <- 2

Con_GlobalVarName_LOB <- "PendingOrderBook"
Con_GlobalVarName_PositionBook <- "positionbook"
Con_GlobalVarName_TradesBook <- "tradesbook"
Con_GlobalVarName_MktPrice <- "market_price"
Con_GlobalVarName_BidPrice <- "bid_price"
Con_GlobalVarName_AskPrice <- "ask_price"

Con_Data_Tick_Suffix <- "_tick"
Con_Data_Ask_Suffix <- "_ask"
Con_Data_Bid_Suffix <- "_bid"
Con_Data_ColName_Date <- "Date"
Con_Data_ColName_Open <- "OPEN"
Con_Data_ColName_High <- "HIGH"
Con_Data_ColName_Low <- "LOW"
Con_Data_ColName_LastPrice <- "LAST_PRICE"
Con_Data_ColName_NumTicks <- "NUMBER_TICKS"
Con_Data_ColName_Volume <- "VOLUME"
Con_Data_ColName_Value <- "VALUE"

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

mkt_quote_spec <- c(Con_FieldName_Sym,
                    Con_FieldName_CurrentBid,
                    Con_FieldName_CurrentAsk,
                    Con_FieldName_CurrentTick,
                    Con_FieldName_LastHighestBid,
                    Con_FieldName_LastLowestAsk,
                    Con_Data_ColName_LastNumTicks,
                    Con_Data_ColName_LastVolume,
                    Con_Data_ColName_LastValue)
