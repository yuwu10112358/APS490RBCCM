
source('constants.r')
data_extraction <- function(env, tick_name, bid, ask)
{
      #Requirements:
        #The excel sheet contains 3 tables arranged in order: Tick, Ask, Bid price.
        #Number of columns in each table can vary
    library(XLConnect)
    file <- readWorksheetFromFile("testdata.xls", 
                                    sheet=1, 
                                    startRow = 3,
                                    check.names = FALSE
                                    )
    mylist = c()
    mylist[1] = 1
    for(i in 1:length(file)){
          if(is.na(file[,i])) {
            mylist[length(mylist)+1] = i
          }}
    cat(tick_name)
    cat(env$tick_name)
    tick = file[, mylist[1]:   (mylist[2] - 1)] 
    bid  = file[,(mylist[2]+1):(mylist[3] - 1)]
    ask  = file[,(mylist[3]+1):length(file)]
    env$market_price <- tick
    
}
 
update_orderbook <- function (marketprice, orderbook, timestamp){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #taking in orderbook as argument and returns a list containing execution messages
  #for the purpose of this back testing order book will conly contain pending limit orders
  ready_indices = (orderbook[,Con_FieldName_Price] >= marketprice && orderbook[,Con_FieldName_Side] == Con_Side_Buy) || (orderbook[,Con_FieldName_Price] <= marketprice && orderbook[,Con_FieldName_Side] == Con_Side_Sell)
  ready_orders = orderbook[ready_indices,]
  orderbook = orderbook[!ready_indices,]
  return (generate_fill_msgs(ready_orders, marketprice, timestamp))
}

update_trades_pnl_tables<- function (fill_msgs, positionbook, tradesbook, timestamp){
  #posTable and tradesTable are references and changes are permanent
  #takes in a list of execution messages and change the two tables, returns nothing
  #make sure every message is fill for sure
  fill_msgs <- fill_msgs[fill_msgs[,Con_FieldName_ExecStatus] == Con_ExecStatus_filled,]
  #update the position tables & trades table
  last_pos <- positionbook[[length(positionbook)]]
  new_pos <- last_pos
  previous_cash <- new_pos[new_pos[, Con_FieldName_Sym]== Con_Sym_Cash, Con_FieldName_Qty]
  cash_change <- 0
  for (i in 1:length(fill_msgs)){
    fill_sym <- fill_msgs[i, Con_FieldName_Sym]
    fill_side <- fill_msgs[i, Con_FieldName_Side]
    fill_qty <- fill_msgs[i, Con_FieldName_Qty]
    fill_price <- fill_msgs[i, Con_FieldName_AvgPrice]
    cash_change <- cash_change + (fill_side == Con_Side_Sell) * fill_qty * fill_price - (fill_side == Con_Side_Buy) * fill_qty * fill_price
    if (length(last_pos[last_pos[,Con_FieldName_Sym] == fill_sym]) == 0){
      #no positions exist for this symbol yet
      new_line_index <- nrow(last_pos) + 1
      new_pos[new_line_index, Con_FieldName_Sym] <- fill_sym
      new_pos[new_line_index, Con_FieldName_Qty] <- ((fill_side == Con_Side_Buy) * fill_qty
        - (fill_side == Con_Side_Sell) * fill_qty)
      new_pos[new_line_index, Con_FieldName_BookVal] <- new_pos[new_line_index, Con_FieldName_Qty] * fill_price
      new_pos[new_line_index, Con_FieldName_MktVal] <- new_pos[new_line_index, Con_FieldName_BookVal]
      #update the trades table
      insert_into_tradesbook(tradesbook, 
                             time = timestamp,
                             sym = fill_sym,
                             side = fill_side,
                             qty = fill_qty,
                             price = fill_price,
                             openclose = Con_OpenClose_Open,
                             pnl = NA)
    }
    else{
      #positions exist for this symbol
      index <- last_pos[last_pos[,Con_FieldName_Sym] == fill_msgs[i, Con_FieldName_Sym]][1]
      orig_quantity <- new_pos[index, Con_FieldName_Qty]
      orig_mktval <- new_pos[index, Con_FieldName_MktVal]
      new_pos[index, Con_FieldName_Qty] <- (orig_quantity + 
                  (fill_side == Con_Side_Buy) * fill_qty
                - (fill_side == Con_Side_Sell) * fill_qty)
      new_pos[index, Con_FieldName_MktVal] <- new_pos[index, Con_FieldName_Qty] * fill_price
      new_pos[index, Con_FieldName_BookVal] <- (new_pos[index, Con_FieldName_BookVal] + 
        ((fill_side == Con_Side_Buy) * fill_qty * fill_price
         - (fill_side == Con_Side_Buy) * fill_qty) * fill_price)

        oc <- Con_OpenClose_Open
        pnl <- NA
        if ((orig_quantity < 0 && fill_side == Con_Side_Buy) || (orig_quantity > 0 && fill_side == Con_Side_Sell)){
          #the execution offsets a closes some positions
          if ((new_pos[index, Con_FieldName_Qty] > 0 && fill_side == Con_Side_Buy) || 
              (new_pos[index, Con_FieldName_Qty] < 0 && fill_side == Con_Side_Sell)){
            #if the execution opens up an position as well
            opposite_pos_qty <- new_pos[index, Con_FieldName_Qty]
            oc = Con_OpenClose_Close
            pnl = -(orig_quantity) * (orig_mktval/orig_quantity - fill_price)
            insert_into_tradesbook(tradesbook, 
                                   time = timestamp,
                                   sym = fill_sym,
                                   side = fill_side,
                                   qty = opposite_pos_qty,
                                   price = fill_price,
                                   openclose = Con_OpenClose_Open,
                                   pnl = NA)
          }
          else{
            oc = Con_OpenClose_Close
            pnl = fill_qty * (orig_mktval/orig_quantity - fill_price)
          }  
        }
        insert_into_tradesbook(tradesbook, 
                               time = timestamp,
                               sym = fill_sym,
                               side = fill_side,
                               qty = opposite_pos_qty,
                               price = fill_price,
                               openclose = oc,
                               pnl = pnl)
    }
  }
  new_pos[new_pos[, Con_FieldName_Sym]== Con_Sym_Cash, Con_FieldName_Qty] <- previous_cash + cash_change
  positionbook[[length(positionbook) + 1]] <- new_pos
  names(positionbook)[length(positionbook)] <- timestamp
  
}

handle_orders <- function (orders, orderbook, marketprice, timestamp){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #handles all orders (new, replace, cancels) and update the order book approriately
  #returns execution messages
  new_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_New,]
  replace_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_Replace,]
  cancel_orders <- orders[orders[,Con_FieldName_MsgType] == Con_MsgType_Cancel,]
  
  mkt_new <- new_orders[new_orders[,Con_FieldName_OrdType] == Con_OrdType_Mkt, ]
  limit_new <- new_orders[new_orders[,Con_FieldName_OrdType] == Con_OrdType_Limit, ]
  
  insert_into_orderbook(limit_new, orderbook)
  exec_replace <- handle_replaces(replace_orders, orderbook, timestamp)
  exec_cancel <- handle_cancels(cancel_orders, orderbook, timestamp)
  #fill must come after replace and cancel has been handled
  exec_fill <- rbind(generate_fill_msgs(mkt_new, marketprice, timestamp), update_orderbook(marketprice, orderbook, timestamp))
  
  return(rbind(exec_replace, exec_cancel, exec_fill))
}

generate_fill_msgs <- function(ready_orders_list, marketprice, timestamp){
  fill_msgs <- data.frame(matrix(0, nrow(ready_orders_list), length(exec_msg_spec)))
  colnames(fill_msgs) <- exec_msg_spec
  fill_msgs[, Con_FieldName_OrdID] <- ready_orders_list[, Con_FieldName_OrdID]
  fill_msgs[, Con_FieldName_ExecStatus] <- Con_ExecStatus_filled
  fill_msgs[, Con_FieldName_Sym] <- ready_orders_list[, Con_FieldName_Sym]
  fill_msgs[, Con_FieldName_Qty] <- ready_orders_list[, Con_FieldName_Qty]
  fill_msgs[, Con_FieldName_AvgPrice] <- marketprice
  fill_msgs[, Con_FieldName_Side] <- ready_orders_list[, Con_FieldName_Side]
  fill_msgs[, Con_FieldName_Time] <- timestamp
  return (fill_msgs)
}

insert_into_orderbook <-function(limit_orders, orderbook){
  #orderbook is a referene (pointer in an environment), and changes are meant to be permanent
  #insert limit orders into orderbook, return nothing
  new_entries <- data.frame((matrix(0, nrow(limit_orders), length(orderbook_spec))))
  colnames(new_entries) <- orderbook_spec
  new_entries[, Con_FieldName_OrdID] <- limit_orders[, Con_FieldName_OrdID]
  new_entries[, Con_FieldName_Time] <- limit_orders[, Con_FieldName_Time]
  new_entries[, Con_FieldName_Sym] <- limit_orders[, Con_FieldName_Sym]
  new_entries[, Con_FieldName_Price] <- limit_orders[, Con_FieldName_Price]
  new_entries[, Con_FieldName_Qty] <- limit_orders[, Con_FieldName_Qty]
  new_entries[, Con_FieldName_Side] <- limit_orders[, Con_FieldName_Side]
  new_entries[, Con_FieldName_OrdType] <- limit_orders[, Con_FieldName_OrdType]
  rbind(orderbook, new_entries)
}

insert_into_tradesbook <- function(tradesbook, time, sym, qty, side, price, openclose, pnl){
  new_line_index <- nrow(tradesbook) + 1
  tradesbook[new_line_index, Con_FieldName_Time] <- time
  tradesbook[new_line_index, Con_FieldName_Sym] <- sym
  tradesbook[new_line_index, Con_FieldName_Side] <- side
  tradesbook[new_line_index, Con_FieldName_Qty] <- qty
  tradesbook[new_line_index, Con_FieldName_Price] <- price
  tradesbook[new_line_index, Con_FieldName_OpenClose] <- openclose
  tradesbook[new_line_index, Con_FieldName_Pnl] <- pnl
}
handle_cancels <- function(cancelorders, orderbook){
  #returns execution messages
  cat(cancelorders)
}

handle_replaces <- function(replaceorders, orderbook){
  #returns execution messages
}

    # ################################################ 