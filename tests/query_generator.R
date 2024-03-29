#testing query_generator
query_generator <- function(right_border, left_border) {
  query <- paste0("SELECT * FROM co2_atm_data WHERE timestamp <= ", "to_timestamp('",
                  right_border, "',  'yyyy-mm-dd hh24:mi:ss') AND WHERE timestamp >= ",
                  "to_timestamp('", left_border, "', 'yyyy-mm-dd hh24-mi-ss'")
  return(query)
}

t1 <- query_generator(Sys.time(), Sys.time()-86400)

t2 <- query_generator(Sys.time(), Sys.time()-(7*86400))

t1

t2
