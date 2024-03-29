#getter tests
query_generator <- function(right_border, left_border) {
  query <- paste0("SELECT co2_partial_pressure, timestamp FROM co2_atm_data WHERE timestamp <= ", "to_timestamp('",
                  right_border, "',  'yyyy-mm-dd hh24:mi:ss') AND timestamp >= ",
                  "to_timestamp('", left_border, "', 'yyyy-mm-dd hh24-mi-ss')")
  return(query)
}


getter <- function(method) {
  conn <- dbConnect(drv = RPostgres::Postgres(),
                    host     = '81.31.246.77',
                    user     = 'shinytest',
                    password = "17082002asxc",
                    dbname   = "default_db")
  
  query <- switch(method, 
                  "daily" = query_generator(right_border = Sys.time(), 
                                            left_border = Sys.time()-(1*86400)),
                  "weekly" = query_generator(right_border = Sys.time(), 
                                             left_border = Sys.time()-(7*86400))
                  #,"personal" = query_generator(right_border = ,
                  #                              left_border = ))
  )
  
  df <- dbGetQuery(conn, query)
  return(df)
}

conn <- dbConnect(drv = RPostgres::Postgres(),
                  host     = '81.31.246.77',
                  user     = 'shinytest',
                  password = "17082002asxc",
                  dbname   = "default_db")


df_daily <- getter("daily")

df_weekly <- getter("weekly")
# SELECT * 
# FROM co2_atm_data 
# WHERE timestamp <= to_timestamp('2024-03-25 18:21:22.27788',  'yyyy-mm-dd hh24:mi:ss') 
# AND WHERE timestamp >= to_timestamp('2024-03-24 18:21:22.277888', 'yyyy-mm-dd hh24-mi-ss'