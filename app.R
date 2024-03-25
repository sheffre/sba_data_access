#shiny trials

library(shiny)
library(bslib)
library(DBI)
library(RPostgres)

# query <- paste0("SELECT * FROM co2_atm_data WHERE timestamp < ", "to_timestamp('",
#                 Sys.time(), "',  'yyyy-mm-dd hh24:mi:ss') AND WHERE timestamp > ",
#                 "to_timestamp('", Sys.time()-86400, "', 'yyyy-mm-dd hh24-mi-ss'")

query_generator <- function(left_border, right_border) {
  query <- paste0("SELECT * FROM co2_atm_data WHERE timestamp <= ", "to_timestamp('",
                  right_border, "',  'yyyy-mm-dd hh24:mi:ss') AND WHERE timestamp >= ",
                  "to_timestamp('", left_border, "', 'yyyy-mm-dd hh24-mi-ss'")
  return(query)
}



getter <- function() {
  conn <- dbConnect(drv = RPostgres::Postgres(),
                    host     = '81.31.246.77',
                    user     = 'yukhoroshilov',
                    password = '0nXX2p9H\v5IY-',
                    dbname   = "default_db")
  
  query <- switch(method, 
                  "daily" = query_generator(right_border = Sys.time(), 
                                            left_border = Sys.time()-(1*86400)),
                  "weekly" = query_generator(right_border = Sys.time(), 
                                             left_border = Sys.time()-(7*86400)),
                  "personal" = query_generator(right_border = ,
                                               left_border = ))
}

ui <- fluidPage(
  
  
  
)

server <- function(input, output) {
  
}

shinyApp(ui, server)