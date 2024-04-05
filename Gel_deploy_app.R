#shiny trials
required_packages <- c("shiny",
                       "bslib",
                       "DBI",
                       "RPostgres",
                       "lubridate",
                       "ggplot2")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

lapply(required_packages, install_if_missing)

library(shiny)
library(bslib)
library(DBI)
library(RPostgres)
library(lubridate)
library(ggplot2)

# query <- paste0("SELECT * FROM co2_atm_data WHERE timestamp < ", "to_timestamp('",
#                 Sys.time(), "',  'yyyy-mm-dd hh24:mi:ss') AND WHERE timestamp > ",
#                 "to_timestamp('", Sys.time()-86400, "', 'yyyy-mm-dd hh24-mi-ss'")

query_generator <- function(right_border, left_border) {
  query <- paste0("SELECT co2_partial_pressure, timestamp FROM co2_atm_data WHERE timestamp <= ", "to_timestamp('",
                  right_border, "',  'yyyy-mm-dd hh24:mi:ss') AND timestamp >= ",
                  "to_timestamp('", left_border, "', 'yyyy-mm-dd hh24-mi-ss') ORDER BY timestamp DESC")
  return(query)
}


getter <- function(listed_con) {
  conn <- dbConnect(drv = listed_con$drv,
                    host     = listed_con$host,
                    user     = listed_con$user,
                    password = listed_con$password,
                    dbname   = listed_con$dbname)
  
  query <- query_generator(right_border = as.POSIXct(paste0(as.character(Sys.Date()), " 23:59:59")),
                           left_border = as.POSIXct(paste0(as.character(Sys.Date()), " 00:00:00")))
  
  df <- dbGetQuery(conn, query)
  return(df)
}

plotter <- function(ls, method) {
  switch(method,
         "sec" = return({
            ggplot(data = ls$df, mapping = aes(x = ls$df$timestamp,
                                                 y = ls$df$co2_partial_pressure)) +
            geom_point() +
            geom_line() +
            scale_x_continuous(name = "Дата и время",
                              limits = c(min(ls$df$timestamp), max(ls$df$timestamp)),
                              breaks = round_date(ls$df$timestamp, "1 hour")) +
            scale_y_continuous(name = "Содержание CO2, ppm",
                               limits = c(360, 500),
                               breaks = seq(360, 500, 10)) 
           }),
         "5min" = return({
             ggplot(data = ls$df_5min, mapping = aes(x = ls$df_5min$timestamp_rounded,
                                                  y = ls$df_5min$co2_partial_pressure)) +
             geom_point() +
             geom_line() +
             scale_x_continuous(name = "Дата и время",
                                limits = c(min(ls$df_5min$timestamp), max(ls$df_5min$timestamp)),
                                breaks = round_date(ls$df_5min$timestamp, "1 hour")) +
             scale_y_continuous(name = "Содержание CO2, ppm",
                                limits = c(360, 500),
                                breaks = seq(360, 500, 10)) 
         })
  )
}

processor <- function(df) {
  df$timestamp <- as.POSIXct(df$timestamp, tz = "Europe/Moscow")
  df$timestamp_char <- as.character(round_date(df$timestamp, unit = "second"))
  
  df$timestamp_rounded = round_date(df$timestamp, "5 minutes")
  df_5min <- aggregate(co2_partial_pressure ~ timestamp_rounded, data = df, FUN = mean)
  ls <- list(df = df, df_5min = df_5min)
  return(ls)
}



ui <- fluidPage(
  titlePanel("Просмотр данных газоанализатора SBA-5"),
  radioButtons("avg", "Выберите тип осреднения:",
                   c("1 секунда" = "sec",
                     "5 минут" = "5min")),
  fluidRow(
    plotOutput('plot', height = "500px"),
    column(width = 4,
           tableOutput('table'),
           style = "height:300px; overflow-y: scroll")
    )
)



# listed_conn <- list(drv = RPostgres::Postgres(),
#                     host     = '81.31.246.77',
#                     user     = 'shinytest',
#                     password = "17082002asxc",
#                     dbname   = "default_db")

server <- function(input, output) {
  listed_conn <- list(drv = RPostgres::Postgres(),
                     host     = 'localhost',
                     user     = 'shinytest',
                     password = "shinytest",
                     dbname   = "postgres")
  
  
  conn <- dbConnect(drv = listed_conn$drv,
                    host     = listed_conn$host,
                    user     = listed_conn$user,
                    password = listed_conn$password,
                    dbname   = listed_conn$dbname)
  on.exit(dbDisconnect(conn), add = TRUE)
  
  ls <- processor(getter(listed_conn))
  
  observe({
    invalidateLater(10000)
    ls <- processor(getter(listed_conn))
    output$plot <- renderPlot(plotter(ls, method = input$avg))
    output$table <- renderTable({
      ls$df[c(1:60), c(1,3)]
    })
  })
  
  output$plot <- renderPlot(plotter(ls, method = input$avg), height = 600, res = 96)
  
  output$table <- renderTable({
    ls$df[c(1:60), c(1,3)]
    })
}

shinyApp(ui, server)
