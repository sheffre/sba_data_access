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
  
  # query <- query_generator(right_border = as.POSIXct(paste0(as.character(Sys.Date()), " 23:59:59", origin = "1970-01-01")),
  #                          left_border = as.POSIXct(paste0(as.character(Sys.Date()), " 00:00:00", origin = "1970-01-01")))
  
  query <- "SELECT co2_partial_pressure, timestamp
            FROM co2_atm_data
WHERE timestamp >= (DATE_TRUNC('day', CURRENT_TIMESTAMP AT TIME ZONE 'GMT+3') - INTERVAL '3 hours')
      AND timestamp AT TIME ZONE 'GMT+3' < DATE_TRUNC('day', ((CURRENT_TIMESTAMP + INTERVAL '1 day')) AT TIME ZONE 'GMT+3') ORDER BY timestamp DESC;" 
  
  df <- dbGetQuery(conn, query)
  df$timestamp <- as.POSIXct(df$timestamp, tz = "Europe/Moscow")
  return(df)
}

processor <- function(df) {
  # df$timestamp <- as.POSIXct(df$timestamp, tz = "Europe/Moscow")
  df$timestamp_char <- as.character(round_date(df$timestamp, unit = "second"))
  
  df$timestamp_rounded = round_date(df$timestamp, "5 minutes")
  df_5min <- aggregate(co2_partial_pressure ~ timestamp_rounded, data = df, FUN = mean)
  ls <- list(df = df, df_5min = df_5min)
  return(ls)
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

plotter_plotly <- function(method, ls) {
  switch(method,
         "sec" = {fig <- plot_ly(ls$df, type = "scatter", mode = "lines") %>%
           add_trace(x = ~timestamp, y = ~co2_partial_pressure, name = 'Содержание CO2', color = "black")%>%
           layout(showlegend = F)
         
         fig <- fig %>%
           layout(
             title = "Суточный ход содержания CO2",
             xaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff',
                          title = "Дата и время"
             ),
             yaxis = list(zerolinecolor = '#ffff',
                          zerolinewidth = 2,
                          gridcolor = 'ffff',
                          title = "Парциальное давление CO2, ppm"
             ),
             plot_bgcolor='#e5ecf6')
         
         
         },
         "5min" = {
           fig <- plot_ly(ls$df_5min, type = "scatter", mode = "lines") %>%
             add_trace(x = ~timestamp_rounded, y = ~co2_partial_pressure, name = 'Содержание CO2', color = "black")%>%
             layout(showlegend = F)
           
           fig <- fig %>%
             layout(
               title = "Суточный ход содержания CO2",
               xaxis = list(zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff',
                            title = "Дата и время"
               ),
               yaxis = list(zerolinecolor = '#ffff',
                            zerolinewidth = 2,
                            gridcolor = 'ffff',
                            title = "Парциальное давление CO2, ppm"
               ),
               plot_bgcolor='#e5ecf6')
         }
         )
  return(fig)
}



ui <- fluidPage(
  titlePanel("Просмотр данных газоанализатора SBA-5"),
  radioButtons("avg", "Выберите тип осреднения:",
                   c("1 секунда" = "sec",
                     "5 минут" = "5min")),
  fluidRow(
    plotlyOutput('plot', height = "500px"),
    column(tite = "Текущие значения содержания CO2", 
           width = 4,
           tableOutput('table'),
           style = "height:300px; overflow-y: scroll"),
    page_fillable(
      card("Row 1"),
      card("Row 2"),
      card("Row 3")
    )
    )
)



# listed_conn <- list(drv = RPostgres::Postgres(),
#                     host     = '81.31.246.77',
#                     user     = 'shinytest',
#                     password = "17082002asxc",
#                     dbname   = "default_db")

server <- function(input, output) {
  listed_conn <- list(drv = RPostgres::Postgres(),
                     host     = '81.31.246.77',
                     user     = 'shinytest',
                     password = "17082002asxc",
                     dbname   = "default_db")
  
  
  conn <- dbConnect(drv = listed_conn$drv,
                    host     = listed_conn$host,
                    user     = listed_conn$user,
                    password = listed_conn$password,
                    dbname   = listed_conn$dbname)
  on.exit(dbDisconnect(conn), add = TRUE)
  
  ls <- processor(getter(listed_conn))
  
  observe({
    invalidateLater(5000)
    ls <- processor(getter(listed_conn))
    # output$plot <- renderPlot(plotter(ls, method = input$avg))
    output$table <- renderTable({
      ls$df[c(1:60), c(1,3)]
    })
  })
  
  output$plot <- renderPlotly(plotter_plotly(ls, method = input$avg))
  
  output$table <- renderTable({
    ls$df[c(1:60), c(1,3)]
    })
}

shinyApp(ui, server)
