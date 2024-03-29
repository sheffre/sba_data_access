Основная процедура в приложении: получает данные из СУБД на сервере.

Версия от 25.03.2024 18:16
```
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
```
