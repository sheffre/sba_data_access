```
query_generator <- function(left_border, right_border) {
  query <- paste0("SELECT * FROM co2_atm_data WHERE timestamp <= ", "to_timestamp('",
                  right_border, "',  'yyyy-mm-dd hh24:mi:ss') AND WHERE timestamp >= ",
                  "to_timestamp('", left_border, "', 'yyyy-mm-dd hh24-mi-ss'")
  return(query)
}
```
Процедура принимает временные границы на вход, формирует из них SQL-запрос и возвращает его.
Процедура реализована в авто-обработчике данных.