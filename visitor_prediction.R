library(RMySQL)

get_data <- function(table)
{
  mydb = dbConnect(
    MySQL(), 
    user="root",
    password="password",
    dbname="ITDAproject",
    host="localhost"
  )
  
  air_reserve = dbSendQuery(mydb, "SELECT * FROM air_reserve")
  air_reserve_data = fetch(air_reserve, n=-1)
  
  air_visit = dbSendQuery(mydb, "SELECT * FROM air_visit")
  air_visit_data = fetch(air_visit, n=-1)
  
  date_info = dbSendQuery(mydb,"SELECT * FROM date_info")
  date_info_data = fetch(date_info, n=-1)
  
  restaurant_info = dbSendQuery(mydb, "SELECT * FROM restaurant_info")
  restaurant_info_data = fetch(restaurant_info, n=-1)
  
  dbDisconnect(mydb)
  
  result = switch(  
    table,  
    "air_reserve"= return(air_reserve_data),  
    "air_visit"= return(air_visit_data),  
    "date_info"= return(date_info_data),  
    "restaurant_info"= return(restaurant_info_data)
  )
}

air_reserve_data <- get_data("air_reserve")
air_visit_data <- get_data("air_visit")
date_info_data <- get_data ("date_info")
restaurant_info_data <- get_data("restaurant_info")
