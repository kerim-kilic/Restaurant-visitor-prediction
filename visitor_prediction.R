library(RMySQL)
library(stringr)
library(plyr)

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

create_training_data <- function()
{
  # Create function to generate training data
}

create_test_data <- function()
{
  prediction_format <- read.csv("project_submission.csv",sep=",")
  test <- str_split_fixed(prediction_format$ID, " _ ", 2)
  b <- data.frame(test[,1], test[,2])
  colnames(b) <- c("rest_id","calendar_date")
  date_info_data <- get_data("date_info")
  test <- join(b, date_info_data,type = "left")
  colnames(test) <- c("ID","visit_date","day_of_week","holiday_flg")
  test$visitors <- rep(0, times=15770)
  test$visit_date <- as.Date(test$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  return(test)
}

generate_predictions <- function()
{
  # Create function to generate predictions
}

air_reserve_data <- get_data("air_reserve")
air_visit_data <- get_data("air_visit")
date_info_data <- get_data ("date_info")
restaurant_info_data <- get_data("restaurant_info")
