library(RMySQL)
library(stringr)
library(plyr)
library(dplyr)
library(tidyverse)

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
  air_visit_data <- get_data("air_visit")
  air_reserve_data <- get_data("air_reserve")
  date_info_data <- get_data("date_info")
  air_visit_data <- air_visit_data %>% mutate(n = row_number())
  air_reserve_data <- air_reserve_data %>%mutate(visit_date = as.Date(visit_datetime))
  air_reserve_data_visit <- merge(air_reserve_data %>% select(ID, reserve_visitors, visit_date), air_visit_data,  by=c("ID", "visit_date"))
  air_visit_data_date <- merge(air_visit_data, date_info_data, by.x = 'visit_date', by.y='calendar_date')
  air_visit_data_date$visit_date <- as.Date(air_visit_data_date$visit_date )
  air_visit_data_date_reserve <- left_join(air_visit_data_date, air_reserve_data %>% select(ID, reserve_visitors, visit_date), by= c("ID", "visit_date"))
  air_visit_data_date_reserve[is.na(air_visit_data_date_reserve)] <- 0
  a <- air_visit_data_date_reserve %>%
    group_by(ID, visit_date)%>%
    mutate(reserves = sum(reserve_visitors))%>%
    select(!reserve_visitors)%>%
    unique()
  a$n <- NULL
  a$ID <- as.character(a$ID)
  a$day_of_week <- as.character(a$day_of_week)
  add_zero_values_rest <- c("restaurant_ 292", "restaurant_ 325")
  add_zero_values_date <- c("2017-01-05", "2017-01-05")
  add_zero_values_visitors <- c(0,0)
  add_zero_values_week_day <- c("Thursday", "Thursday")
  add_zero_values_holiday <- c(0,0)
  add_zero_values_reserves <- c(0,0)
  new_data <- data.frame(add_zero_values_date, add_zero_values_rest,add_zero_values_visitors,add_zero_values_week_day,add_zero_values_holiday,add_zero_values_reserves)
  colnames(new_data) <- c("visit_date", "ID", "visitors", "day_of_week", "holiday_flg", "reserves")
  new_data$visit_date <- as.Date(new_data$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  modified_data <- rbind.data.frame(a, new_data)
  return(modified_data)
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
  test$reserves <- rep(0, times=15770)
  test$visit_date <- as.Date(test$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  test$ID <- as.character(test$ID)
  return(test)
}

generate_predictions <- function()
{
  # Create function to generate predictions
}

training_data_set <- create_training_data()
test_data_set <- create_test_data()
model <- lm(visitors~ ID+ visit_date+day_of_week+holiday_flg+reserves, data = training_data_set)
predictions <- predict(model, newdata = test_data_set)
