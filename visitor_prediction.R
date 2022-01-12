##
##  Final project Information Technologies and Data Analysis course
##  Master in Technology and Engineering Management
##  Universitat Politecnica de Catalunya 
##

library(RMySQL)
library(stringr)
library(plyr)
library(dplyr)
library(tidyverse)

get_data <- function(table)
{
  mydb = dbConnect(
    MySQL(), 
    user="newuser",
    password="Kerim-40",
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

library(RMySQL)
library(tidyverse)

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
  add_zero_values_rest <- c("restaurant_ 4", "restaurant_ 47", "restaurant_ 514", "restaurant_ 516",
                            "restaurant_ 573", "restaurant_ 619", "restaurant_ 650", "restaurant_ 292", 
                            "restaurant_ 325")
  add_zero_values_date <- c("2017-01-05", "2017-01-05", "2017-01-05", "2017-01-05",
                            "2017-01-05", "2017-01-05", "2017-01-05", "2017-01-05",
                            "2017-01-05")
  add_zero_values_visitors <- c(0,0,0,0,0,0,0,0,0)
  add_zero_values_week_day <- c("Thursday", "Thursday", "Thursday", "Thursday", 
                                "Thursday", "Thursday", "Thursday", "Thursday",
                                "Thursday")
  add_zero_values_holiday <- c(0,0,0,0,0,0,0,0,0)
  add_zero_values_reserves <- c(0,0,0,0,0,0,0,0,0)
  new_data <- data.frame(add_zero_values_date, add_zero_values_rest,add_zero_values_visitors,add_zero_values_week_day,add_zero_values_holiday,add_zero_values_reserves)
  colnames(new_data) <- c("visit_date", "ID", "visitors", "day_of_week", "holiday_flg", "reserves")
  new_data$visit_date <- as.Date(new_data$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  modified_data <- rbind.data.frame(a, new_data)
  modified_data$visit_date <- as.Date(modified_data$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  modified_data <- modified_data%>%filter(visit_date < "2017-03-01")
  return(modified_data)
}

create_training_data2 <- function()
{
  air_visit_data <- get_data("air_visit")
  air_reserve_data <- get_data("air_reserve")
  date_info_data <- get_data("date_info")
  restaurant_info_data <- get_data ("restaurant_info")
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
  add_zero_values_rest <- c("restaurant_ 4", "restaurant_ 47", "restaurant_ 514", "restaurant_ 516",
                            "restaurant_ 573", "restaurant_ 619", "restaurant_ 650", "restaurant_ 292", 
                            "restaurant_ 325")
  add_zero_values_date <- c("2017-01-05", "2017-01-05", "2017-01-05", "2017-01-05",
                            "2017-01-05", "2017-01-05", "2017-01-05", "2017-01-05",
                            "2017-01-05")
  add_zero_values_visitors <- c(0,0,0,0,0,0,0,0,0)
  add_zero_values_week_day <- c("Thursday", "Thursday", "Thursday", "Thursday", 
                                "Thursday", "Thursday", "Thursday", "Thursday",
                                "Thursday")
  add_zero_values_holiday <- c(0,0,0,0,0,0,0,0,0)
  add_zero_values_reserves <- c(0,0,0,0,0,0,0,0,0)
  new_data <- data.frame(add_zero_values_date, add_zero_values_rest,add_zero_values_visitors,add_zero_values_week_day,add_zero_values_holiday,add_zero_values_reserves)
  colnames(new_data) <- c("visit_date", "ID", "visitors", "day_of_week", "holiday_flg", "reserves")
  new_data$visit_date <- as.Date(new_data$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  modified_data <- rbind.data.frame(a, new_data)
  modified_data$visit_date <- as.Date(modified_data$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  modified_data <- modified_data%>%filter(visit_date < "2017-03-01")
  modified_data <- merge(modified_data,restaurant_info_data,by="ID")
  return(modified_data)
}

create_test_data <- function()
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
  a$visit_date <- as.Date(a$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  a <- a%>%filter(visit_date > "2017-02-28")
  return(a)
}

create_test_data2 <- function()
{
  air_visit_data <- get_data("air_visit")
  air_reserve_data <- get_data("air_reserve")
  date_info_data <- get_data("date_info")
  restaurant_info_data <- get_data("restaurant_info")
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
  a$visit_date <- as.Date(a$visit_date, format = "%Y-%m-%d", origin = "1970-01-01")
  a <- a%>%filter(visit_date > "2017-02-28")
  a <- merge(a,restaurant_info_data,by="ID")
  return(a)
}

create_prediction_test_data <- function()
{
  prediction_format <- read.csv("prediction_format.csv",sep=",")
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
  air_reserve_data <- get_data("air_reserve")
  air_reserve_data$visit_date <- as.Date(air_reserve_data$visit_datetime )
  test_data_reserves <- left_join(test, air_reserve_data %>% select(ID, reserve_visitors, visit_date), by= c("ID", "visit_date"))
  test_data_reserves[is.na(test_data_reserves)] <- 0
  test_data <- test_data_reserves %>%
    group_by(ID, visit_date)%>%
    mutate(reserves = sum(reserve_visitors))%>%
    select(!reserve_visitors)%>%
    unique()
   return(test_data)
}

generate_predictions <- function(model,test_data)
{
  prediction_format <- read.csv("prediction_format.csv",sep=",")
  predictions <- predict(model, newdata = test_data)
  predictions <- round(predictions)
  prediction_format$visitors <- predictions
  #write.csv(prediction_format,"C:\\Users\\Kerim\\Desktop\\project_format.csv", row.names = FALSE)
  return(prediction_format)
}

air_reserve <- get_data("air_reserve")
air_visit <- get_data("air_visit")
date_info <- get_data("date_info")
restaurant_info <- get_data("restaurant_info")

training_data_set <- create_training_data()

training_data_set <- training_data_set %>%
  group_by(ID, day_of_week)%>%
  mutate(mean = mean(reserves))

model <- lm(visitors ~ visit_date + ID+day_of_week+holiday_flg+reserves+mean, 
            data = training_data_set)

summary(model)


test_data_set <- create_prediction_test_data()


tmp <- generate_predictions(model,test_data_set)

prediction <- predict(model,test_data_set)
summary(model_nacho)

residuals <- test_data_set$visitors - prediction
sse <- sum(residuals^2)
sst <- sum((test_data_set$visitors - mean(test_data_set$visitors))^2)
RSQ <- 1 - (sse/sst)
RSQ

test_data_set <- create_prediction_test_data()

tmp <- generate_predictions(model_nacho,test_data_2)

summary(tmp$visitors)

library(caTools)
split <- sample.split(training_data_set2$visitors, SplitRatio = 0.7)
train_set <- subset(training_data_set2, split==TRUE)
test_set <- subset(training_data_set2, split==FALSE)

model <- lm(visitors ~ ID + visit_date + day_of_week + holiday_flg + reserves +
              air_genre_name + air_area_name + latitude + longitude, data = training_data_set2)
summary(model)

model_nacho <- lm(visitors ~ ID+day_of_week*visit_date*holiday_flg+reserves, data = training_data_set)
prediction <- predict(model_nacho,test_data_set)
summary(model_nacho)

# Model validation
prediction <- predict(model,test_set)

residuals <- test_data_set$visitors - prediction
sse <- sum(residuals^2)
sst <- sum((test_data_set$visitors - mean(test_data_set$visitors))^2)
RSQ <- 1 - (sse/sst)
RSQ

prediction_test_data <- create_prediction_test_data()
predicted_values <- generate_predictions(model,prediction_test_data)
