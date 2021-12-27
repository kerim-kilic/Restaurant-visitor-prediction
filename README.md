# Restaurant visitor prediction

The **visitor_prediction.R** script in this repository analyzes restaurant data from January 2017 to March 2017 of restaurants in Japan to make predictions for April 2017 using machine learning models. The script includes the creation of test and training datasets, training the machine learning model, and making a prediction for April 2017.

## Requirements

The R script requires the following packages:

```r
library(RMySQL)
library(stringr)
library(plyr)
library(dplyr)
library(tidyverse)
```

In order to set up the database you have to run the **project.sql** file, for running SQL files I use MySQL Workbench. After setting up the database you can query and extract data in the R script.

## Guide

In the **get_data()** function change the parameters of the **dbConnect()** with the credentials to access the database, the name of the database and the host. After setting these up you should be able to query and extract data from the database.

```r
  mydb = dbConnect(
    MySQL(), 
    user="root",
    password="password",
    dbname="ITDAproject",
    host="localhost"
  )
```
