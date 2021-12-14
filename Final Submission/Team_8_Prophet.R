
## Import all the required libraries
library(tidyverse)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(prophet)
# Load data
train <- readr::read_csv("train.csv")
test <- readr::read_csv("test.csv")

# There are 2970 completely empty rows in data so we will remove those rows
#For segement 2 branch id and zone are missing, so we will first deselect those columns and then drop the null values to avoid losing 2nd segemsnt data
raw_data = subset(train, select = -c(branch_id,zone) )
if (sum(is.na(raw_data))!= 0)
{
  data <- na.omit(raw_data)
} else
{
  data <- raw_data
}

#Check unique segement in data
unique(data$segment)

## Convert application date in proper date format
data$application_date <- as.Date(data$application_date, "%d-%m-%Y") 
## Convert application date in proper date format
train$application_date <- as.Date(train$application_date, "%d-%m-%Y") 
test$application_date <- as.Date(test$application_date, "%d-%m-%Y") 

## Splitting train data based on 2 different segemnt 
# splitting dataframe based on segments 1 and 2
train_s1 <- train[which(train$segment == 1),]%>%
  group_by(application_date) 

train_s2 <- train[which(train$segment == 2),]%>%
  group_by(application_date) 
## Splitting test data 
test_s1 <- test[which(test$segment == 1),]

test_s2 <- test[which(test$segment == 2),]

## Subsetting the train and test data for model building and prediction

df_1 = subset(train_s1, select = c(application_date,no_of_applicants) )
df_2 = subset(train_s2, select = c(application_date,no_of_applicants) )

test_s1 = subset(test_s1, select = c(application_date) )
test_s2 = subset(test_s2, select = c(application_date) )

## Building 4 test and train dataset with variable names “ds” and “y” representing the application_date and no of application counts to accommodate prophet
test_s1 <- mutate (
  test_s1,
  ds = application_date,  # Create new ds column from date using mutate
  
)

test_s2 <- mutate (
  test_s2,
  ds = application_date,  # Create new ds column from date using mutate
  
)
test_s1 = subset(test_s1, select = c(ds) )
test_s2 = subset(test_s2, select = c(ds) )

df_1 <- mutate (
  df_1,
  ds = application_date,  # Create new ds column from date using mutate
  y = no_of_applicants   # Create new y column from value using mutate
)

df_2 <- mutate (
  df_2,
  ds = application_date,  # Create new ds column from date using mutate
  y = no_of_applicants   # Create new y column from value using mutate
)


# Prophet Model building
## Prophet Model Building for segement 1 data with weekly and daily sesonality set as true for better forecast plot
m = prophet(df_1,weekly.seasonality=TRUE, daily.seasonality=TRUE)
future = make_future_dataframe(m, periods = 30)
forecast = predict(m, future)

prophet_plot_components(m, forecast)

## predicting the values for test dataset for segment 1 and storing in y_s1
y_s1 <- predict(m, test_s1)

## Adding predicted values for in test dataset where segement is 1 for no. of applicants
test[test$segment==1, 'no_of_applicants'] <- y_s1$yhat



## Prophet Model Building for segement 2 data with weekly and daily sesonality set as true for better forecast plot

# Model building
m_2 = prophet(df_2,weekly.seasonality=TRUE, daily.seasonality=TRUE)
future_2 = make_future_dataframe(m_2, periods = 30, include_history = TRUE)
forecast_2 = predict(m_2, future_2)

## Plotting the forcast values 
prophet_plot_components(m_2, forecast_2)

## Summary of the model
summary(m_2)

## Predicting values for in test dataset where segement is 2 for no. of applicants
y_s2 <- predict(m_2, test_s2)

## length of test data 
nrow(test)

## Adding predicted values for in test dataset where segement is 2 for no. of applicants
test[test$segment==2, 'no_of_applicants'] <- y_s2$yhat

## Saving the test data with predicted values as output_files.csv
write.csv(test, "output_files.csv")


