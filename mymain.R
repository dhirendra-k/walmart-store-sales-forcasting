##############################################################################################
# References
# 1.) https://liangfgithub.github.io/Example_Code_Project2_Josh.html
# 2.) https://github.com/davidthaler/Walmart_competition_code/blob/master/grouped.forecast.R
##############################################################################################



rm(list = ls())
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  "lubridate",
  "forecast",
  "tidyverse",
  "dplyr"
)

# converts a Date x num_store forecast to a dataframe
# with Date, Store, value = Weekly_Price columns
flatten_forecast <- function(f_model) {
  f_model %>%
    gather(Store, value, -Date, convert = TRUE)
}

#tuning based on test results, 8 components seems to work best for this dataset
#Implementation taken from David Thaler's Walmart grouped.forecast github file (reference 2)
preprocess.svd <- function(train, n.comp){
  train[is.na(train)] <- 0
  z <- svd(train[, 2:ncol(train)], nu=n.comp, nv=n.comp)
  s <- diag(z$d[1:n.comp])
  train[, 2:ncol(train)] <- z$u %*% s %*% t(z$v)
  train
}

#returns time series for each store in the second passed argument dataframe. Each store ts is a column
#built off of David Thaler's github code (reference 2)
tslm.model <- function (train_ts, test_ts) {
  train_ts <- preprocess.svd(train_ts, 8)
  #for each store j in train timeseries for given department
  for (j in 2:ncol(train_ts)) {
    store_ts <- ts(train_ts[,j], frequency = 52)
    model <- tslm(store_ts ~ trend + season)
    test_ts[,j] <- forecast(model, newdata = test_ts[,1])$mean
  }
  return(test_ts)
}

#Function used from Joshua Loyal's sample code (reference 1)
#For updating the prediction dataframe which will be returned by mypredict
update_forecast <- function(test_month, dept_preds, dept) {
  dept_preds <- flatten_forecast(dept_preds)
  
  pred.d <- test_month %>%
    filter(Dept == dept) %>%
    select('Store', 'Date') %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  pred.d.idx <- test_month$Dept == dept
  pred.d <- test_month[pred.d.idx, c('Store', 'Date')] %>%
    left_join(dept_preds, by = c('Store', 'Date'))
  
  test_month$Weekly_Pred[pred.d.idx] <- pred.d$value
  test_month
}

#returns column of predictions
#variables 'train', 'test', and 't' are global parameters in testing environment
#if t>1, update train with new_train data with '<<-' operator.
#Built off of Joshua Loyal's sample code (reference 1)
mypredict = function() {
  #add new fold train data to train global var
  if (t > 1) {
    train <<- train %>% add_row(new_train)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_month <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  # Dates are not the same across months!
  test_dates <- unique(test_month$Date)
  num_test_dates <- length(test_dates)
  
  # Not all stores may need predictions either
  all_stores <- unique(test_month$Store)
  num_stores <- length(all_stores)
  
  # Most importantly not all departments need predictions
  test_depts <- unique(test_month$Dept)
  
  # Dateframe with (num_test_dates x num_stores) rows
  test_frame <- data.frame(
    Date=rep(test_dates, num_stores),
    Store=rep(all_stores, each=num_test_dates)
  )
  
  # Create the same dataframe for the training data
  # (num_train_dates x num_stores)
  train_dates <- unique(train$Date)
  num_train_dates <- length(train_dates)
  train_frame <- data.frame(
    Date=rep(train_dates, num_stores),
    Store=rep(all_stores, each=num_train_dates)
  )
  
  #### Perform a individual forecasts for each department
  # for (dept in 1:1) {
  for (dept in test_depts) {
    
    # filter for the particular department in the training data
    train_dept_ts <- train %>%
      filter(Dept == dept) %>%
      select(Store, Date, Weekly_Sales)
    
    # Reformat so that each column is a weekly time-series for that
    # store's department.
    # The dataframe has a shape (num_train_dates, num_stores)
    train_dept_ts <- train_frame %>%
      left_join(train_dept_ts, by = c('Date', 'Store')) %>%
      spread(Store, Weekly_Sales)
    
    # We create a similar dataframe to hold the forecasts on
    # the dates in the testing window
    #initialize with 0 values (rows are forecasts, 46 columns for each store and 1st is date)
    test_dept_ts <- test_frame %>%
      mutate(Weekly_Sales = 0) %>%
      spread(Store, Weekly_Sales)
    
    f_tslm <- tslm.model(train_dept_ts, test_dept_ts)
    test_month <- update_forecast(test_month, f_tslm, dept)
  }
  return(test_month)
}