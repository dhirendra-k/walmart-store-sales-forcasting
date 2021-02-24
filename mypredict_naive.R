#example instructor naive function which works with evaluation code

mypredict_naive = function(){
  if (t>1){
    train <<- train %>% add_row(new_train)
  }
  
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)
  
  most_recent_date <- max(train$Date)
  tmp_train <- train %>%
    filter(Date == most_recent_date) %>%
    rename(Weekly_Pred = Weekly_Sales) %>%
    select(-Date, -IsHoliday)
  
  test_pred <- test_current %>%
    left_join(tmp_train, by = c('Dept', 'Store')) 
  return(test_pred)
}