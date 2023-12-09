
#Load library

library("caret")
library("tidymodels")
library("tidyverse")
library("lubridate")
library("chron")
library("randomForest")
library("ranger")
library("ggmap")
library("tree")


#read creel data

creel_data <- read.csv("C:/Users/kelcy/Documents/GitHub/ML data/NE count data.csv")

#US holidays

# Assuming count_data$Date is in Date format
creel_data$Date <- as.Date(creel_data$Date)

us_holidays <- as.Date(c(
  "2014-01-01", "2014-02-17", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-25",
  "2015-01-01", "2015-02-16", "2015-05-25", "2015-07-04", "2015-09-07", "2015-10-12", "2015-11-11", "2015-11-26", "2015-12-25",
  "2016-01-01", "2016-02-15", "2016-05-30", "2016-07-04", "2016-09-05", "2016-10-10", "2016-11-11", "2016-11-24", "2016-12-25",
  "2017-01-01", "2017-02-20", "2017-05-29", "2017-07-04", "2017-09-04", "2017-10-09", "2017-11-11", "2017-11-23", "2017-12-25",
  "2018-01-01", "2018-02-19", "2018-05-28", "2018-07-04", "2018-09-03", "2018-10-08", "2018-11-11", "2018-11-22", "2018-12-25",
  "2019-01-01", "2019-02-18", "2019-05-27", "2019-07-04", "2019-09-02", "2019-10-14", "2019-11-11", "2019-11-28", "2019-12-25",
  "2020-01-01", "2020-02-17", "2020-05-25", "2020-07-04", "2020-09-07", "2020-10-12", "2020-11-11", "2020-11-26", "2020-12-25",
  "2021-01-01", "2021-02-15", "2021-05-31", "2021-07-04", "2021-09-06", "2021-10-11", "2021-11-11", "2021-11-25", "2021-12-25",
  "2022-01-01", "2022-02-21", "2022-05-30", "2022-07-04", "2022-09-05", "2022-10-10", "2022-11-11", "2022-11-24", "2022-12-25",
  "2023-01-01", "2023-02-20", "2023-05-29", "2023-07-04", "2023-09-04", "2023-10-09", "2023-11-11", "2023-11-23", "2023-12-25"
))

## CLEAN DATA

# Add a new column "day_type" with weekday = 1, weekend = 2, holiday = 3.
#remove dates between November-March ("summer" season only)
#remove columns cd_Date 

creel_data <- creel_data %>%
  mutate(day_type = case_when(
    wday(Date) %in% 2:6 ~ 1,        # Weekdays Monday=2
    wday(Date) %in% c(1, 7) ~ 2))  %>%  # Weekends, Sundays= 1  
  filter(creel_data$Month %in% c(4, 5, 6, 7, 8, 9, 10)) %>%
  select(-cd_Date) 

creel_data <- creel_data %>% 
  mutate(day_type = ifelse(Date %in% us_holidays, 2, day_type)) %>%
  select( -c(c_CountTime,Date, Time, c_AnglerBoats, c_BankAnglers))


##SPLIT DATA

creel_split <-initial_split(creel_data, prop = 0.8)
creel_split

#create training and testing sets

creel_train <- training(creel_split)

creel_test <- testing(creel_split)

#verification

nrow(creel_train)/nrow(creel_data)

## PREDICT

#training a random forest

spec <- rand_forest(trees= 500) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance= "impurity") 
  

rf_model <- spec %>%
  fit(Total_Anglers ~., data= creel_train)

# Make predictions on the test set
predictions <- predict(rf_model, new_data = creel_test)

# Evaluate the model
mse <- mean((predictions$.pred - creel_test$Total_Anglers)^2)
mae <- mean(abs(predictions$.pred - creel_test$Total_Anglers))

cat("Mean Squared Error:", mse, "\n")
cat("Mean Absolute Error:", mae, "\n")


 plot(creel_test$Total_Anglers,predictions$.pred,pch=19,cex=0.5)

 
 ## Take Two
 
 rand_mod <- randomForest(Total_Anglers ~., data= creel_train, trees= 500, type= "regression", importance = TRUE )   
 
 varImpPlot(rand_mod, sort = FALSE, main = "Test VarImp")
 
 pred_2 <- predict(rand_mod, newdata = creel_test)
 
 mse <- mean(rand_mod$mse)
 mse
 mae <- mean(abs(pred_2$.pred - creel_test$Total_Anglers))

# Variable Importance Plot for ranger models
 importance_values <- importance(rf_model$fit)
 importancePlot(importance_values, main = "Variable Importance Plot") 
 

#importance plots

varImpPlot(rf_model, sort=FALSE, main="Variable Importance Plot")




#how continuous variables affect the number of anglers you see (time)


#tuning hyper parameters 







#pull out random lakes, how well does it perform with lakes out of sample
#excluding one lake at a time 
unique(creel_data$WaterbodyName)


# List of lakes to exclude
lakes_to_exclude <- unique(creel_data$WaterbodyName)

# Initialize an empty list to store results
results_list <- list()

# Loop over each lake
for (lake in lakes_to_exclude) {
  cat("Excluding lake:", lake, "\n")
  
  # Filter data for the current lake
  creel_data_filtered <- creel_data %>% 
    filter(WaterbodyName != lake)
  
  # Split data
  creel_split <- initial_split(creel_data_filtered, prop = 0.8)
  
  # Create training and testing sets
  creel_train <- training(creel_split)
  creel_test <- testing(creel_split)
  
  # Verify training set proportion
  cat("Training set proportion:", nrow(creel_train)/nrow(creel_data_filtered), "\n")
  
  # Train a random forest model
  spec <- rand_forest(trees = 500) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  rf_model <- spec %>%
    fit(Total_Anglers ~ ., data = creel_train)
  
  # Make predictions on the test set
  predictions <- predict(rf_model, new_data = creel_test)
  
  # Evaluate the model
  mse <- mean((predictions$.pred - creel_test$Total_Anglers)^2)
  mae <- mean(abs(predictions$.pred - creel_test$Total_Anglers))
  
  cat("Mean Squared Error:", mse, "\n")
  cat("Mean Absolute Error:", mae, "\n")
  
  # Store the results in the list
  results_list[[lake]] <- list(
    "model" = rf_model,
    "predictions" = predictions,
    "mse" = mse,
    "mae" = mae
  )
}

# Access results for a specific lake (e.g., "Calamus Reservoir")
results_Calamus <- results_list[["Calamus Reservoir"]]



library(ggplot2)

# Initialize an empty list to store plots
plots_list <- list()

# Loop over each lake
for (lake in lakes_to_exclude) {
  cat("Plotting results for lake:", lake, "\n")
  
  # Access results for the current lake
  results <- results_list[[lake]]
  
  # Extract actual and predicted values
  actual_values <- creel_test$Total_Anglers
  predicted_values <- predict(rf_model, new_data = creel_test)
  
    # Create a data frame for plotting
 
  plot_data <- data.frame(actual_values = actual_values, predicted_values = predicted_values)
  
  # Create a scatter plot
  plot <- ggplot(plot_data, aes(x = actual_values, y = predictions)) +
    geom_point() +
    ggtitle(paste("Scatter Plot for", lake))
  
  # Store the plot in the list
  plots_list[[lake]] <- plot
}

# Access and print a specific plot (e.g., for "Calamus Reservoir")
plots_list[["Calamus Reservoir"]]

cat("Length of actual values:", length(actual_values), "\n")
cat("Length of predicted values:", length(predicted_values), "\n")
