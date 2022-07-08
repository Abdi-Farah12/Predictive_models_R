# Demonstration: Time-series analysis in R

# Import libraries for time series analysis:

install.packages('forecast')
library(ggplot2)
library(forecast)
library(tseries)

# Import data set and assign to new object:

FRED_base <- read.csv (file.choose (), header = T)

# [1] Sense-check the new data set:

summary(FRED_base)

#2. Change the column names

#Change the values in the DATE column to date values as opposed to characters.
#and then for column names to 'Date' and 'Value'.

# [1] Specify the column, change the variables, and format the date:
FRED_base$DATE <- as.Date(FRED_base$DATE,format = '%d/%m/%Y')

#[2] Specify the new columns.
colnames(FRED_base) <-c('Date', 'Value')

head(FRED_base)

# 3. Create time-series objects

#timeseries.naem <- ts(date, start, end, frequency)

# [1, 2] Specify the min and max functions and set the parameters:

min(FRED_base$Date, na.rm = TRUE)
max(FRED_base$Date, na.rm = TRUE)

# [1] Create new data frame and assign time-series value, and [2] specify the 'ts' function:

FRED_ts <- ts(FRED_base$Value, start = c(1972,1), end = c(2021,12), frequency = 12)


head(FRED_ts)  # [3] Sense-check the new object.
str(FRED_ts)   # [4] Check the object's structure.

# 4. Check for missing values and the frequency

sum(is.na(FRED_ts))  # [1] Check for missing values. 
frequency(FRED_ts)  # [2] Check the frequency of the data.

cycle(FRED_ts)  # Show the datapoint's position in the cycle.
# the cycle output shows that the observations are numbered 1-12

#6. Display the data/check for outliers

boxplot(FRED_ts~cycle(FRED_ts), xlab ='Month',ylab = 'Value',
        main = 'Months from 1972 to 2022')

#The boxplot suggests that the mean and variance is noticeably higher from October (10) through to December (12) every year. 

#Each observed data point in a time series can be expressed as either a sum or a product of three components: 
#seasonality, trend, and error or random/irregular. We can separate these by decomposing the time series.

#7. Decompose the data

# Extract and plot the main components to decompose the time series:

components_FRED_ts <- decompose(FRED_ts)
plot(components_FRED_ts) 

# We can make some inferences from this plot, namely, that we don’t appear to have a clear trend in series, and that seasonality appears to be clear. 

# 1.time-series data (top)
# 2. the trend values (second from top)
# 3. the seasonal values (second from bottom)
# 4. random values (bottom)


#########################################################################################################################################
#########################################################################################################################################

#In the first part of this worked example, we loaded and cleaned our data set, created time-series objects in R, 
#and decomposed our time-series data. Next, we’ll focus on testing our data for stationarity and autocorrelation, 
#and then creating our model, making our predictions, and testing our outputs. 

#### 2. TESTING FOR STATIONARITY AND AUTOCORRELATION

# 1. Testing stationarity

##To reject the null hypothesis in an ADF test (i.e. that there is no unit root) 
#the test needs to return a p-value below 5% (or 0.05). We use a function, ADF.test(), 
#on our time series data, FRED_ts, to return a result. 

# Test stationarity with augmented ADF test:

adf.test(FRED_ts)

#Our p-value is 0.01 and therefore less than 5% (0.05).
#We can reject the null hypothesis and conclude that our time series data is stationary. 
#We can now feel confident to make some predictions.

# 2. Testing autocorrelation

# To plot ACF with our time series, we first need to account for any NA values. 
#We review the random time-series variables for any NA values.

components_FRED_ts$random  # [1] Review random time-series variables.

# There are NA values in 1972 and 2021

# Since there are NA values, we need to exclude them from our plot. 
#To do this, we:

autoplot(acf(na.remove(components_FRED_ts$random),plot=FALSE)) + 
  labs(title="Randomness value") + # [3] Add a title.
  theme_classic()  # [4] Set the theme.

#Recall that each vertical line is an autocorrelation measure.
#When it crosses the blue dotted line, it means that that specific lag 
#is significantly correlated with the current time series. In our ACF plot the 
#vertical bars drop quickly to zero or below, suggesting that our residuals are not autocorrelated. 

# We can also plot the random variables on a histogram to check distribution.

# Plot random variables to check the distribution:

hist((components_FRED_ts$random))

#The shape of the histogram suggests a normal distribution. We know that the data we have is a function of time,
# and can proceed with fitting a model and making forecasts. 

# 3. Predicting with the ARIMA model

#We’ll use the autoregressive integrated moving average (ARIMA) model to make our predictions. 
#Recall that ARIMA models essentially explain a time series based on the time series's own past values.
#In other words, ARIMA models take correlations in the data into account. They are among the most widely 
#used algorithms for time-series forecasting. 

# [1] Fit the model to our time series: 

arima_FRED_ts <- auto.arima(FRED_ts)

# [2] Make a forecast for the next three months:

forecast3_FRED_ts <- forecast(arima_FRED_ts, 3)

# [3] Plot the forecast on a graph:

autoplot(forecast3_FRED_ts) + theme_classic()

#The small blue line at the far right side of the plot shows the estimated 
#production outputs for the next three months, with confidence 
#intervals of 80% (lighter blue) and 95% (darker blue).

#We can see these values in a data frame too.
forecast3_FRED_ts  # Print the values in the data frame.

# Let’s extend our prediction to two years into the future. To do this, we:

# [1] Extend the prediction; set the data source, the time span, and assign a new object:

forecast2_FRED_ts <- forecast(arima_FRED_ts, 24)

# [2] Plot the output and set the theme: 
autoplot(forecast2_FRED_ts) + theme_classic()

#The blue line and shading at the far right side of the plot shows the estimated 
#values for the next two years or 24 months, with confidence intervals of 
#80% (lighter blue) and 95% (darker blue). 

#We can see these values in a data frame.
forecast2_FRED_ts  # Print the values in the data frame

# 4. Testing for accuracy
# we need to split our data into test and train sets 

# 1.Extract a subset of the values

# We will extract the last 12 months of the data as a test set using window() function

# [1] (Training data) Create a new time series object and assign the values and parameters:

FRED_train_ts = window(FRED_ts, start = c(1972,1), 
                       end = c(2020,12), frequency = 12)

# [2] (Test data) Create a new time series object and assign the values and parameters:

FRED_test_ts = window(FRED_ts, start = c(2021,1), 
                      end = c(2021,12), frequency = 12)

# 2. Fit the model to the training data

#Now, let’s fit the ARIMA model to the training series, with the auto.arima() function, 
#and then plot our forecast for the next 12 months on a graph. Note that this is the 
#forecasted values derived from the training data. 
#To do this, we:

# [1] Create a new object and [2] specify the forecast function and pass the ARIMA model:
forecast_FRED_train_ts <- forecast(auto.arima(FRED_train_ts), 12)

# [3] Plot the values and forecast and [4] add a theme:
autoplot(forecast_FRED_train_ts) + theme_classic() 

#The shaded blue section is the fitted values forecast for the 12 months we removed from the time series. 

# 3. Plot the test set

# [1] Plot test set onto the graph: specify the lines function, set the source, and specify the line colour:

lines(FRED_test_ts, col='red')############ The line does not show on the graph#########

#The red line, our plotted test data set values, appear to fit closely together with the blue shaded areas. 

# 4.Check the MAPE value (accuracy)

#We can explicitly check the accuracy of our forecasted values with the accuracy() function in R.
#This function returns a range of summary measures. Bear in mind that a forecast error is the 
#difference between an observed value and its forecasted value. 


#The accuracy function returns the following statistical measures:
  
#ME: Mean error
#RMSE: Root mean error
#MAE: Mean absolute error
#MPE: Mean percentage error
#MAPE: Mean absolute percentage error
#MASE: Mean absolute scaled error
#ACF1: Autocorrelation of errors at lag 1
#Thiel’s U: a relative accuracy measure that compares the forecasted results with the results of forecasting with minimal historical data.

#For example, if the MAPE value of a model is 5%, it infers that the average difference between the forecasted value and the actual value is 5%


# Check the accuracy of the prediction:
accuracy(forecast_FRED_train_ts,FRED_test_ts)

#Let’s focus on our MAPE value for the test set, which sits at 4.59%. This means that the average difference between 
#the predicted value and the actual value is 4.59%%. This is a strong MAPE value and suggests that the model is relatively accurate. 

#Sharing this result with Sweets n Stuff, you can tell them that you have built a time-series predictive model that can strongly
#predict future production output in the confectionery products industry, thereby giving them an opportunity to plan for demand and supply, 
#and keep their inventory well-stocked.


