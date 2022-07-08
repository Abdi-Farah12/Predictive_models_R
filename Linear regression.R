library(tidyverse)

cpi <- read.csv(file.choose(),header=TRUE)

summary(cpi)

# 1. Identify the relationship between the two variables

# first check correlation
cor(cpi$Year,cpi$Index)
#  0.9139685 is a very strong positive correlation

# Check the relationship between the variables by plotting
# we can plot using ggplot or base R graphics. I will use base R graphics
 
plot(cpi$Year, cpi$Index)
 
# as expected we can see an increasing relationship between the 2

# Now lets fit the linear regression 

model1 <- lm(Index~Year, data = cpi) # this is a simple lm model because we only have on x variable

model1
# the output below shows 2 figures:
#a) the intercept =-4548
#b) the coeffcient/slope of year at 2.355 (this means that we expect in the model the index should go up by 2.355 every year)

# by running smmary we get the full regression table
summary(model1)
#including the r-sqaured, t-value and p-value (which are the most important)

# R-squared is 0.835 which means that the year as the independent variable
# explains 83.5% of teh variablity in the index variable

# We also want to look at the residuals which is the difference between the actual values and 
#the values predicted by the model. 

#There shouldnt be a  pattern in the residuals and we will test this by plotting

plot(model1$residuals)


#There seems to be a pattern here as the  values drop until 60 then start to rise again.

# By fitting a line of best fit on the graph we can see that its not a great fit
# Its not a linear relationship
abline(coefficients(model1))

# One of the things we can do here is take a log transformation
# we add new column using mutate called logindex which is a log(Index)
cpi2 <- mutate(cpi,
              logindex = log(Index))

# Lets now fit a new model where the y varibale is the log index

model2 <- lm(Year~logindex, data = cpi2)

summary(model2)

#We can see that the r-sqaured is much higher than the first model but we cant compare it because the scale has changed as this is a log model
# But we can instead check the fit of this model

# create plot of new model and fit lin using abline



ggplot(data = model2, aes(x = Year, y = logindex)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Scatterplot of Weight of Car vs City MPG",
       x = "Weight of Car (in pounds)",
       y = "City Miles per Gallon")


abline(coefficients(model2)) ########The line does not appear on the grapgh########

# Now this new line looks more convincing

# histogram to this if there is a skew, we can also use this model to make a forcast using the predict function

# make a prediction

cpiForecast <- data.frame(2022:2025)

cpiForecast$logindex <- predict(lm(model2), newdata = cpiForecast)

cpiForecast <- mutate(cpiForecast, 
                      Index = exp(logindex))


########### The above code does not work properly###############


