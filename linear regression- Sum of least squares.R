library(tidyverse)

eth <- read.csv(file.choose(), header=TRUE)

str(eth)
head(eth)

summary(eth)

cor(eth)


#Create a linear regression model and print the summary stats
model1 <- lm(Adj.Close_Eth~Open_sp, data = eth )
summary(model1) 

# Sum of least squares to determine strength
SSE = sum(model1$residuals^2)
SSE

# Need to learn how to plot these models and predicr with them

# Create a linear regression model
model2 <- lm(Adj.Close_Eth ~ Close_sp, data = eth)
summary(model2)

# Sum of least squares to determine strength
SSE = sum(model2$residuals^2)
SSE

# Create a linear regression model
model3 <- lm(Adj.Close_Eth ~ Adj.Close_sp, data = eth)
summary(model3)

# Sum of least squares to determine strength
SSE = sum(model3$residuals^2)
SSE

# With the data you have in hand, select the model that you think most strongly predicts the adjusted closing values for Ethereum.


