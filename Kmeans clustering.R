install.packages('factoextra')

library(dplyr)
library(cluster)
library(factoextra)


df_fb <- read.csv(file.choose(), header = TRUE)

# sense check data
head(df_fb)
summary(df_fb)

# drop first 3 columns
df_fb_nec <- select(df_fb, -c(1:3))

colnames(df_fb_nec)

# drop all rows with NA values
df_fb_nec <- na.omit(df_fb_nec)

# next we scale each variable to have a mean of 0 and a standard deviation of 1 
# as we can only do this scaling with a matrix we must covert the data frame to a matrix

df_fb_nec <- as.matrix(scale(df_fb_nec))
  
# Now we want to find the optimum number of clusters to do this we will use:
# fviz_nbclust() function. this creates a plot that compares the number of clusters vs the total
# within sum squares

fviz_nbclust(df_fb_nec, kmeans, method = 'wss') # wss stands for within sum of sqaures

# Next we make the example reproducable by seting the seed to 1. This means that the same centroids will be chosen each time

set.seed(1)

# Use Kmeans to create kmeans model
model <- kmeans(df_fb_nec,4,nstart = 25) # nstart = 25 is the initial number of configurations the model will create
print(model)

# Visualise the clusters
fviz_cluster(model, data = df_fb_nec)

# We can use the aggreggate function to find the mean of the variables in each cluster
aggregate(df_fb_nec, by=list(cluster=model$cluster), mean)
  