# Rq3:Neural network for predicting topic 
#Rumor2.1copy.csv

krirs<-read.csv(file.choose(),header=TRUE)
library(neuralnet)

# Make training and validation data
set.seed(1)
train <- sample(nrow(krirs), nrow(krirs)*0.5)
valid <- seq(nrow(krirs))[-train]
iristrain <- krirs[train,]
irisvalid <- krirs[valid,]

# Binarize the categorical output
iristrain <- cbind(iristrain, iristrain$Type == 'City')
iristrain <- cbind(iristrain, iristrain$Type == 'electronics')
iristrain <- cbind(iristrain, iristrain$Type == 'Politics')
iristrain <- cbind(iristrain, iristrain$Type == 'transport')
names(iristrain)[6:9] <- c('City', 'electronics', 'Politics','transport')

# Fit model
nn <- neuralnet(
  City+electronics+Politics+transport ~ user_friends_count + user_followers_count + retweet_count,
  data=iristrain, 
  hidden=c(1)
)
plot(nn)

# Fit model 2
nn2 <- neuralnet(
  City+electronics+Politics+transport ~ user_friends_count + user_followers_count + retweet_count,
  data=iristrain, 
  hidden = 2, 
  stepmax = 1e6
)
plot(nn2)

