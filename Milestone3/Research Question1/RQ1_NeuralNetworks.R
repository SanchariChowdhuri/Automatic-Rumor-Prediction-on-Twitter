# One Hidden Layer
rumors <- read.csv(file.choose())
rumors$retweet_count[is.na(rumors$retweet_count)] <- median(rumors$retweet_count, na.rm = TRUE)
rumors$favorite_count[is.na(rumors$favorite_count)] <- median(rumors$favorite_count, na.rm = TRUE)
rumors$user_followers_count[is.na(rumors$user_followers_count)] <- median(rumors$user_followers_count, na.rm = TRUE)
rumors$user_friends_count[is.na(rumors$user_friends_count)] <- median(rumors$user_friends_count, na.rm = TRUE)
str(rumors)
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}
rumor_norm <- as.data.frame(lapply(rumors,normalize))
summary(rumor_norm$Type)
summary(rumors$Type)
rumor_train <- rumor_norm[1:8155,]
rumor_test <- rumor_norm[8156:10874,]
library(neuralnet)
rumor_model <- neuralnet(formula = rumors$Type~rumors$retweet_count+rumors$favorite_count+rumors$user_followers_count+rumors$user_friends_count, data = rumor_train)
plot(rumor_model)
model_results <- compute(rumor_model, rumor_test[1:4])
model_results$neurons
model_results$net.result
predicted_type <- model_results$net.result
cor(predicted_type, rumor_test$Type)

# Adding hidden nodes
rumor_model2 <- neuralnet(formula = rumors$Type~rumors$retweet_count+rumors$user_followers_count+rumors$user_friends_count, data = rumor_train, hidden = 2)
plot(rumor_model2)
