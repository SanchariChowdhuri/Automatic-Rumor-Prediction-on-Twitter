#multinomial regression:
# read data rumor2.1csv
#RQ2: predicting gender based on retweet count,userfollower count,friends count
data2=read.csv(file.choose(),stringsAsFactors=FALSE)
library(nnet)
mod <- multinom(data2$Gender ~ data2$retweet_count + data2$user_followers_count+data2$user_friends_count, data=data2)
summary(mod)
predict(mod)
predict(mod,data2,"probs")


#RQ3: predicting topic of tweet based on retweet count,userfollower count,friends count
mod1 <- multinom(data2$Type ~ data2$retweet_count + data2$user_followers_count+data2$user_friends_count, data=data2)
summary(mod1)
predict(mod1)
predict(mod1,data2,"probs")
