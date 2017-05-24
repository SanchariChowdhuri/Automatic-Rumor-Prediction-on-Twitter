#Random forest
#use rumor2
#RQ2


leeers<-read.csv(file.choose(),header=TRUE)
leeers=na.omit(leeers)
str(leeers)
#leeers$Type=as.factor(leeers$Type)
leeers$Gender=as.factor(leeers$Gender)
#randomising
table(leeers$Gender)
set.seed(12345)
tweet_raw_rand=leeers[order(runif(5157)),]


leeers_train<-leeers[1:3868,]
leeers_test<-leeers[3869:5157,]

prop.table(table(leeers_train$Gender))
prop.table(table(leeers_test$Gender))
#prop.table(tabllibrary(randomForest)
model <- randomForest(as.factor(leeers$Gender) ~leeers$favorite_count+leeers$retweet_count+leeers$user_followers_count+leeers$user_friends_count, data = leeers_train)
model
pred <- predict(model,data=leeers_test)

#error coming in it
table(predict(model),leeers_train$Gender)

importance(model)

