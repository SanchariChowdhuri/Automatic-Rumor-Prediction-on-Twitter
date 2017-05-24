#Decision tree

#*-*-*-*-* Decision tree and boosting
#predicting user's gender based on favorite counts,retweets,userfollowers,user friends
#use dataset Rumor2.csv
user=read.csv(file.choose(),stringsAsFactors=FALSE)
str(user)

#lookattheclassvariable
table(user$Type)
set.seed(12345)
user_rand=user[order(runif(10037)),]


#split the dataframes
user_train=user_rand[1:9033, ]
user_test=user_rand[9034:10037,]
#check the proportion of class variable
prop.table(table(user_train$Type))
prop.table(table(user_test$Type))

library(C50)
user_model=C5.0(user_train[-4],as.factor(user_train$Type))
#display facts about the model

user_model
#display detailed information about the tree
summary(user_model)
user_pred=predict(user_model,user_test)
library(gmodels)
CrossTable(user_test$Type,user_pred,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual','predicted'))

# boosting

user_boost10=C5.0(user_train[-4],as.factor(user_train$Type),trials=10)
user_boost10
summary(user_boost10)

user_boost_pred10=predict(user_boost10,user_test)
CrossTable(user_test$Type,user_boost_pred10,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual','predicted'))




