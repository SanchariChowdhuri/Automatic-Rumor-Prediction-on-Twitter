#RQ3 Predicting type of tweet based on text  
#NB predicting type of tweet based on text
#Rumor _sorted.csv
tweet_raw=read.csv(file.choose(),header=TRUE)
str(tweet_raw)
tweet_raw$Type=factor(tweet_raw$Type)
str(tweet_raw$Type)
table(tweet_raw$Type)
library(tm)
tweet_corpus=Corpus(VectorSource(tweet_raw$text))

print(tweet_corpus)
inspect(tweet_corpus[1:3])

corpus_clean=tm_map(tweet_corpus,tolower)
corpus_clean=tm_map(corpus_clean,removeNumbers)
corpus_clean=tm_map(corpus_clean,removeWords,stopwords())
corpus_clean=tm_map(corpus_clean,removePunctuation)
corpus_clean=tm_map(corpus_clean,stripWhitespace)

inspect(tweet_corpus[1:3])
inspect(corpus_clean[1:3])

tweet_dtm=DocumentTermMatrix(corpus_clean)
tweet_dtm

#lookattheclassvariable
table(tweet_raw$Type)
set.seed(12345)
tweet_raw_rand=tweet_raw[order(runif(5135)),]


#divide your dataset into training and testing set 
tweet_raw_train=tweet_raw[1:3868,]
tweet_raw_test=tweet_raw[3869:5135,]
tweet_dtm_train=tweet_dtm[1:3868,]
tweet_dtm_test=tweet_dtm[3869:5134,]
tweet_corpus_train=corpus_clean[1:3868]
tweet_corpus_test=corpus_clean[3869:5135]

prop.table(table(tweet_raw_train$Type))
prop.table(table(tweet_raw_test$Type))

findFreqTerms(tweet_dtm_train,5)
tweet_dict=c(findFreqTerms(tweet_dtm_train,5))
tweet_train=DocumentTermMatrix(tweet_corpus_train,list(dictionary=tweet_dict))
tweet_test=DocumentTermMatrix(tweet_corpus_test,list(dictionary=tweet_dict))

convert_counts=function(x){
  x=ifelse(x>0,1,0)
  x=factor(x,levels=c(0,1),labels=c("No","Yes"))
}
tweet_train=apply(tweet_train,MARGIN=2,convert_counts)
tweet_test=apply(tweet_test,MARGIN=2,convert_counts)

library(e1071)
tweet_classifier=naiveBayes(tweet_train,tweet_raw_train$Type)
tweet_classifier

tweet_test_pred=predict(tweet_classifier,tweet_test)
library(gmodels)
#confusion matrix.
CrossTable(tweet_test_pred,tweet_raw_test$Type,prop.chisq=FALSE,prop.t=FALSE,prop.r=FALSE,dnn=c('predicted','actual'))

#Laplace estimator
tweet_classifier2=naiveBayes(tweet_train,tweet_raw_train$Type,laplace=1)
tweet_test_pred2=predict(tweet_classifier2,tweet_test)
CrossTable(tweet_test_pred2,tweet_raw_test$Type,prop.chisq=FALSE,prop.t=FALSE,prop.r=FALSE,dnn=c('predicted','actual'))



#*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
#Decision tree

#*-*-*-*-* Decision tree and boosting
#predicting user's gender based on favorite counts,retweets,userfollowers,user friends
#use dataset Rumor2.1.csv
user=read.csv(file.choose(),stringsAsFactors=FALSE)
str(user)

#lookattheclassvariable
table(user$Type)
set.seed(12345)
user_rand=user[order(runif(5157)),]


#split the dataframes
user_train=user_rand[1:3868, ]
user_test=user_rand[3869:5157,]
#check the proportion of class variable
prop.table(table(user_train$Type))
prop.table(table(user_test$Type))

library(C50)
user_model=C5.0(user_train[-5],as.factor(user_train$Type))
#display facts about the model

user_model
#display detailed information about the tree
summary(user_model)
user_pred=predict(user_model,user_test)
library(gmodels)
CrossTable(user_test$Type,user_pred,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual default','predicted default'))

# boosting

user_boost10=C5.0(user_train[-5],as.factor(user_train$Type),trials=10)
user_boost10
summary(user_boost10)

user_boost_pred10=predict(user_boost10,user_test)
CrossTable(user_test$Type,user_boost_pred10,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn=c('actual default','predicted default'))
