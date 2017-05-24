#RQ1 caret

install.packages("caret")
library(caret)

#choose the file
df=read.csv(file.choose(),header=TRUE)
train_control <- trainControl(method="cv", number=10)
model <- train(Type~., data=df, trControl=train_control, method="nb")
predictions <- predict(model, df[,1:4])
# summarize results
confusionMatrix(predictions, df$Type)


library(mlbench)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the NB model
set.seed(7)
modelNB <- train(Type~., data=df, method="nb",
                 trControl=control)
predictionsNB <- predict(modelNB, df[,1:4])
# summarize results
confusionMatrix(predictionsNB, df$Type)




# train the RF model
set.seed(7)
modelRF <- train(Type~., df=df, method="rf",
                 trControl=control, verbose=FALSE)
predictionsRF <- predict(modelRF, df[,1:4])
# summarize results
confusionMatrix(predictionsRF, df$Gender)




# train the SVM model
set.seed(7)
modelSvm <- train(Type~., data=df, method="svmRadial",trControl=control)
predictionsSVM <- predict(modelSvm, df[,1:4])
# summarize results
confusionMatrix(predictionsSVM, df$Gender)




# collect resamples
results <- resamples(list(NB=modelNB, RF=modelRF, SVM=modelSvm))
# summarize the distribu=ons
summary(results)

bwplot(results)
# dot plots of results
dotplot(results)
