# Comparative Analysis
# SVM, NN, RF

### bringing library
# Please install these packages, if needed
# install.packages("mlbench")
# install.packages("caret")
# install.packages("nlme")
library(mlbench)
library(caret)
library(nlme)
library(kernlab)
library(randomForest)

## Read data rumor2.1csv
data3=read.csv(file.choose(),stringsAsFactors=FALSE)
View(data3)
data2 = data.frame(data3)
data2 = na.exclude(data2)

# Preparing training scheme
controlB <- trainControl(method="boot", number=10)
control0 <- trainControl(method="cv", number=10)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)



##### Training
set.seed(7)
# SVM
modelSVMB <- train(Type~.,data=data2,method="svmRadial",trControl=controlB)
modelSVM0 <- train(Type~.,data=data2,method="svmRadial",trControl=control0)
modelSVM <- train(Type~.,data=data2,method="svmRadial",trControl=control)
# NN - Neural Networks
modelNNB <- train (Type~.,data=data2,method="nnet",trControl=controlB)
modelNN0 <- train (Type~.,data=data2,method="nnet",trControl=control0)
modelNN <- train (Type~.,data=data2,method="nnet",trControl=control)
# Random Forest
modelRFB <- train (Type~.,data=data2,method="rf",trControl=controlB,verbose=FALSE)
modelRF0 <- train (Type~.,data=data2,method="rf",trControl=control0,verbose=FALSE)
modelRF <- train (Type~.,data=data2,method="rf",trControl=control,verbose=FALSE)

# Compare models
resultsB <- resamples((list(NN=modelNNB,RF=modelRFB,SVM=modelSVMB)))
results0 <- resamples((list(NN=modelNN0,RF=modelRF0,SVM=modelSVM0)))
results <- resamples((list(NN=modelNN,RF=modelRF,SVM=modelSVM)))

# One time only
# Show results
summary(results0)
# Boxplots of results
bwplot(results0)

# Repeated 3 times
# Show results
summary(results)
# Boxplots of results
bwplot(results)

# Boot
# Show results
summary(resultsB)
# Boxplots of results
bwplot(resultsB)

