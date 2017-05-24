#R script for RQ2
#Q1
rumors <- read.csv(file.choose())

# Replace NAs with the median value of the column
rumors$retweet_count[is.na(rumors$retweet_count)] <- median(rumors$retweet_count, na.rm = TRUE)
rumors$favorite_count[is.na(rumors$favorite_count)] <- median(rumors$favorite_count, na.rm = TRUE)
rumors$user_followers_count[is.na(rumors$user_followers_count)] <- median(rumors$user_followers_count, na.rm = TRUE)
rumors$user_friends_count[is.na(rumors$user_friends_count)] <- median(rumors$user_friends_count, na.rm = TRUE)

str(rumors)

# Assign the training and testing set in the ratio of 80:20
rumors_train <- rumors[1:4125,]
rumors_test <- rumors[4126:5156,]

# Load the kernlab library
library(kernlab)

# Train the SVM model
rumor_classifier <- ksvm(Gender~., data = rumors_train, kernel = "vanilladot")

# Look at basic information about the model
rumor_classifier

# Evaluate model performance
rumor_predictions <- predict(rumor_classifier, rumors_test)
head(rumor_predictions)

# Display the confusion matrix
table(rumor_predictions, rumors_test$Gender)

# Calculate overall accuracy
agreement <- rumor_predictions == rumors_test$Gender
table(agreement)
prop.table(table(agreement))

# Plot ROCR Curve
library(ROCR)

mylogit <- glm(rumors$Gender~rumors$retweet_count+rumors$favorite_count+rumors$user_followers_count+rumors$user_friends_count, family = "binomial")
summary(mylogit)

rumors$score <-predict.glm(mylogit, type="response" )
pred<-prediction(rumors$score,rumors$Gender)
perf<-performance(pred,"tpr", "fpr")
plot(perf)
abline(a=0,b=1)

# Compute sensitivity, specificity and cutoff
opt.cut = function(perf, pred) {	
  cut.ind = mapply(FUN = function(x,y,p) {
    d = (x-0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(perf,pred))

# Accuracy Plot
acc.perf = performance(pred, measure = "acc")
plot(acc.perf)

## Maximum Accuracy
ind = which.max(slot(acc.perf, 'y.values')[[1]])
acc = slot(acc.perf, 'y.values')[[1]][ind]
cutoff = slot(acc.perf, 'x.values')[[1]][ind]
print(c(accuracy = acc, cutoff = cutoff))

# Area Under The Curve
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

# Precision and Recall
perf1 <- performance(pred, "prec", "rec")
plot(perf1)

precision1 <- table(rumor_predictions, rumors_test$Gender)[1,1]/sum(table(rumor_predictions, rumors_test$Gender)[1,1:2])
precision1

recall1 <- table(rumor_predictions, rumors_test$Gender)[1,1]/sum(table(rumor_predictions, rumors_test$Gender)[1:2,1])
recall1

# F-1 Measure
f1_measure1 <- 2 * precision1 * recall1 / (precision1 + recall1)
f1_measure1

# RBF Kernel
rumor_classifier_rbf <- ksvm(Gender~., data = rumors_train, kernel = "rbfdot")
rumor_predictions_rbf <- predict(rumor_classifier_rbf, rumors_test)
table(rumor_predictions_rbf, rumors_test$Gender)
agreement <- rumor_predictions_rbf == rumors_test$Gender
table(agreement)
prop.table(table(agreement))

# Precision
precision2 <- table(rumor_predictions_rbf, rumors_test$Gender)[1,1]/sum(table(rumor_predictions_rbf, rumors_test$Gender)[1,1:2])
precision2

# Recall
recall1 <- table(rumor_predictions_rbf, rumors_test$Gender)[1,1]/sum(table(rumor_predictions_rbf, rumors_test$Gender)[1:2,1])
recall1

# F-1 Measure
f1_measure2 <- 2 * precision2 * recall2 / (precision2 + recall2)
f1_measure2

#Q2

#SVM Multiclass
# Multiclass SVM
# Read the input data
rumors <- read.csv(file.choose())

# Replace NAs with the median value of the column
rumors$retweet_count[is.na(rumors$retweet_count)] <- median(rumors$retweet_count, na.rm = TRUE)
rumors$favorite_count[is.na(rumors$favorite_count)] <- median(rumors$favorite_count, na.rm = TRUE)
rumors$user_followers_count[is.na(rumors$user_followers_count)] <- median(rumors$user_followers_count, na.rm = TRUE)
rumors$user_friends_count[is.na(rumors$user_friends_count)] <- median(rumors$user_friends_count, na.rm = TRUE)

# Assign the training and testing set in the ratio of 80:20
rumors_train <- rumors[1:4125,]
rumors_test <- rumors[4126:5156,]

# Load the kernlab library
library(kernlab)

# Train the SVM model
rumor_classifier <- ksvm(Gender~., data = rumors_train, kernel = "vanilladot")

# Look at basic information about the model
rumor_classifier

# Evaluate model performance
rumor_predictions <- predict(rumor_classifier, rumors_test)
head(rumor_predictions)

# Display the confusion matrix
table(rumor_predictions, rumors_test$Gender)

# Calculate overall accuracy
agreement <- rumor_predictions == rumors_test$Gender
table(agreement)
prop.table(table(agreement))

# Precision
precision1 <- table(rumor_predictions, rumors_test$Gender)[1,1]/sum(table(rumor_predictions, rumors_test$Gender)[1,1:2])
precision1

# Recall
recall1 <- table(rumor_predictions, rumors_test$Gender)[1,1]/sum(table(rumor_predictions, rumors_test$Gender)[1:2,1])
recall1

# F-1 Measure
f1_measure1 <- 2 * precision1 * recall1 / (precision1 + recall1)
f1_measure1


# RBF Kernel
rumor_classifier_rbf <- ksvm(Gender~., data = rumors_train, kernel = "rbfdot")
rumor_predictions_rbf <- predict(rumor_classifier_rbf, rumors_test)
table(rumor_predictions_rbf, rumors_test$Gender)
agreement <- rumor_predictions_rbf == rumors_test$Gender
table(agreement)
prop.table(table(agreement))

#Q3
#RQ2 : Neural network for predicting gender
#use dataset Rumor2.csv

library(neuralnet)
leeers1<-read.csv(file.choose(),header=TRUE)
length(leeers1)
leeers=na.omit(leeers1)
set.seed(1)
train <- sample(nrow(leeers1), nrow(leeers1)*0.5)
valid <- seq(nrow(leeers1))[-train]
datatrain <- leeers1[train,]
datavalid <- leeers1[valid,]
datatrain <- cbind(datatrain, datatrain$Gender == 'andy')
datatrain <- cbind(iristrain, datatrain$Gender == 'male')
datatrain <- cbind(iristrain, datatrain$Gender == 'female')
names(datatrain)[6:8] <- c('andy', 'male', 'female')

nn <- neuralnet(andy+male+female ~datatrain$user_followers_count +datatrain$user_friends_count,data=datatrain,hidden=c(1))

nn <- neuralnet(andy+male+female ~datatrain$user_followers_count +datatrain$user_friends_count,data=datatrain,hidden=c(1),stepmax = 1e6)

plot(nn)


#RQ2

#K mean clustering

df<-read.csv(file.choose(),header=TRUE)
head(df)
library(ggplot2)
ggplot(df, aes(df$user_followers_count, df$user_friends_count, color = df$Gender)) + geom_point()
set.seed(20)
Cluster <- kmeans(df[, 3:4], 2, nstart = 50)
Cluster
table(Cluster$cluster, df$Gender)
Cluster$cluster <- as.factor(Cluster$cluster)
ggplot(df, aes(df$user_followers_count, df$user_friends_count, color = df$Gender)) + geom_point()



# Determine number of clusters
wss <- (nrow(df[,2:5])-1)*sum(apply(df[,2:5],2,var))
for(i in 2:15) wss[i] <- sum(kmeans(rumors[,2:5], centers = i, iter.max = 50)$withinss)

# Bases on the plot we should try our analysis with 2 clusters
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

#----------------------------------------

# heirarchial clustering
clusters2 <- hclust(dist(df[, 0:1]))
plot(clusters2)

#heirarchy clustering with a small sample 
idx <- sample(1:dim(df)[1], 50)
Sample <- df[idx,]
Sample$Gender <- NULL
hc <- hclust(dist(Sample), method="ave")
plot(hc, hang = -1, labels=df$Gender[idx])


#-----------------------------------------------

#Density based clustering
# Read input file
rumors <- read.csv(file.choose())
# Replace NAs by the median value of their respective columns
rumors$retweet_count[is.na(rumors$retweet_count)] <- median(rumors$retweet_count, na.rm = TRUE)
rumors$favorite_count[is.na(rumors$favorite_count)] <- median(rumors$favorite_count, na.rm = TRUE)
rumors$user_followers_count[is.na(rumors$user_followers_count)] <- median(rumors$user_followers_count, na.rm = TRUE)
rumors$user_friends_count[is.na(rumors$user_friends_count)] <- median(rumors$user_friends_count, na.rm = TRUE)
# Install necessary packages for dbscan
install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)


# Convert data into a matrix form
d <- as.matrix(rumors[,1:4])
#dbscan(d, eps = 0.2, MinPts = 20)

# Run dbscan with epsilon value of 0.2 and minimum points as 20
set.seed(123)
db <- fpc::dbscan(d, eps = 0.2, MinPts = 20)
db

# Plot the result
plot(db, d, main = "DBSCAN", frame = FALSE)


#compartive analysis
library(caret)
df=read.csv(file.choose(),header=TRUE)


# Method1: K-Fold cross validation
# control method function
train_control <- trainControl(method="cv", number=10)

#model SVM
# train the SVM model
set.seed(7)
modelSvm <- train(Gender~., data=df, method="svmRadial",trControl=train_control)


# Model: Neural Networks
set.seed(7)
modelNN <- train(Gender~., data=df, method="nnet",trControl=train_control)


# Model: Random Forests
set.seed(7)
modelRF <- train(Gender~., data=df, method="rf",trControl=train_control)

results	<-	resamples(list(NN=modelNN,	RF=modelRF,	SVM=modelSvm))	

summary(results)

bwplot(results)
dotplot(results)

#------------
#Method2
#Method: Repeated K-Fold cross validation (3 repeats)
# control method function
train_control1<- trainControl(method = "repeatedcv", number=10, repeats = 3)

#model SVM
# train the SVM model
set.seed(7)
modelSvm <- train(Gender~., data=df, method="svmRadial",trControl=train_control1)


# Model: Neural Networks
set.seed(7)
modelNN <- train(Gender~., data=df, method="nnet",trControl=train_control1)


# Model: Random Forests
set.seed(7)
modelRF <- train(Gender~., data=df, method="rf",trControl=train_control1)

results	<-	resamples(list(NN=modelNN,	RF=modelRF,	SVM=modelSvm))	


summary(results)

bwplot(results)
dotplot(results)


#--------------------
# Method: Repeated Bootstrap
# control method function
train_control2<- trainControl(method = "boot", number=10)

#model SVM
# train the SVM model
set.seed(7)
modelSvm <- train(Gender~., data=df, method="svmRadial",trControl=train_control2)


# Model: Neural Networks
set.seed(7)
modelNN <- train(Gender~., data=df, method="nnet",trControl=train_control2)


# Model: Random Forests
set.seed(7)
modelRF <- train(Gender~., data=df, method="rf",trControl=train_control2)

results	<-	resamples(list(NN=modelNN,	RF=modelRF,	SVM=modelSvm))	


summary(results)

bwplot(results)
dotplot(results)




