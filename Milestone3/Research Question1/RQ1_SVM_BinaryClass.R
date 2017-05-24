# Binary Class
# Vanilladot Kernel
# Read the input data
rumors <- read.csv(file.choose())

# Replace NAs with the median value of the column
rumors$retweet_count[is.na(rumors$retweet_count)] <- median(rumors$retweet_count, na.rm = TRUE)
rumors$favorite_count[is.na(rumors$favorite_count)] <- median(rumors$favorite_count, na.rm = TRUE)
rumors$user_followers_count[is.na(rumors$user_followers_count)] <- median(rumors$user_followers_count, na.rm = TRUE)
rumors$user_friends_count[is.na(rumors$user_friends_count)] <- median(rumors$user_friends_count, na.rm = TRUE)

str(rumors)

# Assign the training and testing set in the ratio of 80:20
rumors_train <- rumors[1:8700,]
rumors_test <- rumors[8701:10874,]

# Load the kernlab library
library(kernlab)

# Train the SVM model
rumor_classifier <- ksvm(Type~., data = rumors_train, kernel = "vanilladot")

# Look at basic information about the model
rumor_classifier

# Evaluate model performance
rumor_predictions <- predict(rumor_classifier, rumors_test)
head(rumor_predictions)

# Display the confusion matrix
table(rumor_predictions, rumors_test$Type)

# Calculate overall accuracy
agreement <- rumor_predictions == rumors_test$Type
table(agreement)
prop.table(table(agreement))

# Plot ROCR Curve
library(ROCR)

mylogit <- glm(rumors$Type~rumors$retweet_count+rumors$favorite_count+rumors$user_followers_count+rumors$user_friends_count, family = "binomial")
summary(mylogit)

rumors$score <-predict.glm(mylogit, type="response" )
pred<-prediction(rumors$score,rumors$Type)
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

precision1 <- table(rumor_predictions, rumors_test$Type)[1,1]/sum(table(rumor_predictions, rumors_test$Type)[1,1:2])
precision1

recall1 <- table(rumor_predictions, rumors_test$Type)[1,1]/sum(table(rumor_predictions, rumors_test$Type)[1:2,1])
recall1

# F-1 Measure
f1_measure1 <- 2 * precision1 * recall1 / (precision1 + recall1)
f1_measure1

# RBF Kernel
rumor_classifier_rbf <- ksvm(Type~., data = rumors_train, kernel = "rbfdot")
rumor_predictions_rbf <- predict(rumor_classifier_rbf, rumors_test)
table(rumor_predictions_rbf, rumors_test$Type)
agreement <- rumor_predictions_rbf == rumors_test$Type
table(agreement)
prop.table(table(agreement))

# Precision
precision2 <- table(rumor_predictions_rbf, rumors_test$Type)[1,1]/sum(table(rumor_predictions_rbf, rumors_test$Type)[1,1:2])
precision2

# Recall
recall2 <- table(rumor_predictions_rbf, rumors_test$Type)[1,1]/sum(table(rumor_predictions_rbf, rumors_test$Type)[1:2,1])
recall2

# F-1 Measure
f1_measure2 <- 2 * precision2 * recall2 / (precision2 + recall2)
f1_measure2
