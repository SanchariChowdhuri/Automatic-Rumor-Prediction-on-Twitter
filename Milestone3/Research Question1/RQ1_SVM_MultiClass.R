# Multiclass SVM
# Read the input data
rumors <- read.csv(file.choose())

# Replace NAs with the median value of the column
rumors$retweet_count[is.na(rumors$retweet_count)] <- median(rumors$retweet_count, na.rm = TRUE)
rumors$favorite_count[is.na(rumors$favorite_count)] <- median(rumors$favorite_count, na.rm = TRUE)
rumors$user_followers_count[is.na(rumors$user_followers_count)] <- median(rumors$user_followers_count, na.rm = TRUE)
rumors$user_friends_count[is.na(rumors$user_friends_count)] <- median(rumors$user_friends_count, na.rm = TRUE)

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


# RBF Kernel
rumor_classifier_rbf <- ksvm(Type~., data = rumors_train, kernel = "rbfdot")
rumor_predictions_rbf <- predict(rumor_classifier_rbf, rumors_test)
table(rumor_predictions_rbf, rumors_test$Type)
agreement <- rumor_predictions_rbf == rumors_test$Type
table(agreement)
prop.table(table(agreement))
