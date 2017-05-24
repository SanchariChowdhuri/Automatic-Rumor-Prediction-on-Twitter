# Read the input file
rumor_raw	<- read.csv(file.choose(), stringsAsFactors	=	FALSE)

# See how the data looks like
str(rumor_raw)

# Convert the values 'Rumor' and 'Non-Rumor' to factor
rumor_raw$type <- factor(rumor_raw$Type)

# This will show the values in the Type variable
str(rumor_raw$Type)
table(rumor_raw$Type)

# Library to clean the text
# Build a corpus using the text mining package
library(tm)
rumor_corpus <- Corpus(VectorSource(rumor_raw$text))

# Examine the corpus
print(rumor_corpus)
inspect(rumor_corpus[1:3])

# Clean the corpus using tm_map
corpus_clean <- tm_map(rumor_corpus, tolower)	
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# Inspect how the cleaned corpus looks like
inspect(rumor_corpus[1:3])
inspect(corpus_clean[1:3])

# Create the document term sparse matrix
rumor_dtm <- DocumentTermMatrix(corpus_clean)
rumor_dtm

# Create training and test datasets
rumor_raw_train <- rumor_raw[1:8156,]
rumor_raw_test <- rumor_raw[8157:10875,]
rumor_dtm_train <- rumor_dtm[1:8156,]
rumor_dtm_test <- rumor_dtm[8157:10875,]
rumor_corpus_train <- corpus_clean[1:8156]
rumor_corpus_test <- corpus_clean[8157:10875]

# Check if the proportion of rumors and non-rumors are the same in the training and testing datasets
prop.table(table(rumor_raw_train$Type))
prop.table(table(rumor_raw_test$Type))

# Retain only frequent words
# Frequent words are those that appear in at least 8 tweets
findFreqTerms(rumor_dtm_train, 8)
rumor_dict <- c(findFreqTerms(rumor_dtm_train, 8))
rumor_train <- DocumentTermMatrix(rumor_corpus_train, list(dictionary=rumor_dict))
rumor_test <- DocumentTermMatrix(rumor_corpus_test, list(dictionary=rumor_dict))

# Convert the count of times a word appears in a tweet to a "yes"/"no" appearance
convert_counts <- function(x) {
  x <- ifelse(x>0,1,0)
  x <- factor(x, levels = c(0,1), labels = c("No","Yes"))
}

# apply the convert_counts function to columns of train/test data
rumor_train <- apply(rumor_train, MARGIN = 2, convert_counts)
rumor_test <- apply(rumor_test, MARGIN = 2, convert_counts)

# Train a model on the data
library(e1071)
rumor_classifier <- naiveBayes(rumor_train, as.factor(rumor_raw_train$Type))
rumor_classifier

# Evaluate model performance
rumor_test_pred <- predict(rumor_classifier, rumor_test)
library(gmodels)
# Display confusion matrix
CrossTable(rumor_test_pred,	rumor_raw_test$Type,	
           prop.chisq	=	FALSE,	prop.t	=	FALSE,	prop.r	=	FALSE,	
           dnn	=	c('predicted',	'actual'))

# Add Laplace Estimator to improve model performance
rumor_classifier2 <- naiveBayes(rumor_train, as.factor(rumor_raw_train$Type), laplace = 1)
rumor_test_pred2 <- predict(rumor_classifier2, rumor_test)
# Display confusion matrix
CrossTable(rumor_test_pred2, rumor_raw_test$type, prop.chisq = FALSE, prop.t = FALSE,
           prop.r	=	FALSE, dnn = c('predicted',	'actual'))
