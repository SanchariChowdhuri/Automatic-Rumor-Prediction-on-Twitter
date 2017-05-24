library(aod)
library(ggplot2)

# Load the data
mydata <- read.csv(file.choose())

# Compute the logistic model
mylogit <- glm(Type ~ retweet_count + favorite_count + user_followers_count + user_friends_count, data=mydata, family = "binomial")

# Set max.print=1000000 so that R does not omit rows
options(max.print=1000000)

# Get the details of the model and export it into a text file
out <- capture.output(summary(mylogit))
cat("Model Summary", out, file="C:/Temp/Logistic_Regression_1.txt", sep="n", append=TRUE)

# Exponentiate coefficients to interpret as odd ratios
# Export the result into a text file
exp_out <- capture.output(exp(coef(mylogit)))
cat("Model Summary", exp_out, file = "C:/Temp/Logistic_Regression_2.txt", sep="n", append=TRUE)

# Predict the testing dataset by calculating predicted probability of rumors
# Create dataframe
newdata1 <- with(mydata, data.frame(retweet_count=retweet_count, 
                                    user_followers_count=user_followers_count, 
                                    user_friends_count=user_friends_count, 
                                    favorite_count=favorite_count))
# Compute probability of rumor for each value in the other columns
newdata1$RumorP <- predict(mylogit, newdata=newdata1,type="response")

# View the new dataframe
head(newdata1)
View(newdata1)

# Calcuate mean of the predicted probabilities
mean(newdata1$RumorP,na.rm = TRUE)
