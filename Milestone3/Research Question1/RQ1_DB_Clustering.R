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

# dbscan(rumors[,2:5], eps, MinPts = 5, scale = FALSE, method = c("hybrid", "raw", "dist"))

# Convert data into a matrix form
d <- as.matrix(rumors[,2:5])
#dbscan(d, eps = 0.2, MinPts = 20)

# Run dbscan with epsilon value of 0.2 and minimum points as 20
set.seed(123)
db <- fpc::dbscan(d, eps = 0.2, MinPts = 20)
db

# Plot the result
plot(db, d, main = "DBSCAN", frame = FALSE)
