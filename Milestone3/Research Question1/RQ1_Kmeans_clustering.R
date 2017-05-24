# Read input file
rumors <- read.csv(file.choose())

# Replace NAs by the median value of their respective columns
rumors$retweet_count[is.na(rumors$retweet_count)] <- median(rumors$retweet_count, na.rm = TRUE)
rumors$favorite_count[is.na(rumors$favorite_count)] <- median(rumors$favorite_count, na.rm = TRUE)
rumors$user_followers_count[is.na(rumors$user_followers_count)] <- median(rumors$user_followers_count, na.rm = TRUE)
rumors$user_friends_count[is.na(rumors$user_friends_count)] <- median(rumors$user_friends_count, na.rm = TRUE)

# Plot the dataset
library(ggplot2)
ggplot(rumors, aes(rumors$retweet_count, rumors$favorite_count, rumors$user_followers_count, rumors$user_friends_count, color = rumors$Type))+geom_point()

# Determine number of clusters
wss <- (nrow(rumors[,2:5])-1)*sum(apply(rumors[,2:5],2,var))
for(i in 2:15) wss[i] <- sum(kmeans(rumors[,2:5], centers = i, iter.max = 50)$withinss)

# Bases on the plot we should try our analysis with 2 clusters
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

# Since initial cluster assignments are random, set seed to ensure reproducibility
set.seed(20)

# We ask the algorithm to group the data into 2 clusters
# We ask R to try 50 different random starting assignments and select the one with lowest within cluster violation
rumor_cluster <- kmeans(rumors[,2:5], 2, iter.max = 10, nstart = 50)
rumor_cluster
fitted(kmeans(rumors[,2:5], 2, iter.max = 10, nstart = 50), method = c("centers", "classes"))

# Compare the cluster with the Type (Rumor/Non-Rumor)
table(rumor_cluster$cluster, rumors$Type)

# Plot the data to see the clusters
rumor_cluster$cluster <- as.factor(rumor_cluster$cluster)
ggplot(rumors, aes(rumors$retweet_count, rumors$favorite_count, rumors$user_followers_count, rumors$user_friends_count, color = rumor_cluster$cluster)) + geom_point()
