
#RQ3

#Kmean clustering
#Rumor 2
df<-read.csv(file.choose(),header=TRUE)
head(df)
library(ggplot2)
ggplot(df, aes(df$user_followers_count, df$user_friends_count, color = df$Type)) + geom_point()
set.seed(20)
Cluster <- kmeans(df[, 3:4], 3, nstart = 20)
Cluster
table(Cluster$cluster, df$Type)
Cluster$cluster <- as.factor(Cluster$cluster)
ggplot(df, aes(df$user_followers_count, df$user_friends_count, color = df$Gender)) + geom_point()

#Determine number of clusters
wss <- (nrow(df[,2:5])-1)*sum(apply(df[,2:5],2,var))
for(i in 2:15) wss[i] <- sum(kmeans(rumors[,2:5], centers = i, iter.max = 50)$withinss)

# Bases on the plot we should try our analysis with 2 clusters
plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")



#---------------------

# heirarchial clustering
clusters3 <- hclust(dist(df[, 0:3]))
plot(clusters3)
clusterCut <- cutree(clusters, 1)

#heirarchy clustering with a small sample 
idx <- sample(1:dim(df)[1], 100)
Sample <- df[idx,]
Sample$Type <- NULL
hc <- hclust(dist(Sample), method="ave")
plot(hc, hang = -1, labels=df$Type[idx])




#Density based heirarchy
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
d <- as.matrix(rumors[,1:4])
#dbscan(d, eps = 0.2, MinPts = 20)

# Run dbscan with epsilon value of 0.2 and minimum points as 20
set.seed(123)
db <- fpc::dbscan(d, eps = 0.2, MinPts = 20)
db

# Plot the result
plot(db, d, main = "DBSCAN", frame = FALSE)
