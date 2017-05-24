# Read input file
rumors <- read.csv(file.choose())

# Replace NAs by the median value of their respective columns
rumors$retweet_count[is.na(rumors$retweet_count)] <- median(rumors$retweet_count, na.rm = TRUE)
rumors$favorite_count[is.na(rumors$favorite_count)] <- median(rumors$favorite_count, na.rm = TRUE)
rumors$user_followers_count[is.na(rumors$user_followers_count)] <- median(rumors$user_followers_count, na.rm = TRUE)
rumors$user_friends_count[is.na(rumors$user_friends_count)] <- median(rumors$user_friends_count, na.rm = TRUE)

clusters <- hclust(dist(rumors[,2:5]), method = "centroid")
plot(clusters)
clustercut <- cutree(clusters, 2)
table(clustercut, rumors$Type)

ggplot(rumors, aes(rumors$retweet_count, rumors$favorite_count, rumors$user_followers_count, rumors$user_friends_count, color = rumors$Type)) + geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustercut) + scale_color_manual(values = c('red', 'green'))

# heirarchy clustering with a small sample
# Run it for 1000 rows and then repeat for 100
idx <- sample(1:dim(rumors)[1], 1000)
idx
Sample <- rumors[idx,]
Sample
Sample$Type <- NULL
hc <- hclust(dist(Sample), method="ave")
plot(hc, hang = -1, labels=rumors$Type[idx])
clustercut <- cutree(hc, 2)
table(clustercut, rumors[1:1000,]$Type)
