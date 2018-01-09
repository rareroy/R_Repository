require(rattle.data)
data(wine, package='rattle')
head(wine)

wine.stand <- scale(wine[-1])  # To standarize the variables

# K-Means
k.means.fit <- kmeans(wine.stand, 3) # k = 3
attributes(k.means.fit)

#centroids
k.means.fit$centers
#cluster
k.means.fit$cluster

#clustersize
k.means.fit$size

#plot
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(wine.stand, nc=6)

#2D representation of cluster solution
library(cluster)
clusplot(wine.stand, k.means.fit$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

#confusionMatrix
table(wine[,1],k.means.fit$cluster)

d <- dist(wine.stand, method = "euclidean") # Euclidean distance matrix.
H.fit <- hclust(d, method="ward")

plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red")

table(wine[,1],groups)