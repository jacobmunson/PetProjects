##################################
### A little k-Means excursion ###
##################################
# A 2D k-Means Visualization 
# Dataframe D is spaced oddly for "readability" of points
# 

D <- matrix(data = c(0,1,
                     0,2,
                     0,3,
                     1,1,
                     1,2,
                     1,3,
                     2,1,
                     2,2,
                     2,3,
                     3,1,
                     3,2,
                     3,3,
                     3,4,
                     4,1,
                     4,2,
                     5,6,
                     5,7,
                     6,4,
                     6,5,
                     6,6,
                     6,7,
                     6,8,
                     7,4,
                     7,5,
                     7,6,
                     7,7,
                     7,8,
                     8,4,
                     8,5,
                     8,6,
                     9,3,
                     9,2,
                     10,1,
                     10,2,
                     10,3,
                     11,1,
                     11,2,
                     11,3,
                     11,4,
                     12,1,
                     12,2,
                     12,3,
                     12,4,
                     12,7,
                     13,1,
                     13,2,
                     13,3,
                     13,4),ncol = 2,byrow = TRUE)
plot(D)
# From the plot, it's "obvious" that there are 3 distinct clusters
km <- kmeans(x = D, centers = 3)
id <- km$cluster
D1 <- data.frame(D,id)
for(i in 1:nrow(D1)){
  if(D1[i,3] == 2){
    points(D1[i,1:2], col = "green", pch = "2")
  }else if(D1[i,3] == 1){
    points(D1[i,1:2], col = "blue", pch = "1")
  }else if(D1[i,3] == 3){
    points(D1[i,1:2], col = "purple", pch = "3")
  }else if(D1[i,3] == 4){
    points(D1[i,1:2], col = "orange", pch = "4")
  }
}
points(km$centers, col = "red",pch = 16)
# with 3 clusters, it looks like a nice fit (the odd point was intentionally included)
# For illustration purposes, what if we can't visualize ahead of time and select "too" many clusters?
# Let's go with 4 clusters ("close enough" to 3, right?)
plot(D)
km <- kmeans(x = D, centers = 4)
id <- km$cluster
D1 <- data.frame(D,id)
for(i in 1:nrow(D1)){
  if(D1[i,3] == 2){
    points(D1[i,1:2], col = "green", pch = "2")
  }else if(D1[i,3] == 1){
    points(D1[i,1:2], col = "blue", pch = "1")
  }else if(D1[i,3] == 3){
    points(D1[i,1:2], col = "purple", pch = "3")
  }else if(D1[i,3] == 4){
    points(D1[i,1:2], col = "orange", pch = "4")
  }
}
points(km$centers, col = "red",pch = 16)
# often one of the big chunks gets broken in two peices 
# I may continue on later with clustering validation metrics...