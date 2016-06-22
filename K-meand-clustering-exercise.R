#This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
df <- scale(wine[-1])
head(df)


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)

# Exercise 2: 
#   * How many clusters does this method suggest?
# Answer: This method suggests 3 clusters.

#   * Why does this method work? What's the intuition behind it?

#     Answer: This method works because it reassign data points to the 
#cluster whose centroid is closest. It then calculate 
#new centroid of each cluster. This method calculates 
#the within cluster variation as the sum of the Euclidean 
#distance between the data points and their respective cluster 
#centroid. 
#       The intuition behind it is that it tries to cluster data 
#based on their similarity and executes till within cluster 
#variation cannot be reduced any further.

#   * Look at the code for wssplot() and figure out how it works

#     Answer:The function returns the numeric dataset to be analyzed,
#the maximum number of clusters to consider, and a random number 
#seed. The above function does K means for different values of 
#K aka no. of clusters and for each one calculates the sum of 
#squares (or error). After the function runs - we can determine 
#what the ideal no. of cluster is based on withinss values.


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?

#   Answer:A 3-cluster solution may be a good fit to the data. 


# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

# fit.km <- kmeans( ... )
set.seed(1234)
fit.km <- kmeans(df, 3, nstart = 25)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
install.packages("flexclust")
library(flexclust)
randIndex(tab.km)
cluster = fit.km$cluster
tab.km <- table(fit.km$cluster, wine$Type)
tab.km

#     Answer: The adjusted randIndex provides a measure of the 
# agreement between two partitions, adjusted for chance. It ranges from
# -1 (no agreement) to 1 (perfect agreement). 
# Agreement between the wine variety type and the cluster 
# solution is 0.897.

# Exercise 6:

# * Visualize these clusters using  function clusplot() from the cluster library
#clusplot( ... )
library(cluster)
clusplot(df, fit.km$cluster, color = TRUE)

# * Would you consider this a good clustering?

#   Answer: It seems to be quite good clustering as the same type of 
#data points are within the same cluster and the data points 
#are not overlapping. However, different data types are very 
#closely spaced with respect to each other and the clusters 
#are not far apart and there is overlapping of very few data points.
