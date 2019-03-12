####################################################################
# Machine Learning - MIRI
# Lluís A. Belanche

# LAB 2: Clustering (k-means and E-M) practise
# version of February 2019
####################################################################

# Choose one of these exercises to practice

####################################################################
## Exercise 1:  Clustering artificial 2-D CIRCLE DATA

library(mlbench)
library(Rmixmod)

# We generate 2D data: each of the clusters is a 2-D Gaussian. The centers are equally spaced 
# on a circle around the origin with radius r
# The covariance matrices are of the form sigma^2 I (sd^2 parameter in  mlbench.2dnormals())

N <- 1000
K <- 6

# the clusters
data.1 <- mlbench.2dnormals (N,K)
plot(data.1)

# the raw data (what the clustering method will receive)

plot(x=data.1$x[,1], y=data.1$x[,2])


## Do the following (in separate sections)

# 1. Decide beforehand which clustering method will work best and with which settings.
#    Hint: have a look at the way the data is generated: ?mlbench.2dnormals
summary(data.1)

# 2. Apply k-means a number of times with fixed K=6 and observe the results
K <- 2
kmeans2 <- cclust (data.1$x,K,method="kmeans",dist="euclidean")
plot(data.1$x[,1],data.1$x[,2],col=(kmeans2$cluster+1))
CH2 <- clustIndex(kmeans2, data.1$x, index="calinski")

# 3. Apply k-means with a choice of K values of your own and monitor the CH index; which K looks better?
K <- 3
kmeans3.1 <- cclust (data.1$x,K,method="kmeans",dist="euclidean")
CH3.1 <- clustIndex(kmeans3.1, data.1$x, index="calinski")
K <- 4
kmeans3.2 <- cclust (data.1$x,K,method="kmeans",dist="euclidean")
CH3.2 <- clustIndex(kmeans3.2, data.1$x, index="calinski")
K <- 5
kmeans3.3 <- cclust (data.1$x,K,method="kmeans",dist="euclidean")
CH3.3 <- clustIndex(kmeans3.3, data.1$x, index="calinski")
K <- 6
kmeans3.4 <- cclust (data.1$x,K,method="kmeans",dist="euclidean")
CH3.4 <- clustIndex(kmeans3.4, data.1$x, index="calinski")

#????

source ("C-H.r")   # gives C.H() 
do.kmeans <- function (whatdata, whatK)
{
  r <- cclust (whatdata,centers=whatK,iter.max=100,method="kmeans",dist="euclidean")
  C.H (r, whatdata)
}

range <- 2:30
chs <- numeric(length(range))
for (myK in range)
{
  chs[myK] <- max (replicate (100, do.kmeans(data.1$x, myK)))
}

plot (c(1,range), chs, type='b', xlab="No. of clusters", ylab="C-H index", main="Data")


# 4. Apply E-M with K=6 and observe the results (means, coefficients and covariances)
fammodel <- mixmodGaussianModel (family="diagonal", equal.proportions=FALSE)
z <- mixmodCluster (data.frame(data.1$x),models = fammodel, nbCluster = 6)
summary(z)

#Plot Clusters
(found.clusters <- z@bestResult@partition)
plot(data.1$x[,1],data.1$x[,2],col=(found.clusters+1))

#Plot Centers
(means <- z@bestResult@parameters@mean)
points(means,col=seq(1:5)+1,cex=2,pch=19)

# 5. Check the results against tour expectations (#1.)
plot(data.1)





####################################################################
## Exercise 2:  Clustering real 2-D data

## This exercise involves the use of the 'Geyser' data set, which contains data from the ‘Old Faithful’ geyser 
## in Yellowstone National Park, Wyoming. 

## the MASS library seems to contain the best version of the data
library(MASS)

help(geyser, package="MASS")

summary(geyser)

plot(geyser)

## with ggplot2, maybe we get better plots:
library(ggplot2)

qplot(waiting, duration, data=geyser)

# 6. Decide beforehand which clustering method will work best and with which settings.
#    No hint this time, this is a real dataset

# 7. Apply k-means with different values of K and observe the results
g= data.matrix(geyser)

K <- 2
kmeans7.2 <- cclust (g,K,method="kmeans",dist="euclidean")
CH7.2 <- clustIndex(kmeans7.2, g, index="calinski")
K <- 3
kmeans7.3 <- cclust (g,K,method="kmeans",dist="euclidean")
CH7.3 <- clustIndex(kmeans7.3, g, index="calinski")
K <- 4
kmeans7.4 <- cclust (g,K,method="kmeans",dist="euclidean")
CH7.4 <- clustIndex(kmeans7.4, g, index="calinski")
K <- 5
kmeans7.5 <- cclust (g,K,method="kmeans",dist="euclidean")
CH7.5 <- clustIndex(kmeans7.5, g, index="calinski")
K <- 6
kmeans7.6 <- cclust (g,K,method="kmeans",dist="euclidean")
CH7.6 <- clustIndex(kmeans7.6, g, index="calinski")


# 8. Apply k-means 100 times, get averages of the CH index, and decide the best value of K. Does it work?
source ("C-H.r")   # gives C.H() 
do.kmeans <- function (whatdata, whatK)
{
  r <- cclust (whatdata,centers=whatK,iter.max=100,method="kmeans",dist="euclidean")
  C.H (r, whatdata)
}

range <- 2:30
chs <- numeric(length(range))
for (myK in range)
{
  chs[myK] <- max (replicate (100, do.kmeans(g, myK)))
}

plot (c(1,range), chs, type='b', xlab="No. of clusters", ylab="C-H index", main="Geyser Data")

# 9. Apply E-M with a family of your choice ("spherical", "diagonal", etc), with the best value fo K delivered by k-means
fammodel <- mixmodGaussianModel (family="general", equal.proportions=FALSE)
z <- mixmodCluster (data.frame(g),models = fammodel, nbCluster = 6)
summary(z)

# 10. Choose the model and number of clusters with the largest BIC
fammodel <- mixmodGaussianModel (family="general", equal.proportions=FALSE)
z <- mixmodCluster (data.frame(g),models = fammodel, nbCluster = 2:6)
summary(z)
fammodel <- mixmodGaussianModel (family="spherical", equal.proportions=FALSE) #-! (highest BIC)
z <- mixmodCluster (data.frame(g),models = fammodel, nbCluster = 2:8)
summary(z)
fammodel <- mixmodGaussianModel (family="diagonal", equal.proportions=FALSE)
z <- mixmodCluster (data.frame(g),models = fammodel, nbCluster = 2:6)
summary(z)


# 11. Apply E-M again with a family of your choice ("spherical", "diagonal", etc), this time letting BIC decide the best number of clusters
#     The easiest way to inspect the final results is with summary() of your mixmodCluster() call
fammodel <- mixmodGaussianModel (family="spherical", equal.proportions=FALSE) #-! (highest BIC)
z <- mixmodCluster (data.frame(g),models = fammodel, nbCluster = 2:8)
summary(z)

#but... this one?
fammodel <- mixmodGaussianModel (family="general", equal.proportions=FALSE)
z <- mixmodCluster (data.frame(g),models = fammodel, nbCluster = 2:6)
summary(z)

# 12. Once you're done, try and plot the results; just plot() the result of mixmodCluster()
#Plot Clusters
(found.clusters <- z@bestResult@partition)
plot(g[,1],g[,2],col=(found.clusters+1))

#Plot Centers
(means <- z@bestResult@parameters@mean)
points(means,col=seq(1:5)+1,cex=2,pch=19)


####################################################################
## Exercise 3:  Clustering real multi-dimensional data

## This exercise involves the use of the 'Auto' data set, which we introduced in a previous lab session

# 13. Get the Auto data, redo the preprocessing
# 14. Apply E-M again with a family of your choice ("spherical", "diagonal", etc), letting BIC decide the best
#     number of clusters
# 15. Inspect and report the results of your clustering
#     Warning: do not directly plot() the results, it takes a long time
# 16. Use the clusplot() function in {cluster}
#     Like this:
#        library(cluster)
#        clusplot(Auto, z@bestResult@partition, color=TRUE, shade=TRUE, labels=2, lines=0)
#     please do consult ?clusplot.default


# Your code starts here ...
