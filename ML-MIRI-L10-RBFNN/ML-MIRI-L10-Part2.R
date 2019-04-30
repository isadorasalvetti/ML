####################################################################
# Machine Learning - MIRI Master
# Lluís A. Belanche

# LAB 10: Radial Basis Function Network (Part 2)
# version of April 2019
####################################################################


####################################################################
## Exercise

## We continue with Example 1: regression of a 1D function

## We are interested in studying the influence of sample size on the fit.
## The idea is that you embed the code in Part 1 into a couple of handy functions and leave
## the learning sample size (N) as a parameter.

## These are the learning sample sizes you are going to study

Ns <- c(25,50,100,200,500)

## You are asked to report the chosen lambda and the final test error (on the same test set),
## plot the learned function against the data and the true genearting function and see if the fit
## is better/worse/equal as a function of N and in what sense it is better/worse/equal

# Your code starts here ...

library(MASS) # we need it for lm.ridge

############
# Data set #
############

set.seed (4)

## Let us depart from the following function in the (a,b) interval
myf <- function (x) { (1 + x - 2*x^2) * exp(-x^2) }

## We are going to model this function in the interval (-5,5)
a <- -5
b <- 5
sigma.2 <- 0.04
domain <- c(a,b)

myf.data <- function (N, a, b) 
{
  x <- runif(N, a, b)
  t <- myf(x) + rnorm(N, sd=sqrt(sigma.2))
  dd <- data.frame(x,t)
  names(dd) <- c("x", "t")
  dd
}

## Create a large test data too for testing
N.test <- 2000
d.test <- myf.data (N.test, a , b)

###############################
# Function to compute a PHI (N x M) design matrix, without the Phi_0(x) = 1 column;
# c.i, sp.i are the centers and smoothing parameters or variances (sigma_i^2) of the neurons, respectively
PHI <- function (x,c,sp)
{
  N <- length(x)
  M <- length(c)
  phis <- matrix(rep(0,M*N), nrow=M)
  for (i in 1:M)
    phis[i,] <- exp(-(x - c[i])^2/(2*sp[i]))
  
  t(phis)
}

# Number of Centers
NumKmeans <- 20

## We set a rather large number of hidden units (= basis functions) M as a function of data size (the sqrt is just a heuristic!) because we are going to try different regularizers
(M <- floor(sqrt(N)))
m <- matrix(0,nrow=NumKmeans,ncol=M)
h <- matrix(0,nrow=NumKmeans,ncol=M)
data.Kmeans <- cbind(d$x,rep(0,N))

#Find best Lambda and errors
(lambda.list <- 10^seq(-3,1.5,by=0.1))
errors <- rep(0,NumKmeans)
bestLambdas <- rep(0,NumKmeans)

#######
# Run #
#######

N <- 500
d <- myf.data (N, a , b)

summary(d)

## The black points are the data, the blue line is the true underlying function
plot (d)
curve (myf, a, b, col='blue', add=TRUE)

###################
# Model

## We set number of hidden units
(M <- floor(sqrt(N)))
m <- matrix(0,nrow=NumKmeans,ncol=M)
h <- matrix(0,nrow=NumKmeans,ncol=M)
data.Kmeans <- cbind(d$x,rep(0,N))

for (j in 1:NumKmeans)
{
  # Find the centers c.i with k-means
  km.res <- cclust (x=data.Kmeans, centers=M, iter.max=200, method="kmeans", dist="euclidean")
  m[j,] <- km.res$centers[,1]
  
  # Obtain the variances sp.i as a function of the c.i
  h[j,] <- rep(0,M)
  for (i in 1:M)
  {
    indexes <- which(km.res$cluster == i)
    h[j,i] <- sum(abs(d$x[indexes] - m[j,i]))/length(indexes)
    if (h[j,i] == 0) h[j,i] <- 1
  }

  }
# For each k-means' result
for (num in 1:NumKmeans)
{
  c.i <- m[num,]
  sp.i <- h[num,]
  
  myPHI <- PHI (d$x,c.i,sp.i)
  aux1 <- lm.ridge(d$t ~ myPHI, d, lambda = lambda.list)
  my.lambda <- as.numeric(names(which.min(aux1$GCV)))
  
  aux2 <- lm.ridge(d$t ~ myPHI, d, lambda = my.lambda)
  
  errors[num] <- sqrt(aux2$GCV)
  bestLambdas[num] <- my.lambda
}

## Now we obtain the best model among the tested ones
(bestIndex <- which(errors == min(errors)))
(bestLambda <- bestLambdas[bestIndex])
c.i <- m[bestIndex,]
sp.i <- h[bestIndex,]

## We now create the final model:
my.RBF <- lm.ridge (d$t ~ PHI (d$x,c.i,sp.i), d, lambda = bestLambda)
(w.i <- setNames(coef(my.RBF), paste0("w_", 0:M)))

## It remains to calculate the prediction on the test data:
test.PHI <- cbind(rep(1,length(d.test$x)),PHI(d.test$x,c.i,sp.i))
y <- test.PHI %*% w.i

## And now the normalized error of this prediction:
(errorsTest <- sqrt(sum((d.test$t - y)^2)/((N.test-1)*var(d.test$t))))

#Plot
par(mfrow=c(1,1))
plot(d.test$x,d.test$t,xlab="x",ylab="t",main=paste("Prediction (learning size: ",toString(N),"examples)"),ylim=c(-1.5,1.5))
points(d.test$x,y,col='red',lwd=1)
curve (myf, a, b, col='blue', add=TRUE)

## The classical (predictive) R^2 coefficient is:
1-errorsTest^2


