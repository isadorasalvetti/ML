####################################################################
# Machine Learning - MIRI
# Lluís A. Belanche

# LAB 3: Bias/Variance (practise)
# version of February 2019
####################################################################

####################################################################
## Exercise: Bias/Variance analysis on simulated data


set.seed(5345)
par(mfrow=c(1, 1))
library(PolynomF)

# Consider this function:

# this is the (unknown) target function and the best solution to our problem
f <- function(x) sin(x-5)/(x-5)

# From which we can sample datasets:
N <- 150
x <- runif(N,0,15)              # generate the x according to a uniform distribution p(x)
t <- f(x) + rnorm(N, sd=0.1)      # generate the t according to a gaussian conditional distribution p(t|x)

plot(data.frame(x, t))

## alongside f
curve(f, type="l", col="blue", add=TRUE)

# Exercise:

# The exercise consists in estimating bias and variance (and hence bias^2+variance)
# for different models, and deduce which (polynomial) model is better for this problem.

# To this end, you must generate many (thousands?) datasets of size N, choose one
# point x in [0,15] (I suggest x=10) and estimate bias and variance for it.
# Notice that you do not need to store the datasets.

# The models are going to be polynomials of degrees of your choice (I suggest 1,2,3,4,5,8).

## COMPARE POLINOMIALS
#Dataset
x <- runif(N,0,15)                 
t <- f(x) + rnorm(N, sd=0.1)
errors <- matrix (nrow=6, ncol=3)
colnames(errors) <- c("Degree","TR.NRMSE","VA.NRMSE")

## Generate only polinomials.
degrees <- c(1,2,3,4,5,8)
for (i in 1:length(degrees))
{
  #PLOT DATA
  plot(data.frame(x, t), xlab=paste("Polynomial fit of degree ", degrees[i]), ylab="f(x)")
  curve(f, type="l", col="green", add=TRUE)
  
  #FIT MODEL
  polyfit <- lm(t ~ poly(x, degrees[i], raw=TRUE))
  
  #GET TRAINING ERROR
  errors[i, "Degree"] <- degrees[i]
  errors[i, "TR.NRMSE"] <- sqrt( sum(polyfit$residuals^2) / ((N-1)*var(t)) )
  
  #PRINT
  p <- polynom(coef(polyfit))
  curve(p, col="red", add=TRUE)
}


## Validate models ?????
for (deg in degrees){
  for (i in 1:5)
  {
    x.v <- runif(N,0,15)                 
    t.v <- f(x) + rnorm(N, sd=0.1)
    
    plot(data.frame(x, t), xlab=paste(paste("Polynomial fit of degree", deg, "data sample"), i), ylab="f(x)")
    curve(f, type="l", col="green", add=TRUE)
    polyfit <- lm(t ~ poly(x, deg, raw=TRUE))
    p <- polynom(coef(polyfit))
    curve(p, col="red", add=TRUE)
    
    # fill in degree, training error and validation error (both are NRMSEs)
    predictions <- predict(polyfit, newdata=val)
    errors[i, "VA.NRMSE"] <- sqrt( sum((t - predictions)^2) / ((N-1)*var(t)) )
    points(data.frame(x, predict(polyfit, newdata=val)), col="red") # these are the predictions
  }
}

errors

