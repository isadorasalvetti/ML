####################################################################
# Machine Learning - MIRI Master
# Lluís A. Belanche

# LAB 11: Kernel methods - the SVM for classification (Part 2)
# version of May 2019
####################################################################


####################################################################
## Exercise: Play with the SVM for classification of 2D data (3 classes)
####################################################################

#Add seed
set.seed(123)

N <- 300

circle.data <- function(N) 
{
  angles = runif(3*N)*2*pi
  norms = c(runif(N), runif(N) + 2, runif(N) + 4)
  x = sin(angles)*norms
  y = cos(angles)*norms
  c = factor(c(rep(1,N), rep(2,N), rep(3,N)))
  data.frame(x1=x, x2=y, target=c)
}

## let's generate the data
dataset <- circle.data (N)

## let's plot the data and the 3 classes
plot(dataset$x1,dataset$x2, col=as.factor(dataset$target))

## For multiclass-classification with k levels, k>2, this svm uses the ‘one-against-one’ approach, 
## in which k(k-1)/2 binary classifiers are trained; the appropriate class is found by a voting scheme


## Do the following (in separate sections)

# 1. Decide beforehand which kernel function could work better
#    Hint: have a look at the way the data is generated and specially the plot
# 2. Split the data into learning (2/3) and test (1/3)
# 3. Fit several svms with different kernels and evaluate their cross-validation error
# 4. You can use either svm() in {e1071} or ksvm() in {kernlab} (recommended)
#    Note that the cross-validation error is cross(), have a look at the help for ksvm()!
# 5. Choose your best model by playing a little bit with the C parameter
# 6. Refit it using the whole learning part
# 7. Use this model to predict the test part anf give a final prediction error

##################################
#Split data:
smp_size <- floor(0.66 * nrow(dataset))
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

plot(data.train$x1,data.train$x2, col=as.factor(data.train$target))

data.train <- dataset[train_ind, ]
data.test <- dataset[-train_ind, ]

library(e1071)
library(kernlab)
source("plot-prediction.R")

# Quadradic Kernel.
C <- 1
modelQ <- svm(data.train[,1:2],data.train$target, type="C-classification", cost=C, kernel="polynomial", degree=2, coef0=1, scale = FALSE)
plot.prediction (model, paste ("quadratic kernel (C=", C, ") ", model$tot.nSV, " Support Vectors", sep=""))
pred <- predict(modelQ,data.test[,1:2])
error <- sum(pred != data.test$target)/length(data.test$target)

#Linear - worst
modelL <- svm(data.train[,1:2],data.train$target, type="C-classification", cost=C, kernel="linear", coef0=1, scale = FALSE)
plot.prediction (model, paste ("linear kernel (C=", C, ") ", model$tot.nSV, " Support Vectors", sep=""))
pred <- predict(modelL,data.test[,1:2])
error <- sum(pred != data.test$target)/length(data.test$target)

#Radial
modelR <- svm(data.train[,1:2],data.train$target, type="C-classification", cost=C, kernel="radial", coef0=1, scale = FALSE)
plot.prediction (model, paste ("radial-basis kernel (C=", C, ") ", model$tot.nSV, " Support Vectors", sep=""))
pred <- predict(modelR,data.test[,1:2])
error <- sum(pred != data.test$target)/length(data.test$target)
