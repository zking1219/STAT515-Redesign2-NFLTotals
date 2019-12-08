# Ridge and Lasso Regression

# Adapted from
# Ch 6.6 Lab2 Ridge Regression and the Lasso
#Originally Editted by Dr. Carr and edutted again for this course

#  Uses the Hitters data address previously in
# Ch 6.5 all subsets regression
#
# Sections

# 0.  Setup
# 1.  Ridge Regression and tuning parameters
# 1.1 Cross-validation and test set MSE
# 1.2 Extreme tuning equivalent models
#       The high penalty mean model
#       The no penalty lm model
# 1.3 Tuning parameter selection via
#     10 fold cross-validation
#
# 2. Lasso Regression and variable selection
#
#==============================================================

# 0.  Setup

library(ISLR)
library(glmnet)

#quickDif <- function(a,b,r=0) View(
#  round(cbind(a,b,"a-b"=a-b),r))

quickDif <- function(a,b,r=0) round(cbind(a,b,"a-b"=a-b),r)

Hitters <- na.omit(Hitters)
x = model.matrix(Salary~.,Hitters)[,-1]
y = Hitters$Salary

# 1. Ridge Regression========================

# Specify 100 candidate tuning parameters

grid  = 10^seq(10,-2,length=100)

# In glmnet, alpha = 0 is specifies ridge regression

ridge.mod = glmnet(x, y, alpha = 0, lambda=grid)

# The data set x as 19 variables
# For each of the 100 tuning parameters
# glmnet() produces penalized linear models
# produces 20 regression coefficients
# counting the mean. We convert the
# storage format to a standard
# 20 x 100 matrix to view the coefficients.

View(as.matrix(coef(ridge.mod)))
dim(coef(ridge.mod))

# See equation 6.5
# As examples we can pick the 50th and 60th
# tuning parameters, get the model
# coefficients, compute their L2Norms
# and the shrinkage penalties.
#
# We can look at the results and
# their differences

# Compute

tunePar50 <- ridge.mod$lambda[50]
coef50 <- coef(ridge.mod)[,50]
L2NormCoef50 <- sqrt( sum(coef50[-1]^2) )
shrinkPenalty50 <- tunePar50*L2NormCoef50^2

tunePar60 <- ridge.mod$lambda[60]
coef60 <- coef(ridge.mod)[,60]
L2NormCoef60 <- sqrt(sum(coef60[-1]^2))
shrinkPenalty60 <- tunePar60*L2NormCoef60^2

quickDif(tunePar50,tunePar60)# Look at values and differences

# Tuning parameters are decreasing quickly


# shrinkage penalty decreases
quickDif(shrinkPenalty50,
         shrinkPenalty60)

# L2Norm increases
quickDif(L2NormCoef50,L2NormCoef60,2)

# Coefficients magnitudes tend to increase
quickDif(coef50,coef60,r=3)

# We can specify a different tuning
# parameter, lambda, with the s argument
# to obtain corresponding predicted
# regression coefficients.

predict(ridge.mod, s = 50,type="coefficients")[1:20,]

# 1.1 Cross-validation and test set MSE=========

# Compute train and test set integer subscripts
# This is an alternative to computing TRUE and
# FALSE subscripts illustrate previously

set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

# Fit the training data set
ridge.mod = glmnet(x[train,],y[train],
  alpha=0, lambda=grid, thresh=1e-12)

# Get predicted values for the test set
# Use the newx argument to specify the test set.
# For now set the tuning parameter to 4.
# Later we select this with cross-validation.

ridge.pred= predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)

# The MSE = 101037 is much better than
# the MSE = 193253 computed below using
# the training set mean as the fit.

# 1.2 Equivalents to extreme tuning parameter models

# High coefficient penalty MSE and mean only model
# ridge tune = 10^10
ridge.pred=predict(ridge.mod, s=1e10, newx=x[test,])
highCoefPenalty = mean((ridge.pred-y.test)^2)
highCoefPenalty

# training set mean predicted MSE
fitTrainMean = mean((mean(y[train])-y.test)^2)

quickDif(highCoefPenalty,fitTrainMean)

# Zero Coefficient Penalty MSE and ordinary regression


#linear Model
lm.mod = lm(Salary~., subset=train,data=Hitters)
lmMSE <- mean((predict(lm.mod,Hitters[test,])-y.test)^2)

quickDif(zeroCoefPenalty,lmMSE)

# No Coefficient penalty coefficients and ordinary
# regression

# ridge tune 0
ridge0Coef = predict(ridge.mod, s=0,
  exact=T,type="coefficients")[1:20,]

quickDif(lm.mod$coef,ridge0Coef)

# 1.3 Tuning parameter selection via
# ridge tune = 0
ridge.pred=predict(ridge.mod, s=0,
  newx=x[test,],exact=T)
zeroCoefPenalty = mean((ridge.pred-y.test)^2)
#     10 fold cross-validation

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)

plot(cv.out,main="Ridge Regression")
bestlam=cv.out$lambda.min
bestlam

# test MSE better than the s=4 MSE of 101037
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)

# The model coefficients.  None are zero.

out = glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# 2.  The Lasso

# 2.1 A training set model

# In glmnet, alpha = 1 specifies lasso regression
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

# In the plot the L1Norm on the x-axis is the sum of the absolute
# values of coefficients.
#
# The numbers at the top are the number of non-zero coefficients in the
# model.
#
# As the tuning parameter decreases
# the magnitude of coefficients tend to increase
# including some becoming non-zero.
# The L1Norm increases.

# 2.2 Tuning parameter model with
#     10 fold cross validation

# Find the best tuning parameters
# and the test MSE

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)

# Find the non-zero model coefficients

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]
