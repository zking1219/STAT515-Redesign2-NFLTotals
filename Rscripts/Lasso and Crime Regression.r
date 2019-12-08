                      Lasso Crime Rate Example

No Assignment Due 

Related Reading:  

Introduction to Statistical Learning
Section 6.2.2 The Lasso:   page 219
Section 6.6 Lab 2 Ridge Regression and Lasso: page 251
The script below roughly follows the general template in:
Introduction to Statistical Learning by James et al. 2013.    

Sections  1. Read the data, make training and test subsets

          2. Create training and test samples using
             randomly selected row subscripts

          3. Generating shrinkage tuning parameters to try

          4. Fitting lasso models with a vector of tuning
             parameters seeing coefficients go to zero

          5. Estimating the best tuning parameter, lambda,
             via 10-fold cross validation

          6. Making predictions for the test set
             and computing the mean squared error

          7. Looking at the remaining coefficients

Comments  8. Considerations in comparing
             lasso and random forest models

          9. Quantitative models, graphics and
             verbal reasoning 

1. Read the data, make training and test subsets.

## Run_____________________________

crimeReg <- read.csv(file="crimeReg.csv", header=TRUE, row.names=1)
colnames(crimeReg)

# glmnet wants a Matrix of explanatory variables

x <- as.matrix(crimeReg[,1:80])

# the dependent variable: violent crime rate 

y <- crimeReg[, 81]

head(x)
head(y)

## End_____________________________

2. Create training and test samples using
   randomly selected row subscripts.

## Run___________________

nr <- nrow(x)
nr
trainingSize <- ceiling(nr/2) # half case for training

set.seed(37)
train <- sample(1:nr,trainingSize)

x.train <- x[train,]
y.train <- y[train]
head(x.train)
head(y.train)

# other half for testing
x.test <- x[-train,] 
y.test <- y[-train]
head(x.test)

## End__________________


3. Fitting lasso models with default values
   of lambda penalties and see the regresson
   coefficients go to zero from right to left.  
  
## Run_____________________  

library(glmnet)

#lasso.mod <- glmnet(x[train,],y[train], alpha=1, lambda=lambda)
lasso.mod <- glmnet(x[train,],y[train], alpha=1)
plot(lasso.mod,las=1)

## End ____________________ 

Note that by default glmnet lasso standardizes
variables before running the model. Documentation
says the coefficients are always returned on the
original scale. 

In the plot above the x-axis shows the sum of the
coefficient absolute values.
On the right the sum is large and primarily
based on one large magnitude positive
coefficient (the blue line) and one large
magnitude negative coefficient (the black line). 
The trajectories of two coefficients dominate
the scale of the  plot.  A future example will
omit the two variables to reveal more about other
the other trajectories that remain with these
two have coefficients of zero 

As we read from right to left the penalty parameter
is increasing, the L1 norm gets smaller, and more
regression coefficients contributing to the L1
norm shrinking toward to zero.  The numbers along the top
if plot indicated the number of non-zero coefficients
remaining.  

A rough guess it that 60 coefficients are left
when the shrinkage penalty drives the extreme
negative black line coefficient to zero.

A rough guess is that 25 coefficients are left 
when the shrinkage penalty drives the extreme
positive blue line coefficient to zero.  
  
5. Estimating the best tuning parameter, lambda,
   via 10-fold cross validation.

## Run____________________

set.seed(37)
cv.out = cv.glmnet(x[train,], y[train] ,alpha=1)
plot(cv.out)

cbind(lambda=round(cv.out$lambda,1),
  cvm=signif(cv.out$cvm,7),
  cvsd=signif(cv.out$cvsd,5))

## End_____________

In the R console, Line 41 with
lambda = 11.7 has the smallest
crossvalidation mean squared error
of 123,389.

Adding this estimate's standard error
of 13244 yields 136,632.

Scrolling up we find the line with the 
largest MSE less than 136,632 has
MSE 135493 and lambda=68.6. 
This lambda has removed
many additional variables from the model. 

It seems to be a compromise convention
to use this model when seeking a model
fewer variables.  

We can use R to find the line
and lambda for this conventional model

## Run
cvm <- cv.out$cvm
sub1 <- which.min(cvm)
sub1
cvmTest <- cvm[sub1]+
           cv.out$cvsd[sub1]
sub2 <- which(cvm < cvmTest)[1]
sub2
cv.out$lambda[sub2]
cv.out$lambda.1se 
## End


## Run

lambda.min <- cv.out$lambda.min
lambda.1se <- cv.out$lambda.1se
lambda.min
lambda.1se

## End__________________________

Ten-fold cross validation above partitions the
training set into 10 subsets. It leaves one
subset out, fits a model using the composite
of the other 9 subsets, predicts the values
for the omitted subset, and obtains the errors.

This process is repeated 10 time with a different subset
left out of the model each time.  At the end,
every case has been left out of the model once
and used to obtain a prediction error.

The function computes the mean of the squared errors,
MSE. The red dots show MSE and the standard deviations
of the errors for each value of the 100 tuning parameters. 

The left dashed vertical lines draws attention to
lambda.min and lambda.1se as shown on a log scale
    
 
6. Making predictions for the test set 
   and computing the mean squared error

## Run_________________

lasso.predBest <- predict(lasso.mod, s=lambda.min,
  newx=x.test)

lasso.pred1se <- predict(lasso.mod, s=lambda.1se,
  newx=x.test)

mean((lasso.predBest-y.test)^2)

mean((lasso.pred1se-y.test)^2)

## End_________________

These are higher than the cross validation values.

7. Looking at the remaining coefficients  

## Run___________________

out <- glmnet(x, y, alpha=1)
lasso.coef=predict(out,type="coefficients",
  s=lambda.min)
# lasso.coef is in a sparse matrix format
# 
coefMat <- as.matrix(lasso.coef)
coefMat <- cbind(coefMat,coefMat>0)
colnames(coefMat) <- c("Coefficent","Keep")
round( coefMat[coefMat[,2]==1,] ,3)

## End____________________

                    Coefficent Keep
(Intercept)           1650.529    1
racepctblack             4.264    1
pctUrban                 0.646    1
AsianPerCap              0.001    1
OtherPerCap              0.001    1
MalePctDivorce          32.624    1
PctKidsBornNeverMar     62.396    1
PctPersDenseHous         8.753    1
PctHousLess3BR           0.532    1
HousVacant               0.006    1
PctVacantBoarded         8.809    1
RentQrange               0.222    1
MedRentPctHousInc        1.580    1
PctForeignBorn           1.233    1


## Run
 
lasso.coef=predict(out,type="coefficients",
  s=lambda.1se)
coefMat <- as.matrix(lasso.coef)
coefOther <- signif(coefMat[coefMat>0,],3)
coefOther

## End

        (Intercept) 
           1.31e+03
 
     MalePctDivorce 
           2.05e+01 

PctKidsBornNeverMar 
           6.33e+01 

   PctPersDenseHous 
           4.86e-01 

         HousVacant 
           1.65e-03 

   PctVacantBoarded 
           1.48e+00 

8. Considerations in comparing
   lasso and random forest models

The MSE for six variable lasso model was 164,884. 
The MSE for the choose six variable random
forest model was 131,875  The random variables were:

racePctWhite
FemalePctDiv
PctKidsBornNeverMar   

PctPersDenseHous
PctUsePubTrans
PctPopUnderPov 

However the comparison was not a fair for several
reasons

  a)  We based the lasso cross validiation on a single training set
      model only made use of only half of the data. We should
      consider more randomly chosen train sets.  We might
      have poorly representative random sample.        
        
   
  b)  In terms of explanatory variables
      this lasso example does NOT include
      transformed explanatory variables

      Transforming skewed explanatory variables
      could help lasso. As discussed before
      random forests fits remain the same
      for monotone transforms of explanatory
      variables.

Both models might benefit from including interaction
terms and other combinations of explanatory variables.
 
Neither model transformed the dependent variable,
violent crime rate. Both would benefit but not
necessarilly by the same amount.

9.  Quantitative models, graphics and verbal reasoning

Models provide lenses through which we can see
patterns that are not directly accessible to our vision.
Our vision can help in curtaining data, critiquing models
and gaining insight.  Neither the power of models nor
the power of our vision should be ignored.  

There other many other models to try, more graphics to produce
and graphic designs to create.  Research in human perception
cognition and in information visualization and provides
a foundation for creating better designs. Eventually models may
include cognitive complexity penalty functions to provide a
better bridges among worlds of quantitative models,
visualization, verbal reasoning and additional forms
of human thought and motivation.
           
  



