# use the package "glmment"
#install.packages("glmnet") 
library(glmnet)

# load in the data set
setwd("")
setwd("~/Dropbox/STAT 8030/data")

bodyfat <- read.csv("BodyFat.csv", header=TRUE)
names(bodyfat)
bodyfat <- bodyfat[,-2] # remove density column


# fit a linear regression with all predictors except density
lm.bodyfat1 <- lm(BODYFAT~.,data=bodyfat)
summary(lm.bodyfat1)
#has to be a matrix
ridge.m1 <- glmnet(x=as.matrix(bodyfat[,-1]), y=bodyfat$BODYFAT, alpha=0)
#alpha=0 ridge 
#alpha=1 lasso
plot(ridge.m1, xvar='lambda',label='TRUE', main='Ridge plot for bodyfat data',lwd=1.5,
     cex=2)

# use cross validation to choose lambda
ridge.m1.cv <- cv.glmnet(x=as.matrix(bodyfat[,-1]), y=bodyfat$BODYFAT, alpha=0) 
best.lambda <- ridge.m1.cv$lambda.min #saving as element to use in following code

# fit the ridge model with the optimal lambda. just find solution at one value, the optimal one
ridge.m1.opt <- glmnet(x=as.matrix(bodyfat[,-1]), y=bodyfat$BODYFAT, alpha=0, lambda=best.lambda)
# print coefficients
ridge.m1.opt$beta 

beta.ridge <-coef(ridge.m1.opt)
beta.ols <- coef(lm.bodyfat1)

# plot coefficients for both models
plot(beta.ridge[-1], beta.ols[-1])
abline(a=0, b=1)

### Lasso regression

# use alpha=1 to implement Lasso

# fit lasso for a grid of lambda values 
lasso.m1 <- glmnet(x=bodyfat[,-1], y=bodyfat$BODYFAT, alpha=1)
plot(lasso.m1, xvar='lambda',label='TRUE')

# use cross validation to choose lambda
lasso.m1.cv <- cv.glmnet(x=as.matrix(bodyfat[,-1]), y=bodyfat$BODYFAT, alpha=1) 
best.lambda.lasso <- lasso.m1.cv$lambda.min

# fit the best lasso model. you can tell it's a lasso because of the .'s for coeffs zeroed out
lasso.m1.opt <- glmnet(x=as.matrix(bodyfat[,-1]), y=bodyfat$BODYFAT, alpha=1, lambda=best.lambda.lasso)
lasso.m1.opt$beta


beta.lasso <-coef(lasso.m1.opt)

plot(beta.lasso, beta.ols)

### Exercise 1 lasso

# Next we'll consider all regressors except density + all two way interactions

# setup code: create the X matrix with the help of model.matrix
X<- model.matrix(lm(BODYFAT~.^2,data=bodyfat))
dim(X)

# fit lasso for a grid of lambda values
lasso.m2 <- glmnet(x=X, y=bodyfat$BODYFAT, alpha=1)
plot(lasso.m2, xvar='lambda',label='TRUE')

# use cross validation to choose lambda
lasso.m2.cv <- cv.glmnet(x=X, y=bodyfat$BODYFAT, alpha=1) 
best.lambda.lasso2 <- lasso.m2.cv$lambda.min

# fit the best lasso model
lasso.m2.opt <- glmnet(x=X, y=bodyfat$BODYFAT, alpha=1, lambda=best.lambda.lasso2)
lasso.m2.opt$beta

###Rate My Professor Exercise

library(alr4)
data("Rateprof")
Rateprof <- Rateprof[,-7]
# Consider all regressors except density + all two way interactions

# some setup code: create the X matrix with the help of model.matrix
X<- model.matrix(lm(quality~.^2,data=Rateprof))
dim(X)

# fit lasso for a grid of lambda values
lasso.m3 <- glmnet(x=X, y=Rateprof$quality, alpha=1)
plot(lasso.m3, xvar='lambda',label='TRUE')

# use cross validation to choose lambda
lasso.m3.cv <- cv.glmnet(x=X, y=Rateprof$quality, alpha=1) 
best.lambda.lasso3 <- lasso.m3.cv$lambda.min

# fit the best lasso model
lasso.m3.opt <- glmnet(x=X, y=Rateprof$quality, alpha=1, lambda=best.lambda.lasso3)
lasso.m3.opt$beta

#try ridge 

# fit ridge for a grid of lambda values
ridge.m3 <- glmnet(x=X, y=Rateprof$quality, alpha=0)
plot(ridge.m3, xvar='lambda',label='TRUE')

# use cross validation to choose lambda
ridge.m3.cv <- cv.glmnet(x=X, y=Rateprof$quality, alpha=0) 
best.lambda.lasso3 <- ridge.m3.cv$lambda.min

# fit the best ridge model
ridge.m3.opt <- glmnet(x=X, y=Rateprof$quality, alpha=0, lambda=best.lambda.lasso3)
ridge.m3.opt$beta
