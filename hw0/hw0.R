library(dplyr)
library(tidyr)
library(tidyverse)
library(glmnet)

run_regularization_model <- function(x, y, train, alpha) {
  # check if this is ridge or lasso
  if(alpha == 0) {
    reg_type = "Ridge"
  } else if(alpha == 1) {
    reg_type = "Lasso"
  } else {
    cat(sprintf("unsupported value for alpha %d, exiting\n", alpha))
    return;
  }
  
  # set the grid for a cross validation grid search for best lambda
  grid <- 10^seq(10,-2, length =100)
  
  # regression without CV
  mod <- glmnet(x[train ,],y[train],alpha =alpha, lambda =grid ,
                thresh =1e-12)
  
  # cross validation to find best lambda
  cv.out <- cv.glmnet(x[train ,], y[train], alpha = alpha)
  # plot the cv output
  plot(cv.out)
  
  # lambda min is the best lambda
  bestlam <- cv.out$lambda.min
  cat(sprintf("the best lambda for %s regression is %f\n", reg_type, bestlam))
  
  # how to coefficients change with lambda
  plot(cv.out$glmnet.fit, xvar = c("lambda"), label = TRUE)
  
  # training mse 
  pred <- predict(mod, s=bestlam, newx=x[-train,])
  mse <- mean((pred -y.test)^2)
  cat(sprintf("training mse with %s regression is %f\n", reg_type, mse))
  
  reg_coeff <- predict (mod, type ="coefficients",s=bestlam )
  cat(sprintf("coefficients for %s regression are as follows\n", reg_type))
  reg_coeff
}


# get the data
hitters <- read_csv("https://gist.githubusercontent.com/keeganhines/59974f1ebef97bbaa44fb19143f90bad/raw/d9bcf657f97201394a59fffd801c44347eb7e28d/Hitters.csv")

# here is what it looks like
glimpse(hitters)

# remove all char fields
hitters <- hitters[, !sapply(hitters, is.character)]
#hitters <- hitters %>% select(-X1)

# glimpse again to see the character fields are gone
glimpse(hitters)

# remove any NAs
hitters <- hitters %>%
  drop_na()

# train. test split
set.seed(1)
train <- sample(nrow(hitters), 0.5*nrow(hitters))

# model matrix
x <- model.matrix (Salary ~ ., hitters)[,-1]
y <- hitters$Salary
y.test <- y[-train]

# ridge regression
run_regularization_model(x, y, train, 0)

# lasso
run_regularization_model(x, y, train, 1)
