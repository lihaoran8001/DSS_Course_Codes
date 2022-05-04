library(ISLR)
# a. in order to predict whether a given car gets high or low gas mileage, we should set "mpg" as predictor.
#   we define high mileage when mpg is larger than it's median, we define low milage when mpg is smaller than it's median
# Also we've already added a new column to show the level of gas milage, we should delete the "mpg" column
gas.med = median(Auto$mpg)
new.var = ifelse(Auto$mpg > gas.med, 1, 0)
Auto$mpglevel = as.factor(new.var)
dat = Auto[,-1]
View(dat)

# b.
library(e1071)
set.seed(1)
tune.out = tune(svm, mpglevel ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.001,0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

#c
set.seed(2)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "polynomial", ranges = list(cost = c(0.1, 
                                                                                              1, 5, 10), degree = c(2, 3, 4)))
summary(tune.out)

set.seed(3)
tune.out = tune(svm, mpglevel ~ ., data = Auto, kernel = "radial", ranges = list(cost = c(0.1, 
                                                                                          1, 5, 10), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
