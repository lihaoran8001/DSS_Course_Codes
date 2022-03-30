library(tree)
library(ISLR2)
set.seed(1)
train<-sample(1:nrow(Boston), nrow(Boston) / 2)
#test = Boston[-train, ]
#boston.test<-Boston[-train,"medv"]

X.train = Boston[train, -13]
X.test = Boston[-train, -13]
Y.train = Boston[train, 13]
Y.test = Boston[-train, 13]

p = dim(Boston)[2]-1
p.2 = ceiling(p/2)
p.r = ceiling(sqrt(p))

library(randomForest)
rf.p<-randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, mtry = p, ntree = 500)
rf.p.2<-randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, mtry = p.2, ntree = 500)
rf.p.r<-randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, mtry = p.r, ntree = 500)



plot(1:500, rf.p$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
     ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.p.2$test$mse, col = "red", type = "l")
lines(1:500, rf.p.r$test$mse, col = "blue", type = "l")
legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), col = c("green", "red", "blue"), 
       cex = 1, lty = 1)
