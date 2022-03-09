library(ISLR)
attach(College)
set.seed(11)
#Randomly splitting data into trainig and test set in 7:3 ratio
subset<-sample(nrow(College),nrow(College)*0.7)
train<-College[subset,]
test<-College[-subset,]
y.train = train$Apps
y.test = test$Apps
x.train = train[,-2]
x.test = test[,-2]


# b.
ls.full<-lm(Apps~.,data=train)
summary(ls.full)
predicted.apps<-predict(ls.full,test)
lm.testerr<-mean((y.test-predicted.apps)^2)
lm.testerr

# c.
train.mat<-model.matrix(Apps~.,data=train)[, -1]
test.mat<-model.matrix(Apps~.,data=test)[, -1]
grid<-10^seq(4,-2,length=100)
  #fitting the ridge regression model
library(glmnet)
ridge.mod <- glmnet(train.mat, y.train,alpha = 0, thresh = 1e-12)  # 
  # lambda = 0, that is linear regression
ridge.pred <- predict(ridge.mod, s = 0, newx = test.mat, exact = T, x=train.mat,y=y.train)  # exact = T on page 285
mean((ridge.pred - y.test)^2)        

  # check if the result is equal to the result of lm() because s=0
lm.mod = lm(y.train~train.mat)
lm.pred = predict(lm.mod, x.test)
mean((ridge.pred - y.test)^2) 
  # doing CV on model
set.seed(1)
cv.out<-cv.glmnet(train.mat,y.train, alpha = 0)
bestlam<-cv.out$lambda.min
bestlam

ridge.pred<-predict(ridge.mod, s = bestlam, newx = test.mat)
ridge.testerr = mean((ridge.pred - y.test)^2)
ridge.testerr
# d. lasso model
lasso.mod = glmnet(train.mat, y.train, alpha = 1)
plot(lasso.mod)
  # doing CV on lasso model
set.seed(1)
cv.out<-cv.glmnet(train.mat, y.train, alpha = 1)
plot(cv.out)
bestlam<-cv.out$lambda.min
bestlam
lasso.pred<-predict(lasso.mod, s = bestlam, newx = test.mat)
lasso.testerr = mean((lasso.pred - y.test)^2)
lasso.testerr 
  # coefficient
lasso.mod = glmnet(College[,-2], College$Apps, alpha = 1)
predict(lasso.mod, s=bestlam, type="coefficients")


# e. PCR
library(pls)
set.seed(3)
# #pcr.fit<-pcr(y.train~train.mat,  scale = TRUE, validation = "CV")
pcr.fit<-pcr(Apps~., data = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)
pcr.pred<-predict(pcr.fit, x.test, ncomp = 16)
pcr.testerr=mean((pcr.pred - y.test)^2)
pcr.testerr


# f. PLS
pls.fit = plsr(Apps~., data=train, scale=TRUE, validation="CV")
validationplot(pls.fit, val.type="MSEP")
summary(pls.fit)
pls.pred = predict(pls.fit, x.test, ncomp=10)
pls.testerr = mean((pls.pred - y.test)^2)
pls.testerr
# g.
#Least Square model
test.avg <- mean(y.test)
lm.r2 <- 1 - mean((predicted.apps - test$Apps)^2) / mean((test.avg - test$Apps)^2)
#Ridge model
ridge.r2 <- 1 - mean((ridge.pred - test$Apps)^2) / mean((test.avg - test$Apps)^2)
#Lasso model
lasso.r2 <- 1 - mean((lasso.pred - test$Apps)^2) / mean((test.avg - test$Apps)^2)
#PCR model
pcr.r2 <- 1 - mean((pcr.pred- test$Apps)^2) / mean((test.avg - test$Apps)^2)
#PLS model
pls.r2 <- 1 - mean((pls.pred - test$Apps)^2) / mean((test.avg - test$Apps)^2)
