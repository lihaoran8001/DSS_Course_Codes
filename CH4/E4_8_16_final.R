library(MASS)
summary(Boston)
View(Boston)
attach(Boston)

# 1. preparation 
# 1.1 crime rate above median
high_crime = rep(0, length(crim))
high_crime[crim > median(crim)] = 1
Boston = data.frame(Boston,high_crime)
# 1.2 split dataset (70% training + 30% test)
gp <- runif(nrow(Boston)) # generate uniform random numbers
Boston.train <- Boston[gp < 0.7,]
Boston.test <- Boston[gp >=0.7,]
high_crime.test = Boston.test$high_crime
idx = 1:nrow(Boston)
train = idx[gp<0.7]
test = idx[gp>=0.7]


pairs(Boston)

# 2. logistic regression
glm.fit = glm(high_crime ~ nox+age+dis+medv+black, data = Boston.train, family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, Boston.test, type = "response")
glm.pred = rep(0,length(glm.probs))
glm.pred[glm.probs>0.5]=1
mean(glm.pred!=high_crime.test)

# 3. LDA
lda.fit = lda(high_crime ~ nox+age+dis+medv+black, data = Boston.train)
lda.pred = predict(lda.fit, Boston.test)
mean(lda.pred$class!=high_crime.test)

# 4. Naive Bayes
library(e1071)   # install.packages("e1071")
nb.fit = naiveBayes(high_crime ~ nox+age+dis+medv+black, data = Boston.train)
nb.pred = predict(nb.fit,Boston.test)
mean(nb.pred!=high_crime.test)

# 5. KNN
library(class)
train.X = cbind(nox,age,dis,medv,black)[train,]
test.X = cbind(nox,age,dis,medv,black)[test,]
train.high_crime = high_crime[train]
set.seed(1)
# k=1
knn.pred_1 = knn(train.X,test.X,train.high_crime,k=1)
mean(knn.pred_1!=high_crime.test)
# k=10
set.seed(1)
knn.pred_10 = knn(train.X,test.X,train.high_crime,k=10)
mean(knn.pred_10!=high_crime.test)

# 5.1 KNN with all features
train.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black,lstat, medv)[train,]
test.X = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black,lstat, medv)[test,]
train.high_crime = high_crime[train]

set.seed(1)
knn.pred_10 = knn(train.X,test.X,train.high_crime,k=10)
mean(knn.pred_10!=high_crime.test)

