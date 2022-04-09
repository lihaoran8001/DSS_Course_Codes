library(ISLR)
library(gbm)
library(randomForest)
set.seed(1)
summary(Weekly)
train = sample(nrow(Weekly), 0.8 * nrow(Weekly))
test = -train

#Logistic Regression
glm.fit = glm(Direction ~ . - Year - Today, data = Weekly[train, ], family = "binomial")
glm.probs = predict(glm.fit, newdata = Weekly[test, ], type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Weekly$Direction[test])

log_err= mean(glm.pred != Weekly$Direction[test])

#Transform values "Up" and "Down" to "1" and "0"

Weekly$BinomialDirection = ifelse(Weekly$Direction == "Up", 1, 0)

#Boosting

boost.weekly = gbm(BinomialDirection ~ . - Year - Today - Direction, data = Weekly[train, 
], distribution = "bernoulli", n.trees = 3000)
yhat.boost = predict(boost.weekly, newdata = Weekly[test, ], n.trees = 3000)
yhat.pred = rep(0, length(yhat.boost))
yhat.pred[yhat.boost > 0.5] = 1
table(yhat.pred, Weekly$BinomialDirection[test])

boost_err= mean(yhat.pred != Weekly$BinomialDirection[test])

#Bagging

Weekly = Weekly[, !(names(Weekly) %in% c("BinomialDirection"))]
bag.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, mtry = 6)
yhat.bag = predict(bag.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])

bagging_err= mean(yhat.bag != Weekly$Direction[test])

#Random forests
rf.weekly = randomForest(Direction ~ . - Year - Today, data = Weekly, subset = train, mtry = 6)
yhat.bag = predict(rf.weekly, newdata = Weekly[test, ])
table(yhat.bag, Weekly$Direction[test])

random_err= mean(yhat.bag != Weekly$Direction[test])

results<-c(log_err, boost_err, bagging_err, random_err)

barplot(results,
        main = "Error comparison",
        xlab = "RMSE",
        ylab = "Models",
        names.arg = c("Logistic", "Boosting", "Bagging", "Forests"),
        col = "blue",
        horiz = TRUE)
text(0.03, 0.7, "0.427")
text(0.03, 1.9, "0.472")
text(0.03, 3.1, "0.431")
text(0.03, 4.3, "0.454")
