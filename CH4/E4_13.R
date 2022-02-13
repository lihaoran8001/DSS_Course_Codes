Weekly <- read.csv("./data/Weekly.csv")
Weekly = na.omit(Weekly)
Weekly[,9] = as.numeric(factor(Weekly[,9]))  # turn direction into numeric
# (a)
pairs(Weekly)
cor(Weekly)  # show the correlation among different vars
# (b)
attach(Weekly)
lm.fit1 = lm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume)
summary(lm.fit1)




detach(Weekly)

