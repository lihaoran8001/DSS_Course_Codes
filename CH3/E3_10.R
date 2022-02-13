Carseats <- read.csv("./data/Carseats.csv")
View(Carseats)
attach(Carseats)
# 10.a
lm.fit = lm(Sales~Price+Urban+US)
summary(lm.fit)

# 10.e
lm.fit2 = lm(Sales ~ Price + US)
summary(lm.fit2)

# 10.g
confint(lm.fit2)

# 10.h
#---outliers
plot(predict(lm.fit2), rstudent(lm.fit2))
#---high leverage
plot(hatvalues(lm.fit2))
abline(h = 0.0075, col = "red")

detach(Carseats)

