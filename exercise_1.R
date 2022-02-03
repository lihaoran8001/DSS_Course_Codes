
Auto = read.csv("./data/Auto.csv", header=T, na.strings="?")
Auto = na.omit(Auto)
summary(Auto)
attach(Auto)
lm.fit = lm(mpg ~ horsepower)
summary(lm.fit)

