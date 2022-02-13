# Codes for exercise 3.8
# (a)
Auto = read.csv("./data/Auto.csv", header=T, na.strings="?")
attach(Auto)
fit = lm(mpg ~ horsepower)
summary(fit)

predict(fit, data.frame(horsepower=c(98)), interval="confidence")
predict(fit, data.frame(horsepower=c(98)), interval="prediction", level=0.95)

# (b)
plot(horsepower, mpg)  # draw the points
abline(fit)  # draw a line

# (c)
par(mfrow=c(2,2))  # divide the frame into four part to display
plot(fit)

detach(Auto)
