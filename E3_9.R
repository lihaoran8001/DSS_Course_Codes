Auto <- read.csv("./data/Auto.csv")
Auto<- Auto[,-1]
View(Auto)
Auto = na.omit(Auto)
#9.a
Auto[,9] = as.numeric(factor(Auto[,9]))
pairs(Auto)
#9.b
cor(subset(Auto, select=-name))
#9.c
lm.fit = lm(mpg~.-name, data=Auto)
summary(lm.fit)
#9.d
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit), rstudent(lm.fit))
abline(h = 3, col = "red")

#9.e
lm.fit2 = lm(mpg~acceleration:horsepower+weight*year, data=Auto)
summary(lm.fit2)

#9.f
lm.fit5 = lm(mpg~acceleration:horsepower+weight*year+log(horsepower)+I(acceleration^2), data=Auto)
summary(lm.fit5)

