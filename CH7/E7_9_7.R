library(ISLR)
View(Wage)
set.seed(1)

summary(Wage$maritl)
summary(Wage$jobclass)
par(mfrow=c(1,2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

fit = lm(wage~maritl, data=Wage)
deviance(fit)
summary(fit)
fit = lm(wage~jobclass, data=Wage)
deviance(fit)
summary(fit)
fit = lm(wage~maritl+jobclass, data=Wage)
deviance(fit)
summary(fit)


