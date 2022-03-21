library(ISLR2)
library(boot)
attach(Wage)
cv.error = rep(0, 10)
for(d in 1:10){
  glm.fit = glm(wage ~ poly(age, d, raw = T), data = Wage)
  cv.error[d] = cv.glm(Wage, glm.fit, K = 10)$delta[2]
  # The first value of delta is the standard k-fold estimate 
  # and the second is bias corrected.
}
plot(1:10, cv.error, xlab="Dimension", ylab="CV error", 
     type="l", pch=20, lwd=2, ylim=c(1590, 1700))
min.point = min(cv.error)
# standard deviation
sd.points = sd(cv.error)
abline(h=min.point + 0.2 * sd.points, col="red", lty="dashed")
abline(h=min.point - 0.2 * sd.points, col="red", lty="dashed")
legend("topright", "0.2-standard deviation lines", lty="dashed", col="red")

fit.1 = lm(wage ~ poly(age, 1, raw = T), data = Wage)
fit.2 = lm(wage ~ poly(age, 2, raw = T), data = Wage)
fit.3 = lm(wage ~ poly(age, 3, raw = T), data = Wage)
fit.4 = lm(wage ~ poly(age, 4, raw = T), data = Wage)
fit.5 = lm(wage ~ poly(age, 5, raw = T), data = Wage)
fit.6 = lm(wage ~ poly(age, 6, raw = T), data = Wage)
fit.7 = lm(wage ~ poly(age, 7, raw = T), data = Wage)
fit.8 = lm(wage ~ poly(age, 8, raw = T), data = Wage)
fit.9 = lm(wage ~ poly(age, 9, raw = T), data = Wage)
fit.10 = lm(wage ~ poly(age, 10, raw = T), data = Wage)
# anova analysis
anova(fit.1, fit.2, fit.3, fit.4, fit.5,
      fit.6, fit.7, fit.8, fit.9, fit.10)


# plot the original data and fitted line
plot(wage~age, data=Wage, col="darkgrey")
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.fit = lm(wage~poly(age, 3), data=Wage)
lm.pred = predict(lm.fit, data.frame(age=age.grid))
lines(age.grid, lm.pred, col="blue", lwd=2)

cv.error = rep(0, 10)
for (c in 2:10) {
  Wage$age.cut = cut(Wage$age, c)
  glm.fit = glm(wage~age.cut, data=Wage)
  cv.error[c] = cv.glm(Wage, glm.fit, K=10)$delta[2]
}
plot(2:10, cv.error[-1], xlab="Number of cuts", ylab="CV error", 
     type="l", pch=20, lwd=2)

# fit use 8 cuts
lm.fit = glm(wage~cut(age, 8), data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
lm.pred = predict(lm.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, lm.pred, col="red", lwd=2)
