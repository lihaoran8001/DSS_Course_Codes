set.seed(1)
p = 20
n = 1000
x = matrix(rnorm(n * p), n, p)
B = rnorm(p)
B[4] = 0
B[5] = 0
B[10] = 0
B[11] = 0
B[17] = 0
B[19] = 0
eps = rnorm(p)
y = x %*% B + eps

train = sample(seq(1000), 100, replace = FALSE)
y.train = y[train, ]
y.test = y[-train, ]
x.train = x[train, ]
x.test = x[-train, ]

library(leaps)
regfit.full = regsubsets(y ~ ., data = data.frame(x = x.train, y = y.train), nvmax = p)
val.errors = rep(NA, p)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  pred = as.matrix(x.train[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% x_cols]
  val.errors[i] = mean((y.train - pred)^2)
}
plot(val.errors, xlab= "Predictors", ylab = "Training MSE", pch = 19, type = "b")

val.errors = rep(NA, p)
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  pred = as.matrix(x.test[, x_cols %in% names(coefi)]) %*% coefi[names(coefi) %in% 
                                                                   x_cols]
  val.errors[i] = mean((y.test - pred)^2)
}
plot(val.errors, xlab= "Predictors", ylab = "Test MSE", pch = 19, type = "b")

which.min(val.errors)

coef(regfit.full, id = 12)

val.errors = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)
for (i in 1:p) {
  coefi = coef(regfit.full, id = i)
  a[i] = length(coefi) - 1
  b[i] = sqrt(sum((B[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + 
                sum(B[!(x_cols %in% names(coefi))])^2)
}
plot(x = a, y = b, xlab = "Predictors", ylab = "Error between estimated and true coefficients")
which.min(b)
