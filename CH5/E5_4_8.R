# (a)
set.seed(1)
x = rnorm(100)
y = x-2*x^2 + rnorm(100)

# (b)
plot(x,y)
x1 = seq(-3,3,0.1)
y1 = x1-2*x1^2
lines(x1,y1,col="red")

# (c)
library(boot)
Data = data.frame(x,y)
set.seed(1)
  # i. ii. iii. iv.
cv.error=rep(0,4)
for(i in 1:4){
  glm.fit=glm(y~poly(x,i))
  cv.error[i]=cv.glm(Data,glm.fit)$delta[1]
}
cv.error

# (d)
set.seed(10)         #change it 
cv.error=rep(0,4)
for(i in 1:4){
  glm.fit=glm(y~poly(x,i))
  cv.error[i]=cv.glm(Data,glm.fit)$delta[1]
}
cv.error

# (f)
summary(glm.fit)
