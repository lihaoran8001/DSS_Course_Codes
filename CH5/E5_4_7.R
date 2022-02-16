library(ISLR2)
summary(Weekly)
Weekly = na.omit(Weekly)
Weekly2 <- subset(Weekly, select = -(Direction)) 
temp <- as.integer(as.character(Weekly$Direction)=="Up")
Weekly2 <- subset(Weekly, select = -(Direction))
Weekly["Direction"] <- temp # changes the direction value to 1 and
set.seed(1)
attach(Weekly)

# (a)
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(glm.fit)

# (b)
glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = binomial)
summary(glm.fit)

# (c)
predict(glm.fit, Weekly[1,], type="response")
Weekly[1,9]

# (d)
n = nrow(Weekly)
LOOCV.err = rep(0, n)
for(i in 1:n){
  glm.fit = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i,], family = binomial)
  p = predict(glm.fit, Weekly[i,], type="response")
  if((p > 0.5 && Weekly[i,9] == 1) || (p <= 0.5 && Weekly[i,9] == 0)){
    LOOCV.err[i] = 0
  }else{
    LOOCV.err[i] = 1
  }
}

# (e)
mean(LOOCV.err)
