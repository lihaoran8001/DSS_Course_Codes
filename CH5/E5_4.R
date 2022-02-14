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
