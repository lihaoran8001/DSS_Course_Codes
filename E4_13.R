Weekly <- read.csv("./data/Weekly.csv")
library(ISLR2)
summary(Weekly)

#this temporary delete Direction variable that allows execution of pairs()
pairs(Weekly)
cor(Weekly[, -9])
cor(Weekly2[, -9])
attach(Weekly)

#transform Direction chr (char) variable into num 
as.integer(as.character(Weekly$Direction)=="Up")

#before-glm.fit we have to insert into Weekly dataset the transformed value from chr to int, but how? 
temp <- as.integer(as.character(Weekly$Direction)=="Up")
Weekly2 <- subset(Weekly, select = -(Direction))
Weekly <- cbind(temp, Direction)


glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly)
summary(glm.fit)
