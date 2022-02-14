library(ISLR2)
summary(Weekly)
Weekly = na.omit(Weekly)
Weekly2 <- subset(Weekly, select = -(Direction)) 
temp <- as.integer(as.character(Weekly$Direction)=="Up")
Weekly2 <- subset(Weekly, select = -(Direction))
Weekly["Direction"] <- temp # changes the direction value to 1 and