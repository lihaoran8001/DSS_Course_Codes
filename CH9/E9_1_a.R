x1 = -20:20
x2 = 1 + 3 * x1
plot(x1, x2, xlab= expression ("X"[1]), ylab= expression ("X"[2]), type = "l", col = "red")
text(c(0), c(-20), expression ("1 + 3X"[1]+ X[2]> 0), col = "black")
text(c(0), c(20), expression ("1 + 3X"[1]+ X[2]< 0), col = "Black")

