# (a)
x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
color = c("red", "red", "red", "red", "blue", "blue", "blue")
plot(x1, x2, col=color, xlim=c(0, 5), ylim=c(0, 5))

# (b)
plot(x1, x2, col = color, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)

# (d)
abline(lty = 2, 0, 1)
abline(lty = 2, -1, 1)

# (g)
plot(x1, x2, col = color, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.2, 1)

#(h)
plot(x1, x2, col = color, xlim = c(0, 5), ylim = c(0, 5))
points(c(5), c(1), col = c("red"))
