library("ggplot2")
p = seq(0, 1, 0.01)
gini = p * (1 - p) * 2
entropy = -(p * log(p) + (1 - p) * log(1 - p))
class.err = 1 - pmax(p, 1 - p)
data <- data.frame(p = p, gini = gini, entropy = entropy, error= class.err)
plot<- ggplot(data, aes(p)) + 
  geom_line(aes(y = gini, colour = "gini")) +
  geom_line(aes(y = entropy, colour = "entropy")) +  
  geom_line(aes(y = error, colour = "class error"))
plot <- plot + labs(x = expression(hat(p)[m1]), y = "gini, entropy, class-error")
plot
