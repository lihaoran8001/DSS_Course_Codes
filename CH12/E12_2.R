#a
d = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow=4))
clust= hclust(d, method="complete")
plot(clust,xlab= "Clusters", ylab= "Distance between observation")

#b

clust= hclust(d, method="single")
plot(clust,xlab= "Clusters", ylab= "Distance between observation")

#e

plot(hclust(d, method="complete"), labels=c(2,1,4,3), xlab= "Clusters", ylab= "Distance between observation")
