# http://www.sthda.com/english/wiki/cluster-analysis-in-r-unsupervised-machine-learning

# Install factoextra
#install.packages("factoextra")
# Install cluster package
#install.packages("cluster")
#install.packages("ggplot2")

library("cluster")
library("factoextra")
library("ggplot2")



# 1. Loading and preparing data
data("USArrests")
my_data <- scale(USArrests)

# 2. Compute dissimilarity matrix
d <- dist(my_data, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 4, border = 2:5) # add rectangle



# 1. Loading and preparing data
data("USArrests")
my_data <- scale(USArrests)

# Cluster number
k_number = 3


# 2. Compute dissimilarity matrix
d <- dist(my_data, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
grp <- cutree(res.hc, k = k_number)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = k_number, border = 2:5) # add rectangle


