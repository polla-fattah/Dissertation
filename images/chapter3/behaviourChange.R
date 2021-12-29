library(mlbench)

set.seed(100)
p <- mlbench.2dnormals(40, 2, 5, 1)

first = which(p$classes==1)
p$x[first, 2] = p$x[first, 2] + 4
plot(p, xlab = 'X', ylab = 'Y', 
     xlim = c(-10, 10),  ylim = c(-10, 10), pch=as.integer(p$classes))
