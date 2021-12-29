library(fpc)
library(cluster)
library(clv)

mydataAll <- read.csv('C:/Users/pqf/Google Drive/PhD/Data/game/repeated.csv')
#mydataAll <- mydataAll[, 3:5]

for (p in 1:10){
  mydata = mydataAll[which(mydataAll$period == p), 4:5]
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
  png(filename=paste0('C:/Users/pqf/Google Drive/PhD/Thesis/images/chapter4/elbowNumberOfClusters', p, '.png'))
  par(cex=1.3)
  plot(1:15, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares", main = paste0('Time Period ', p))
  dev.off()
}

Rand <- function(clust1, clust2) clv.Rand(std.ext(clust1, clust2))
Jaccard <- function(clust1, clust2) clv.Jaccard(std.ext(clust1, clust2))

validity = 0

for (p in 1:10){
  mydata = mydataAll[which(mydataAll$period == p), 4:20]
  
  for (i in 1:15) 
    validity[i] <- Rand(kmeans(mydata,
                centers=i)$cluster, 
                mydataAll[which(mydataAll$period == p), 1])
  png(filename=paste0('C:/Users/pqf/Google Drive/PhD/Thesis/images/chapter4/elbowMemberShipAgreement', p, '.png'))
  par(cex=1.3)
  plot(1:15, validity, type="b", xlab="Number of Clusters",
       ylab="Rand index", main = paste0('Time Period ', p))
  dev.off()
  
}




