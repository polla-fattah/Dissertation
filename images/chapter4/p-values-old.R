require(mcclust)
require(pROC)
require(clv)
require(e1071)
library(dunn.test)

setwd('C://Users/pqf/Google Drive/PhD/Conferances/IntelliSys/Calculate_Cluster_Change_Overtime/')
source('generateData.R')

# External clustering validity measures function with single function call
Rand <- function(clust1, clust2) clv.Rand(std.ext(clust1, clust2))
Jaccard <- function(clust1, clust2) clv.Jaccard(std.ext(clust1, clust2))
FM <- function(clust1, clust2) clv.Folkes.Mallows(std.ext(clust1, clust2))
VI <- function(clust1, clust2) vi.dist(clust1, clust2)
AUC <- function(clust1, clust2) multiclass.roc(clust1, clust2)$auc[1]

# Clustering Algorithms

kmeansClusters <- function(x, n)  kmeans(x, n)$cluster
cmeansClusters <- function(x, n)  cmeans(x, n)$cluster
pamClusters <- function(x, n)  pam(x, n)$clustering
hierClusters <- function(x, n) cutree(hclust(dist(x)), n)


getData <- function(dataset = 'test'){
  if (dataset == 'game10'){
    data <- read.csv('C:/Users/pqf/Google Drive/PhD/Data/game/repeated.csv')
    data <- data[,1:5]
  }
  else if(dataset == 'game27'){
    data <- read.csv('C:/Users/pqf/Google Drive/PhD/Data/game27/game27.csv')
    data <- data[,1:5]
  }
  else if(dataset == 'test'){
    data <- generateData(500)
  }
  else{
    return (NULL)
  }
  return (data)
}

measureChangesStart <- function(allData, clusterFUNC = kmeansClusters){
  set.seed(123)
  
  PERIODS <- max(allData$period)
  CLUSTER_NO <- 4
  
  resultRand <<- c()
  resultJaccard <<- c()
  resultFM <<- c()
  resultVI <<- c()
  resultAUC <<- c()
  
  # Cluster the first time point to be compared with all other time points
  data1 <<- allData[which(allData$period == 1), -c(1,2)]
  clusters1 <<- clusterFUNC(data1, CLUSTER_NO)
  
  #cluster all other time points and compare them with the first one.
  for(period in 2:PERIODS){
    data <- allData[which(allData$period == period), -c(1,2)]
    clusters <<- clusterFUNC(data, CLUSTER_NO)
    
    resultRand[period-1] <<- Rand(clusters1, clusters)
    resultJaccard[period-1] <<- Jaccard(clusters1, clusters)
    resultFM[period-1] <<- FM(clusters1, clusters)
    resultVI[period-1] <<- VI(clusters1, clusters)
    resultAUC[period-1] <<- AUC(clusters1, clusters)
    
  }
  
  data.frame(resultRand, resultJaccard, resultFM, resultVI, resultAUC)
}

measureChangesConsiquent <- function(allData, clusterFUNC = kmeansClusters){
  set.seed(123)
  
  PERIODS <- max(allData$period)
  CLUSTER_NO <- 4
  
  resultRand <<- c()
  resultJaccard <<- c()
  resultFM <<- c()
  resultVI <<- c()
  resultAUC <<- c()
  
  # Cluster the first time point to be compared with all other time points
  data1 <<- allData[which(allData$period == 1), -c(1,2)]
  clusters1 <<- clusterFUNC(data1, CLUSTER_NO)
  
  #cluster all other time points and compare them with the first one.
  for(period in 1:PERIODS){
    data <- allData[which(allData$period == period), -c(1,2)]
    clusters2 <<- clusterFUNC(data, CLUSTER_NO)
    
    resultRand[period-1] <<- Rand(clusters1, clusters2)
    resultJaccard[period-1] <<- Jaccard(clusters1, clusters2)
    resultFM[period-1] <<- FM(clusters1, clusters2)
    resultVI[period-1] <<- VI(clusters1, clusters2)
    resultAUC[period-1] <<- AUC(clusters1, clusters2)
    
    clusters1 <<- clusters2
    
  }
  
  data.frame(resultRand, resultJaccard, resultFM, resultVI, resultAUC)
}

plotChanges <- function(f, x, dataset){
  # Plotting results
  #x = 1:(PERIODS - 1)
  
  par(mar = c(3, 2, 1, 1)) ## default is c(5,4,4,2) + 0.1
  
  plot(x, f$resultRand, pch = 16, ylim=c(0, 1), xlab = 'time points', 
       ylab = '', xaxt = "n", mgp = c(2, 1, 0))
  axis(side = 1, tck=-.05,
       at = c(1, 4, 9, 14, 19, 24), 
       labels = c('1-2', '4-5', '9-10', '14-15', '19-20', '24-25'))
  lines(resultRand)
  
  points(x, f$resultJaccard, pch = 1)
  lines(f$resultJaccard)
  
  points(x, f$resultFM, pch = 2)
  lines(f$resultFM)
  
  #change VI to fit with other metrics
  resultVI = 1 - (f$resultVI / max(f$resultVI))
  points(x, resultVI, pch = 4)
  lines(resultVI)
  
  points(x, f$resultAUC, pch = 0)
  lines(f$resultAUC)
  
  
  abline(lm(f$resultAUC~x), lwd=2)
  if(dataset == 'test')
    legend('bottomleft', c('Rand', 'Jaccard', 'FM', 'VI','AUC' ), #topright, bottomleft
           pch=c(16, 1, 2, 4, 0), bty='n', cex=1.3)
  else
    legend('topright', c('Rand', 'Jaccard', 'FM', 'VI','AUC' ), #topright, bottomleft
           pch=c(16, 1, 2, 4, 0), bty='n', cex=1.3)
}

datasets <- c('test', 'game10', 'game27')
clustFunc <- c(kmeansClusters, cmeansClusters, 
               pamClusters, hierClusters)
clustFuncStr <- c('kmeansClusters', 'cmeansClusters', 
                  'pamClusters', 'hierClusters')

resultVecrorsFirs <- list()
resultFrameFirs <- list()

for(dataset in datasets){
  allData <- getData(dataset)
  x <- 1:(max(allData$period)-1)
  i <- 1
  
  for(fun in clustFunc){
    index = paste0(dataset, '_', clustFuncStr[i])
    
    rFirs <- measureChangesStart(allData , clusterFUNC = fun)
    
    #    png(file=paste0(dataset, '_', clustFuncStr[i], '_Firs.png'))
    #    plotChanges(rFirs, x, dataset)
    #    dev.off()
    
    resultVecrorsFirs[[index]] <- as.vector(t(rFirs))
    resultFrameFirs[[index]] <- as.vector(rFirs)
    i = i + 1
  }
}

resultVectorsCons <- list()
resultFrameCons <- list()

for(dataset in datasets){
  allData <- getData(dataset)
  x <- 1:(max(allData$period)-1)
  i <- 1
  
  for(fun in clustFunc){
    index = paste0(dataset, '_', clustFuncStr[i])
    
    rCons <- measureChangesConsiquent(allData , clusterFUNC = fun)
    
    #    png(file=paste0(dataset, '_', clustFuncStr[i], '_Cons.png'))
    #    plotChanges(rCons, x, dataset)
    #    dev.off()
    
    resultVectorsCons[[index]] <- as.vector(t(rCons))
    resultFrameCons[[index]] <- as.vector(rCons)
    
    i = i + 1
  }
}

# clusters Validity dataset p-values
clustersValidity <- function(resultVecrors) {
  for(dataset in datasets){

    clusterList <<- list()
    for(funStr in clustFuncStr){
      index = paste0(dataset, '_', funStr)
      clusterList[[funStr]] <<- resultVecrors[[index]]
      
    }   
    cat("-------------  ", dataset , "  ----------------\n\n")
    cat('All Samples = ', friedman.test(as.matrix(as.data.frame(clusterList)))$p.value, '\n\n')
    
    cat('kmeans, cmeans = ',wilcox.test(clusterList$kmeansClusters, clusterList$cmeansClusters)$p.value, '\n' )
    cat('kmeans, pam    = ',wilcox.test(clusterList$kmeansClusters, clusterList$pamClusters)$p.value , '\n')
    cat('kmeans, hier   = ',wilcox.test(clusterList$kmeansClusters, clusterList$hierClusters)$p.value , '\n')
    cat('cmeans, pam    = ',wilcox.test(clusterList$cmeansClusters, clusterList$pamClusters)$p.value , '\n')
    cat('cmeans, hier   = ',wilcox.test(clusterList$cmeansClusters, clusterList$hierClusters)$p.value , '\n')
    cat('pam,    hier   = ',wilcox.test(clusterList$pamClusters, clusterList$hierClusters)$p.value, '\n' )
  }
}
#cat("P-value check for test data set using first time point as reference of behaviour\n")
#clustersValidity(resultVecrorsFirs)
#cat("P-value check for test data set using qonsiquent time point as reference of behaviour\n")
#clustersValidity(resultVectorsCons)


# measures Validity p-values
measuresValidity <- function(resultFrame) {

  for(dataset in datasets){
  	if(dataset != 'test')
  		break
  	measuerList <<- list()
  	
    for(measuerStr in c('resultRand', 'resultJaccard',  'resultFM', 'resultVI', 'resultAUC')){
      
      for(clustStr in clustFuncStr){
      index = paste0(dataset, '_', clustStr)
      measuerList[[measuerStr]] <<- c(measuerList[[measuerStr]], resultFrame[[index]][[measuerStr]])
      }
      
    } 

  	cat("-------------  ", dataset , ' - ', measuerStr, "  ----------------\n\n")
  	
  	#   cat('All Samples = ', friedman.test(as.matrix(as.data.frame(measuerList)))$p.value, '\n\n')
  	#   cat( wilcox.test(measuerList$kmeansClusters, measuerList$cmeansClusters)$p.value , '\n')
  	#   cat(wilcox.test(measuerList$kmeansClusters, measuerList$pamClusters)$p.value , '\n')
  	#   cat( wilcox.test(measuerList$kmeansClusters, measuerList$hierClusters)$p.value , '\n')
  	#   cat(wilcox.test(measuerList$cmeansClusters, measuerList$pamClusters)$p.value , '\n')
  	#   cat( wilcox.test(measuerList$cmeansClusters, measuerList$hierClusters)$p.value , '\n')
  	#  cat( wilcox.test(measuerList$pamClusters, measuerList$hierClusters)$p.value , '\n')
  	
  }
}
cat("P-value check for test data set using first time point as reference of behaviour\n")
measuresValidity(resultFrameFirs)
#cat("P-value check for test data set using qonsiquent time point as reference of behaviour\n")
#measuresValidity(resultFrameCons)



#stargazer(summaryTable(brCa, 1:(ncol(brCa) - 2)), 
#          summary = F, title = 'Measures of centrality, 
#          dispersion, and number of missing values 
#          that each attribute has in the breast cancer data set.', 
#          label = 'tab:describeData', rownames = F)

