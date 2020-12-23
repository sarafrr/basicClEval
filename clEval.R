####################################################################
## Basic Functions for Cluster Evaluation
####################################################################

####################################################################
## clMeans() function
####################################################################
## Arguments
####################################################################
## data - data frame or matrix
## clClassification - vector of samples' clustering labels
## ... - other arguments
####################################################################
## Value
####################################################################
## List with:
## clCentres - data frame nClustersxncol(data)
## nClusters - number of clusters
## sizeCl - number of elements by cluster
####################################################################
clMeans <- function(data,clClassification,verbose=TRUE,... ) {
  if(is.matrix(data)){
    mat <-data
    df <- as.data.frame(mat)
  }else if(is.data.frame(data)){
    df <- data
    mat<-as.matrix(data)
  }else{
    if(verbose){
      cat("The data has to be a matrix or a dataframe\n",sep="")
    }
    stop()
  }
  if(nrow(mat)!=length(clClassification)){
    if(verbose){
      cat("The classification vector length (",length(clClassification),
          ") differs from the number of observation in the dataset (",nrow(mat),") \n",sep="")
    }
    stop()
  }else{
    sizeCl <- summary(as.factor(clClassification))
    clCenters <- tapply(mat, list(rep(clClassification,ncol(mat)),col(mat)),
                      function(x) mean(x, na.rm=TRUE))
    clCenters <- as.data.frame(clCenters)
    clCenters <- setNames(clCenters, names(df))
  }
  nClusters = nlevels(as.factor(clClassification))
  list(
    clCenters=clCenters,
    nClusters=nClusters,
    sizeCl=sizeCl
  )
}

####################################################################
## clInnerVariances() function
####################################################################
## Arguments
####################################################################
## data - data frame or matrix
## clClassification - vector of samples' clustering labels
## ... - other arguments
####################################################################
## Value
####################################################################
## List with:
## clVariances - data frame nClustersxncol(data)
## nClusters - number of clusters
## sizeCl - number of elements by cluster
####################################################################
clInnerVariances <- function(data,clClassification,verbose=TRUE,... ) {
  if(is.matrix(data)){
    mat <-data
    df <- as.data.frame(mat)
  }else if(is.data.frame(data)){
    df <- data
    mat<-as.matrix(data)
  }else{
    if(verbose){
      cat("The data has to be a matrix or a dataframe\n",sep="")
    }
    stop()
  }
  if(nrow(mat)!=length(clClassification)){
    if(verbose){
      cat("The classification vector length (",length(clClassification),
          ") differs from the number of observation in the dataset (",nrow(mat),") \n",sep="")
    }
    stop()
  }else{
    sizeCl <- summary(as.factor(clClassification))
    clVariances <- tapply(mat, list(rep(clClassification,ncol(mat)),col(mat)),
                        function(x) var(x, na.rm=TRUE))
    clVariances <- as.data.frame(clVariances)
    clVariances <- setNames(clVariances, names(df))
  }
  nClusters = nlevels(as.factor(clClassification))
  list(
    clVariances=clVariances,
    nClusters=nClusters,
    sizeCl=sizeCl
  )
}

####################################################################
## wcss() function
####################################################################
## Arguments
####################################################################
## data - data frame or matrix
## clClassification - vector of samples' clustering labels
## ... - other arguments
####################################################################
## Value
####################################################################
## List with:
## clVariances - data frame nClustersxncol(data)
## nClusters - number of clusters
## sizeCl - number of elements by cluster
####################################################################
wcss <- function(data,clClassification,verbose=TRUE,... ) {
  if(is.matrix(data)){
    mat <-data
    df <- as.data.frame(mat)
  }else if(is.data.frame(data)){
    df <- data
    mat<-as.matrix(data)
  }else{
    if(verbose){
      cat("The data has to be a matrix or a dataframe\n",sep="")
    }
    stop()
  }
  if(nrow(mat)!=length(clClassification)){
    if(verbose){
      cat("The classification vector length (",length(clClassification),
          ") differs from the number of observation in the dataset (",nrow(mat),") \n",sep="")
    }
    stop()
  }else{
    sizeCl <- summary(as.factor(clClassification))
    WCSSByClByVar <- tapply(mat, list(rep(clClassification,ncol(mat)),col(mat)),
                          function(x) var(x, na.rm=TRUE)*(length(x)-1))
    WCSSByClByVar <- as.data.frame(WCSSByClByVar)
    WCSSByClByVar <- setNames(WCSSByClByVar, names(df))
    WCSSByCl <- rowSums(WCSSByClByVar)
    WCSS <- sum(WCSSByCl)
  }
  nClusters = nlevels(as.factor(clClassification))
  list(
    WCSSByClByVar=WCSSByClByVar,
    WCSSByCl = WCSSByCl,
    WCSS = WCSS,
    nClusters=nClusters,
    sizeCl=sizeCl
  )
}

####################################################################
## print.wcss() function
####################################################################
## Arguments
####################################################################
## x - wcss object
## ... - other arguments
####################################################################
## Value
####################################################################
## Print of:
## WCSSByClByVar - Within Cluster Sum of Squares by cluster by variable
## WCSSByCl - Within Cluster Sum of Squares by cluster
## nClusters - number of clusters
## sizeCl - number of elements by cluster
####################################################################
print.wcss <- function(x,...){
  cat("\nWithin Cluster Sum of Squares by cluser by variable:\n")
  print(x$WCSSByClByVar, ...)
  cat("\nWithin Cluster Sum of Squares by cluser:\n")
  print(x$WCSSByCl, ...)
  cat("\nNumber of clusters:\n")
  print(x$nClusters, ...)
  cat("\nNumber of elements by cluster:\n")
  print(x$sizeCl, ...)
  cat("\nAvailable components:\n")
  print(names(x))
  # print the object itself by calling the assigned variable
  # instead of printing the last string
  invisible(x)
}