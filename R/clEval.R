#' Compute centroids
#'
#' This function computes the centroids for each cluster.
#' It takes in input a data.frame or a matrix of data and a vector
#' containing the classification of each sample.
#'
#' @param data Dataset of class type data.frame or matrix
#' @param clClassification Vector of samples' clustering labels
#' @param ... Other arguments
#' @return A list with:\cr
#' * clCentres - data frame of means nClustersxncol(data)
#' * nClusters - number of clusters
#' * sizeCl - number of elements by cluster
#' @export
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

#' Compute clusters' variances
#'
#' This function computes the inner variance for each cluster.
#' It takes in input a data.frame or a matrix of data and a vector
#' containing the classification of each sample.
#'
#' @param data Dataset of class type data.frame or matrix
#' @param clClassification Vector of samples' clustering labels
#' @param ... Other arguments
#' @return A list with:\cr
#' * clVariances - data frame of variances nClustersxncol(data)
#' * nClusters - number of clusters
#' * sizeCl - number of elements by cluster
#' @export
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

#' Compute clusters' Within Cluster Sum of Squares
#'
#' This function computes the Within Cluster Sum of Squares WCSS for each cluster.
#' It takes in input a data.frame or a matrix of data and a vector
#' containing the classification of each sample.
#'
#' @param data Dataset of class type data.frame or matrix
#' @param clClassification Vector of samples' clustering labels
#' @param ... Other arguments
#' @return A list with:\cr
#' * WCSSByClByVar - WCSS by cluster by variable
#' * WCSSByClB - WCSS by cluster
#' * WCSS - Sum of the total WCSS
#' * nClusters - number of clusters
#' * sizeCl - number of elements by cluster
#' @export
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

#' Prints the information of an object returned by wcss
#'
#' This function prints all the information contained into [wcss]
#' It takes in input an object returned by [wcss]
#'
#' @param x Object returned by [wcss] function
#' @param ... Other arguments
#' @return A print with a brief explanation of:\cr
#' * WCSSByClByVar - WCSS by cluster by variable
#' * WCSSByClB - WCSS by cluster
#' * WCSS - Sum of the total WCSS
#' * nClusters - number of clusters
#' * sizeCl - number of elements by cluster
#' @export
print.wcss <- function(x,...){
  cat("\nWithin Cluster Sum of Squares by cluster by variable:\n")
  print(x$WCSSByClByVar, ...)
  cat("\nWithin Cluster Sum of Squares by cluster:\n")
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
