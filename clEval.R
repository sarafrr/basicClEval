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
## clCentres - data frame nrow(data)xnClusters which has as columns
##             the means of each variable
## nClusters - number of clusters
## clLabels - clustering labels
####################################################################