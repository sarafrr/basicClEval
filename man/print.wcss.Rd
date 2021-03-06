% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clEval.R
\name{print.wcss}
\alias{print.wcss}
\title{Prints the information of an object returned by wcss}
\usage{
\method{print}{wcss}(x, ...)
}
\arguments{
\item{x}{Object returned by \link{wcss} function}

\item{...}{Other arguments}
}
\value{
A print with a brief explanation of:\cr
\itemize{
\item WCSSByClByVar - WCSS by cluster by variable
\item WCSSByClB - WCSS by cluster
\item WCSS - Sum of the total WCSS
\item nClusters - number of clusters
\item sizeCl - number of elements by cluster
}
}
\description{
This function prints all the information contained into \link{wcss}
It takes in input an object returned by \link{wcss}
}
