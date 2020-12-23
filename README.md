# basicClEval
Basic functions to evaluate a partition of data (clustered data).

The main objective of this project is to make the basic statistical measures available for clustered data.
It is intended to be applied after the items have been clustered. Therefore, after knowing the labels for each sample in the dataset.

Function |Description|
|--------|-----------|
|```clMeans```|computes the centroid of each cluster|
|```clInnerVariances```  |computes the inner variance of each cluster|
|```wcss```|computes the Within Cluster Sum of Squares of each cluster|
|```print.wcss```|prints the information of an object returned by ```wcss```|
