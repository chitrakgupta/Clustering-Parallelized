# Clustering-Parallelized
The purpose here is to create a 4-dimensioal vector by merging two data files, and then perform clustering on it.
K-medoids clustering is robust to outliers, so this one is being chosen.
Since the number of cluster is unknown, silhouette function is being used to estimate to "goodness" of clsutering

The first part, doing a series of clsutering and calculating the average silhouette score for each, is being done on HPC.
The parallelization makes it faster. The second part simply performs the clustering with the optimized number of clusters.
