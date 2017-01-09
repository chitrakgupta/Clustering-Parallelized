#!/bin/env Rscript

# The objective is to cluster a series of 4-dimensional vectors where the optimal number of clusters is unknown.

# We are using K-medoids clustering. The common practice is to use K-means, but K-medoids is more robust to outliers. We shall use silhouette function to estimate the "goodness" of clustering.

# A series of clustering is performed, each time with a different number of clusters. Average silhouette function is calculated and dumped into a data file. The optimal number of clusters should give the highest average silhouette function.

# Data needs to be combined from two different data files. One file has 3 variables, the other has 1.

# One of the dimensions need to be weighted by a factor of 40.
weight <- 40

library(cluster)

# Define the start, end and step size of loop for number of clusters.
start <- 2
stop <- 10
step <- 1

# Parallelization
library(parallel)
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
clusterEvalQ(cl, library(cluster))

# The analyis needs to be repeated on 3 sets of data.
# This is being done in a for-loop.

# List of the data files
#	This file has 3 of the 4 dimensions
listOfHelicities <- c("nHelices-Garcia-300K-run1.dat", "nHelices-Garcia-300K-run2.dat", "nHelices-Garcia-300K-run3.dat")
#	This file has the 4th dimension
listOfRgs <- c("Rg-300K-run1.dat", "Rg-300K-run2.dat", "Rg-300K-run3.dat")

allList <- vector()


initTime <- proc.time()
for (i in 1:length(listOfHelicities)) {
	dat1 <- readLines(listOfHelicities[i])
	t1 <- dat1[2:length(dat1)]
	t1prime <- strsplit(t1, "\t")
	Ns <- sapply(1:length(t1prime), function(i) t1prime[[i]][2])
	vecNs <- as.numeric(Ns)
	N <- sapply(1:length(t1prime), function(i) t1prime[[i]][3])
	vecN <- as.numeric(N)
	Nc <- sapply(1:length(t1prime), function(i) t1prime[[i]][4])
	vecNc <- as.numeric(Nc)

	dat2 <- readLines("Rg-300K-run3.dat")
	t2prime <- strsplit(dat2," ")
	Rg <- sapply(1:length(t2prime), function(i) t2prime[[i]][2])
	vecRg <- as.numeric(Rg)

# This is where the weight is being put in, and the matrix for clustering is being created
	newDat <- cbind(vecNs*weight, vecN, vecNc, vecRg)

	clusterExport(cl, "newDat")

# Note that "parSapply" is used instead of "sapply".
	silList <- parSapply(cl, seq(start,stop,step), function(i) {
		temp <- summary(silhouette(pam(newDat, i)))$avg.width
		print(c(i,temp))
		temp
	})
	allList <- cbind(allList,silList)
}
timeTaken <- proc.time() - initTime
print(c("Time taken: ", timeTaken[3]))

dataToWrite <- cbind(seq(start,stop,step), allList)
write.table(dataToWrite, file="silhouette-300K-3in1.dat", col.names=c("#nClust", "Run1", "Run2", "Run3"), row.names=FALSE, quote=FALSE)
