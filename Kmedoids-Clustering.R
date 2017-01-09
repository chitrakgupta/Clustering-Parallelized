# The objective is to cluster a series of 4-dimensional vectors where the optimal number of clusters has been found out.S

# This step comes after the "Kmedoids-Silhouette.R" has been run.
# In other words, we already know how many clusters exist in each of our data set.

# Note that parallelization is not invoked here because this one does not take that long.

# This one defines the number of clusters in the data sets, in order
listOfNclust <- c(6,5,5)
weight <- 40

library(cluster)

# List of the data files
#       This file has 3 of the 4 dimensions
listOfHelicities <- c("nHelices-Garcia-300K-run1.dat", "nHelices-Garcia-300K-run2.dat", "nHelices-Garcia-300K-run3.dat")
#       This file has the 4th dimension
listOfRgs <- c("Rg-300K-run1.dat", "Rg-300K-run2.dat", "Rg-300K-run3.dat")


for (i in 1:length(listOfHelicities)) {
	nClust <- listOfNclust[i]

	dat1 <- readLines(listOfHelicities[i])
	t1 <- dat1[2:length(dat1)]
	t1prime <- strsplit(t1, "\t")
	Ns <- sapply(1:length(t1prime), function(i) t1prime[[i]][2])
	vecNs <- as.numeric(Ns)
	N <- sapply(1:length(t1prime), function(i) t1prime[[i]][3])
	vecN <- as.numeric(N)
	Nc <- sapply(1:length(t1prime), function(i) t1prime[[i]][4])
	vecNc <- as.numeric(Nc)

	dat2 <- readLines(listOfRgs[i])
	t2prime <- strsplit(dat2," ")
	Rg <- sapply(1:length(t2prime), function(i) t2prime[[i]][2])
	vecRg <- as.numeric(Rg)

# This is where the weight is being put in, and the matrix for clustering is being created
	newDat <- cbind(vecNs*weight, vecN, vecNc, vecRg)
	colnames(newDat)[1] <- "wNs"
	initTime <- proc.time()
	ClustRes <- pam(newDat, nClust)
	timeTaken <- proc.time() - initTime
	print(c("Time taken: ", timeTaken[3]))

	outFileName1 <- paste("Clusters",substr(listOfHelicities[i], nchar(listOfHelicities[i])-13,nchar(listOfHelicities[i])), sep="")
	frames <- sapply(1:length(t1prime), function(i) t1prime[[i]][1])	
	write.table(cbind(frames,ClustRes$cluster), file=outFileName1, col.names=c("#Frame", "Cluster"), row.names=FALSE, quote=FALSE)

# Remember to remove the weights before writing into the file.
	AllNs <- ClustRes$medoids[,"wNs"]/weight
	AllN <- ClustRes$medoids[,"vecN"]
	AllNc <- ClustRes$medoids[,"vecNc"]
	AllRg <- ClustRes$medoids[,"vecRg"]

	outFileName2 <- paste("Medoids", substr(listOfHelicities[i], nchar(listOfHelicities[1])-13, nchar(listOfHelicities[i])), sep="")
	write.table(cbind((1:nClust), ClustRes$medoids), file=outFileName2, sep="\t",col.names=c("#Cluster",colnames(ClustRes$medoids)), row.names=FALSE, quote=FALSE)
}
