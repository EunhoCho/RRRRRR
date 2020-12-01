rm(list=ls())
setwd('/home/greenmon/Projects/RRRRRR/')
# install.packages('stringdist')

library(stringdist)

data <- read.csv('dataframes/cluster_features.csv')
# 
N <- length(data$location)
# number of data points: 51

dist.matrix <- matrix(ncol=N, nrow=N)

elem2list <- function(elem) {
  return(as.list(strsplit(gsub("\\'|,|\\[|\\]", "", as.character(elem)), " "))[[1]])
}

for (i in 1:N) {
  hashtags <- elem2list(data$hashtags[i])
  for (j in i:N) {
    compare <- elem2list(data$hashtags[j])
    dist_sum <- 0
    for (hashtag in hashtags) {
      dist_sum <- dist_sum + min(stringdist(c(hashtag), compare))
    }
    i
    j
    dist.matrix[j, i] <- dist_sum / length(hashtags)
  }
}
dist.matrix

rownames(dist.matrix) <- as.character(data$location)
colnames(dist.matrix) <- as.character(data$location)

dist <- as.dist(dist.matrix, diag=TRUE)

hc <- hclust(dist)
hc
plot(hc)
