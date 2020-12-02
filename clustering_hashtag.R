
library(dplyr)
library(ggplot2)
library(factoextra)

library(cluster)
library(GrpString)

data <- read.csv('./dataframes/locations_coord.csv')
data

hashtags <- as.character(unique(data$hashtag))
hashtags

hashtags <- read.csv('./dataframes/hashtags_df.csv')

distmatrix <- stringdistmatrix(hashtags)
dist.matrix <- as.matrix(distmatrix)

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
