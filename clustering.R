
# setwd('/home/greenmon/Projects/RRRRRR/')
setwd('/Users/juyeonyoon/Projects/RRRRRR')
# install.packages('stringdist')
# install.packages('ggdendro')

library(stringdist)
library(extrafont) 
library(ggdendro)
library(ggplot2)
library(dplyr) 
library(dendextend)
# font_import()
theme_set(theme_grey(base_family='NanumGothic'))

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
    dist_sum_opposite <- 0
    for (hashtag in hashtags) {
      dist_sum <- dist_sum + min(stringdist(c(hashtag), compare))
    }
    for (hashtag in compare) {
      dist_sum_opposite <- dist_sum_opposite + min(stringdist(c(hashtag), hashtags))
    }
    dist.matrix[j, i] <- (dist_sum / length(hashtags) + dist_sum_opposite / length(compare)) / 2
  }
}
dist.matrix

rownames(dist.matrix) <- as.character(data$location)
colnames(dist.matrix) <- as.character(data$location)

dist <- as.dist(dist.matrix, diag=TRUE)

hc <- hclust(dist)
dend <- hc %>% as.dendrogram
labels <- labels(dend)
points = c()
colors = c()
for (label in labels) {
  if (substr(label, nchar(label), nchar(label)) == "읍") {
    points <- append(points, 17)
    colors <- append(colors, "blue")
  } else if (substr(label, nchar(label), nchar(label)) == "면") {
    points <- append(points, 18)
    colors <- append(colors, "red")
  } else if (substr(label, nchar(label), nchar(label)) == "동") {
    points <- append(points, 19)
    colors <- append(colors, "green")
  }
}

points

dend %>% set("leaves_pch", points) %>%  # node point type
  set("leaves_cex", 1) %>%  # node point size
  set("leaves_col", colors) %>% set("branches_k_color", k = 8)  %>% plot(horiz=TRUE)

hc
memb <- cutree(hc, k = 8)
memb
