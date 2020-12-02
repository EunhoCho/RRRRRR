
# setwd('/home/greenmon/Projects/RRRRRR/')
setwd('/Users/juyeonyoon/Projects/RRRRRR')
# install.packages('stringdist')
# install.packages('ggdendro')

par(family="NanumGothic")

Sys.setlocale("LC_CTYPE", "ko_KR.UTF-8")
library(stringdist)
library(extrafont) 
library(ggdendro)
library(ggplot2)
library(dplyr) 
library(dendextend)
# font_import()
theme_set(theme_grey(base_family='NanumGothic'))
library(readr)

data <- read_csv('dataframes/cluster_features.csv')

data
N <- length(data$location)

elem2list <- function(elem) {
  return(as.list(strsplit(gsub("\\'|,|\\[|\\]", "", as.character(elem)), " "))[[1]])
}

# number of data points: 51

# dist.matrix <- matrix(ncol=N, nrow=N)
# 

# 
# for (i in 1:N) {
#   hashtags <- elem2list(data$hashtags[i])
#   for (j in i:N) {
#     compare <- elem2list(data$hashtags[j])
#     dist_sum <- 0
#     dist_sum_opposite <- 0
#     for (hashtag in hashtags) {
#       dist_sum <- dist_sum + min(stringdist(c(hashtag), compare))
#     }
#     for (hashtag in compare) {
#       dist_sum_opposite <- dist_sum_opposite + min(stringdist(c(hashtag), hashtags))
#     }
#     dist.matrix[j, i] <- (dist_sum / length(hashtags) + dist_sum_opposite / length(compare)) / 2
#   }
# }
# 
# dist.matrix
# 
# rownames(dist.matrix) <- as.character(data$location)
# colnames(dist.matrix) <- as.character(data$location)
# 
# dist <- as.dist(dist.matrix, diag=TRUE)
# 
# hc <- hclust(dist)
# dend <- hc %>% as.dendrogram
# labels <- labels(dend)
# points = c()
# colors = c()
# for (label in labels) {
#   if (substr(label, nchar(label), nchar(label)) == "읍") {
#     points <- append(points, 17)
#     colors <- append(colors, "blue")
#   } else if (substr(label, nchar(label), nchar(label)) == "면") {
#     points <- append(points, 18)
#     colors <- append(colors, "red")
#   } else if (substr(label, nchar(label), nchar(label)) == "동") {
#     points <- append(points, 19)
#     colors <- append(colors, "green")
#   }
# }
# 
# points
# 
# dend %>% set("leaves_pch", points) %>%  # node point type
#   set("leaves_cex", 1) %>%  # node point size
#   set("leaves_col", colors) %>% set("branches_k_color", k = 8)  %>% plot(horiz=TRUE)
# 
# hc





# Use TF-IDF clustering

library(tm)
library(proxy)
library(dplyr)
library(utf8)

data <- read.csv('dataframes/cluster_features_filtered.csv', encoding="utf-8")
doc <- c()
N <- length(data$location)

for (i in 1:N) {
  words <- elem2list(data$hashtags[i])
  print(words)
  sentence <- ''
  for (j in 1:length(words)) {
    word <- words[j]
    if (nchar(word) < 1) {
      continue
    } else if (nchar(word) == 1) {
      sentence <- paste(sentence, word, sep=" ")
    } else if (nchar(word) == 3) {
      sentence <- paste(sentence, word, sep=" ")
    } else {
      index <- 1
      while (index < nchar(word)) {
        subword <- substr(word, start=index, stop=index+1)
        print(subword)
        sentence <- paste(sentence, subword, sep=" ")
        index <- index + 2
      }
    }
  }
  print(sentence)
  doc <- append(doc, sentence)
}

TFIDF <- function(vector) {
  # tf 
  news_corpus  <- Corpus( VectorSource(vector) )
  control_list <- list(removePunctuation = TRUE, tolower = TRUE)
  tf <- TermDocumentMatrix(news_corpus, control = control_list) %>% as.matrix()
  terms <- dimnames(tf)$Terms
  idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) %>% diag()
  return (list(crossprod(tf, idf), terms))
}

# distance between two vectors
Cosine <- function(x, y) {
  similarity <- sum(x * y) / ( sqrt( sum(y ^ 2) ) * sqrt( sum(x ^ 2) ) )
  
  # given the cosine value, use acos to convert back to degrees
  # acos returns the radian, multiply it by 180 and divide by pi to obtain degrees
  return( acos(similarity) * 180 / pi )
}

# tf-idf matrix using news' title 
result <- TFIDF(doc)

hashtag_tf_idf <- result[1][[1]]
terms <- result[2][[1]]

rownames(hashtag_tf_idf) <- data$location
colnames(hashtag_tf_idf) <- terms

write.csv(hashtag_tf_idf, 'tf_idf_matrix.csv')

dist_tf_idf <- dist(hashtag_tf_idf, method = "Cosine")

cluster_tf_idf <- hclust(dist_tf_idf, method="ward.D")
cluster_tf_idf

memb <- cutree(cluster_tf_idf, k = 8)
memb

labels(memb) <- data$location

write.csv(memb, 'cluster_membership.csv')

dend <- cluster_tf_idf %>% as.dendrogram

labels(dend)


labels <- labels(dend)
points <- c()
colors <- c()
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

labels(dend)

dend %>% set("leaves_pch", points) %>%  # node point type
  set("leaves_cex", 1) %>%  # node point size
  set("leaves_col", colors) %>% set("branches_k_color", k = 8)  %>% plot(horiz=TRUE)


