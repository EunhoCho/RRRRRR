
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



# Use TF-IDF clustering

library(tm)
library(proxy)
library(dplyr)

data <- read.csv('dataframes/cluster_features.csv')
doc <- c()
N <- length(data$location)

for (i in 1:2) {
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
      subwords <- substring(word, seq(1, nchar(word)-1, 2), seq(2, nchar(word), 2))
      for (subword in subwords) {
        sentence <- paste(sentence, subword, sep=" ")
      }
    }
  }
  print(sentence)
  doc <- append(doc, sentence)
}
doc

Encoding(tdm$dimnames$Terms) = 'UTF-8'

TFIDF <- function(vector) {
  # tf 
  news_corpus  <- Corpus( VectorSource(vector) )
  control_list <- list(removePunctuation = TRUE, tolower = TRUE)
  tf <- TermDocumentMatrix(news_corpus, control = control_list) %>% as.matrix()
  
  # idf
  idf <- log( ncol(tf) / ( 1 + rowSums(tf != 0) ) ) %>% diag()
  return( crossprod(tf, idf) )
}

# distance between two vectors
Cosine <- function(x, y) {
  similarity <- sum(x * y) / ( sqrt( sum(y ^ 2) ) * sqrt( sum(x ^ 2) ) )
  
  # given the cosine value, use acos to convert back to degrees
  # acos returns the radian, multiply it by 180 and divide by pi to obtain degrees
  return( acos(similarity) * 180 / pi )
}

# tf-idf matrix using news' title 
hashtag_tf_idf <- TFIDF(doc)

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
write.csv(memb, 'cluster_membership.csv')
