rm(list=ls())

setwd("C:/Users/정경원/Desktop/R을 활용한 빅데이터 분석 기초/project/RRRRRR")

Sys.setlocale("LC_ALL", "C")
data <- read.csv("dataframes/posts.csv", header=T, encoding="UTF-8")
Sys.setlocale("LC_ALL", "Korean")
str(data)

data$datetime <- (as.numeric(substr(data$datetime, 12, 13))+9)%%24
hist(data$datetime, breaks=seq(0,24,by=1))

addr <- strsplit(data$address, " ")
for (i in 1:length(addr)){
  data$address[i] <- addr[[i]][3]
}

head(data)

library(dplyr)
library(ggplot2)
library(forcats)

# 6am ~ 5pm
data_6to17 <- filter(data, 6 <= datetime & datetime < 17) %>%
  group_by(address) %>%
  tally(sort=T) %>%
  mutate(rank=order(n, decreasing=T)) %>%
  arrange(rank) %>%
  mutate(address=fct_reorder(address, rank))

ggplot(data_6to17[1:10,], aes(address, n, fill=n)) +
  geom_bar(stat="identity", show.legend=F) +
  xlab("Location (읍/면/동)") +
  ylab("# of posts") +
  ggtitle("Top 10 Locations in Jeju ranked by # of Instagram Posts : 6am~5pm")

# 5pm ~ 10pm
data_17to22 <- filter(data, 17 <= datetime & datetime < 22) %>%
  group_by(address) %>%
  tally(sort=T) %>%
  mutate(rank=order(n, decreasing=T)) %>%
  arrange(rank) %>%
  mutate(address=fct_reorder(address, rank))

ggplot(data_17to22[1:10,], aes(address, n, fill=n)) +
  geom_bar(stat="identity", show.legend=F) +
  xlab("Location (읍/면/동)") +
  ylab("# of posts") +
  ggtitle("Top 10 Locations in Jeju ranked by # of Instagram Posts : 5pm~10pm")

# 10pm ~ 6am
data_22to6 <- filter(data, 22 <= datetime | datetime < 6) %>%
  group_by(address) %>%
  tally(sort=T) %>%
  mutate(rank=order(n, decreasing=T)) %>%
  arrange(rank) %>%
  mutate(address=fct_reorder(address, rank))

ggplot(data_22to6[1:10,], aes(address, n, fill=n)) +
  geom_bar(stat="identity", show.legend=F) +
  xlab("Location (읍/면/동)") +
  ylab("# of posts") +
  ggtitle("Top 10 Locations in Jeju ranked by # of Instagram Posts : 10pm~6am")
