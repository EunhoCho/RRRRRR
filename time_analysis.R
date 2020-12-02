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

# remove duplicated keys
library(tidyverse)
data <- data[!duplicated(data$key),]
str(data)

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
str(data_6to17)

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



##############


# 12:00 ~ 18:00
data_12to18 <- filter(data, 12 <= datetime & datetime < 18) %>%
  group_by(address) %>%
  tally(sort=T) %>%
  mutate(rank=order(n, decreasing=T)) %>%
  arrange(rank) %>%
  mutate(address=fct_reorder(address, rank))

ggplot(data_12to18[1:10,], aes(address, n, fill=n)) +
  geom_bar(stat="identity", show.legend=F) +
  xlab("Location (읍/면/동)") +
  ylab("# of posts") +
  ggtitle("Top 10 Locations in Jeju ranked by # of Instagram Posts : 12:00 ~ 18:00")


# 18:00 ~ 24:00
data_18to24 <- filter(data, 18 <= datetime & datetime < 24) %>%
  group_by(address) %>%
  tally(sort=T) %>%
  mutate(rank=order(n, decreasing=T)) %>%
  arrange(rank) %>%
  mutate(address=fct_reorder(address, rank))

ggplot(data_18to24[1:10,], aes(address, n, fill=n)) +
  geom_bar(stat="identity", show.legend=F) +
  xlab("Location (읍/면/동)") +
  ylab("# of posts") +
  ggtitle("Top 10 Locations in Jeju ranked by # of Instagram Posts : 18:00 ~ 24:00")


# 24:00 ~ 07:00
data_24to07 <- filter(data, datetime < 7) %>%
  group_by(address) %>%
  tally(sort=T) %>%
  mutate(rank=order(n, decreasing=T)) %>%
  arrange(rank) %>%
  mutate(address=fct_reorder(address, rank))

ggplot(data_24to07[1:10,], aes(address, n, fill=n)) +
  geom_bar(stat="identity", show.legend=F) +
  xlab("Location (읍/면/동)") +
  ylab("# of posts") +
  ggtitle("Top 10 Locations in Jeju ranked by # of Instagram Posts : 00:00 ~ 07:00")


# location - # of posts by time

hanlim <- c(36,54,6)
awol <- c(30,77,2)
anduk <- c(27,56,9)
guja <- c(20,56,5)
sungsan <- c(20,39,10)

d <- data.frame(time=rep(c("12~18","18~24","24~07"),5),
                posts=c(hanlim, awol, anduk, guja, sungsan),
                location=c(rep("한림읍",3),rep("애월읍",3),rep("안덕면",3),rep("구좌읍",3),rep("성산읍",3)))

ggplot(d, aes(x=location, y=posts, fill=time)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Location (읍/면/동)") +
  ylab("# of posts")


# ratio graph

af <- sum(data_12to18$n)
ev <- sum(data_18to24$n)
ni <- sum(data_24to07$n)

hanlim2 <- c(36/af,54/ev,6/ni)
awol2 <- c(30/af,77/ev,2/ni)
anduk2 <- c(27/af,56/ev,9/ni)
guja2 <- c(20/af,56/ev,5/ni)
sungsan2 <- c(20/af,39/ev,10/ni)

d2 <- data.frame(time=rep(c("12~18","18~24","24~07"),5),
                posts=c(hanlim2, awol2, anduk2, guja2, sungsan2),
                location=c(rep("한림읍",3),rep("애월읍",3),rep("안덕면",3),rep("구좌읍",3),rep("성산읍",3)))

ggplot(d2, aes(x=location, y=posts, fill=time)) +
  geom_bar(stat="identity", position="dodge") +
  xlab("Location (읍/면/동)") +
  ylab("posts ratio")


# chi-square test

t <- array(c(hanlim, awol, anduk, guja, sungsan), dim=c(3,5),
           dimnames=list("Time"=c("12~18","18~24","24~07"),
                         "Location (읍/면/동)"=c("한림읍","애월읍","안덕면","구좌읍","성산읍")))
chisq.test(as.table(t))

# Pearson's Chi-squared test
# data:  as.table(t)
# X-squared = 16.151, df = 8, p-value = 0.04027

library(grid); library(vcd)
mosaic(as.table(t))           
