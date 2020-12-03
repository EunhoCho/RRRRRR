
# setwd('/home/greenmon/Projects/RRRRRR/')
setwd('/Users/juyeonyoon/Projects/RRRRRR')
library(dplyr)

data <- read.csv('dataframes/location_tags.csv')

mo = matrix(c(data$count), dimnames=list(c(data$location), c('count')))
as.table(mo)
chisq.test(mo)


# X-squared = 1902.1, df = 25, p-value < 2.2e-16
# the popularity of travel spots are not equally divided

data <- read.csv('dataframes/location_tags_restaurant.csv')

mo = matrix(c(data$count), dimnames=list(c(data$location), c('count')))
as.table(mo)
chisq.test(mo)

# X-squared = 1472.9, df = 21, p-value < 2.2e-16
# the popularity of restaurant travel spots are not equally divided


data <- read.csv('dataframes/location_tags_cafe.csv')

# install.packages("extrafont") 
library(extrafont) 
font_import()


mo = matrix(c(data$count), dimnames=list(c(data$location), c('count')))
as.table(mo)
chisq.test(mo)

# X-squared = 478.36, df = 17, p-value < 2.2e-16
# the popularity of travel spots are not equally divided


data <- read.csv('dataframes/location_tags_pub.csv')

mo = matrix(c(data$count), dimnames=list(c(data$location), c('count')))
as.table(mo)
chisq.test(mo)

# X-squared = 160.79, df = 5, p-value < 2.2e-16
# the popularity of travel spots are not equally divided


data <- read.csv('dataframes/location_tags_onemonth.csv')
mo = matrix(c(data$count), dimnames=list(c(data$location), c('count')))
as.table(mo)
chisq.test(mo)

# X-squared = 8.5455, df = 4, p-value = 0.07352
# cannot conclude difference in travel spots of one-month live

data2 <- read.csv('dataframes/location_tags.csv')
library(ggplot2)
theme_set(theme_grey(base_family='NanumGothic'))

ggplot(data2, aes(x=place, y=count)) +
  geom_boxplot(outlier.color = 'red') + 
  stat_summary(fun.y="mean", geom="point", shape=22, size=3, fill="blue") 

ggplot(data2, aes(x=location, y=count)) +
  geom_boxplot(outlier.color = 'red') + 
  stat_summary(fun.y="mean", geom="point", shape=22, size=3, fill="blue") 

mo = matrix(c(data2$count), dimnames=list(c(data2$location), c('count')))
as.table(mo)
chisq.test(mo)



data <- read.csv('cluster_themes.csv')
kinds <- data[, 4:7]

par(mar=c(15,4,4,2))
end_point = 0.5 + nrow(kinds) + nrow(kinds) - 1 

barplot(t(kinds), main="Travel Spot Characteristics", col=c("#FF6666", "#6666CC", "#009900", "#00CCFF"), ylab="# of Posts", space=1)

legend("topright",names(kinds), fill=c("#FF6666", "#6666CC", "#009900", "#00CCFF"))
text(seq(1.5, end_point, by = 2), par("usr")[3]-0.25, 
     srt = 60, adj = 1, xpd = TRUE,
     labels = paste(data$locations), cex = 0.85)

