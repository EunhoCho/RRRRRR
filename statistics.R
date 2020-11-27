
setwd('/home/greenmon/Projects/RRRRRR/')
data <- read.csv('location_tags.csv')

mo = matrix(c(data$count), dimnames=list(c(data$location), c('count')))
as.table(mo)
chisq.test(mo)

# p-value = 0.007383
# the popularity of each travel spots in Jeju not equally divided