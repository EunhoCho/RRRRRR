# Visualize map

library(ggplot2)
require(maps)
library(ggmap)
library(googleway)
library(ggrepel)
library(dplyr)
library(tidyr)
library(extrafont)
font_import()

par(family="AppleGothic")

theme_set(theme_void())

key = "AIzaSyBGuxb5QupCTjLQjvk_Au_3CenPYBGfISY"

register_google(key = key)
geo <- geocode("hallasan")
center_loc = c(geo$lon, geo$lat)
jeju <- get_googlemap(center = center_loc, zoom = 10, maptype = "roadmap", color = "bw")

locations_csv <- read.csv("./dataframes/locations_coord.csv") 

address <- strsplit(locations_csv$address, " ")
length(address)
str(locations_csv)

for (i in 1:length(address)){
  
  address[[i]][3]<-gsub("1", "일", address[[i]][3])
  address[[i]][3]<-gsub("2", "이", address[[i]][3])
  address[[i]][3]<-gsub("3", "삼", address[[i]][3])
  locations_csv$address[i] <- address[[i]][3]
}

par(family="AppleGothic")
ggmap(jeju) + geom_point(data = locations_csv, aes(x = loc_x, y = loc_y, col = address))+ggtitle("location of instagram posts")+
  theme_minimal(base_family = "AppleGothic")

library(raster)
library(rgeos)
library(maptools)

map_shape <- shapefile("./map_data/EMD_202005/EMD.shp")
map<- spTransform(map_shape, CRS("+proj=longlat"))
map <- fortify(map, region = "EMD_CD")
str(map)

emd_code <- read.csv("./map_data/제주도법정코드.csv")

count<-locations_csv %>% group_by(address)%>% tally(sort=TRUE)
#count$n <- log2(count$n)
count <- rename(count, "num_of_hashtag" = "n")
count<-rename(count,"dong" = "address")
count <- left_join(count, emd_code, by = "dong")
count<-rename(count,"id" = "code")
count
count$id <- count$id/100


map$id <- as.numeric(map$id)
jeju_map <- map[map$id >= 50000000, ]
jeju_map <- jeju_map[jeju_map$lat <= 33.9, ]
str(jeju_map)


str(count)

Map_merged <- left_join(jeju_map, count, by = "id")

str(Map_merged)


ggmap(jeju) +
  geom_polygon(data = Map_merged, aes(x = long, y = lat, group = group, fill = num_of_hashtag),color = "white")  +
  scale_fill_gradient(low = "#FBCF61",
                      high = "#fc0303",
                      space = "Lab",
                      guide = "colourbar")+
  ggtitle("heatmap based on number of hashtages")


# time analysis added
df_for_time <- locations_csv
df_for_time$datetime <- (as.numeric(substr(df_for_time$datetime, 12, 13))+9)%%24
str(df_for_time)

df_12to18 <- filter(df_for_time, 12 <= datetime & datetime < 18)
df_18to24 <- filter(df_for_time, 18 <= datetime & datetime < 24)
df_0to7 <- filter(df_for_time, 0 <= datetime | datetime < 7)

emd_code <- read.csv("./map_data/제주도법정코드.csv")

count<-df_0to7 %>% group_by(address)%>% tally(sort=TRUE)
count$n <- log2(count$n)
count <- rename(count, "num_of_hashtag_logscale" = "n")
count<-rename(count,"dong" = "address")
count <- left_join(count, emd_code, by = "dong")
count<-rename(count,"id" = "code")
count
count$id <- count$id/100

map$id <- as.numeric(map$id)
jeju_map <- map[map$id >= 50000000, ]
jeju_map <- jeju_map[jeju_map$lat <= 33.9, ]
str(jeju_map)


str(count)

Map_merged <- left_join(jeju_map, count, by = "id")


str(Map_merged)

ggmap(jeju) +
  geom_polygon(data = Map_merged, aes(x = long, y = lat, group = group, fill = num_of_hashtag_logscale),color = "white")  +
  scale_fill_gradient(low = "#FBCF61",
                      high = "#fc0303",
                      space = "Lab",
                      guide = "colourbar")+
  ggtitle("heatmap based on num of hashtages: 0AM~7AM (logscale)")


# visualize clustering result
source('./clustering.R')

memb_df <- data.frame(t(data.frame(as.list(memb))[-1]))

rownames(memb_df) <- gsub("1", "일", rownames(memb_df))
rownames(memb_df) <- gsub("2", "이", rownames(memb_df))
rownames(memb_df) <- gsub("3", "삼", rownames(memb_df))

memb_df <- cbind(rownames(memb_df), memb_df)
colnames(memb_df) <- c('dong', 'membership')

memb_df <- left_join(memb_df, emd_code, by="dong")
memb_df<-rename(memb_df,"id" = "code")
memb_df
memb_df$id <- memb_df$id/100

cluster_map <- left_join(jeju_map, memb_df, by = "id")
cluster_map$breaks <- cut(cluster_map$membership, 8)
ggmap(jeju) +
  geom_polygon(data = cluster_map, aes(x = long, y = lat, group = group, fill = breaks), show.legend = FALSE) + scale_colour_brewer(type = 'div', palette = 'PRGn', direction = -1) 

