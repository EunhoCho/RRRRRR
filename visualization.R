# Visualize map

library(ggplot2)
require(maps)
library(ggmap)
library(googleway)
library(ggrepel)
library(dplyr)
library(tidyr)

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
  locations_csv$address[i] <- address[[i]][3]
}

ggmap(jeju) + geom_point(data = locations_csv, aes(x = loc_x, y = loc_y, col = address))

library(raster)
library(rgeos)
library(maptools)

map_shape <- shapefile("./map_data/EMD_202005/EMD.shp")
map<- spTransform(map_shape, CRS("+proj=longlat"))
map <- fortify(map, region = "EMD_CD")
str(map)

emd_code <- read.csv("./map_data/제주도법정코드.csv")

count<-locations_csv %>% group_by(address)%>% tally(sort=TRUE)
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

dong_loc <- emd_code$dong %>% geocode()
dong_name <- cbind(emd_code, dong_loc)
str(dong_name)


ggmap(jeju) +
  geom_polygon(data = Map_merged, aes(x = long, y = lat, group = group, fill = n),color = "white")  +
  scale_fill_gradient(low = "#FBCF61",
                      high = "#00CC99",
                      space = "Lab",
                      guide = "colourbar")

