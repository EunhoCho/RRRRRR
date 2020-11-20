# Visualize map

library(ggplot2)
require(maps)
library(ggmap)
library(googleway)
library(ggrepel)

theme_set(theme_void())

key = ""

register_google(key = "")
geo <- geocode("hallasan")
center_loc = c(geo$lon, geo$lat)
map <- get_googlemap(center = center_loc, zoom = 10, size = c(1000, 1000), maptype = "roadmap", color = "bw")

#dummy data
places_cv <- google_places(search_string = "restaurant", location = c(geo$lat, geo$lon), key = key)
df_loc <- places_cv$results["geometry"][,1][,1]
name <- c(1:20)
num_hash <- runif(20, 0, 100)
category <- factor(sample(1:3, 20, replace = TRUE))
df_cv <- cbind(df_name, df_loc, num_hash, category)
str(df_cv)

ggmap(map) + geom_point(data = df_cv, aes(x = lng, y = lat, size = num_hash, col=category)) + geom_text_repel(data = df_cv, aes(x = lng, y = lat,label = name))
                                                                              
                                                                              