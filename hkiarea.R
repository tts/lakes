library(tidyverse)
library(sf)
library(ggrepel)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "avoindata:Seutukartta_maankaytto_jarvet"
wfs_request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json")
res <- st_read(wfs_request, quiet = TRUE, stringsAsFactors = FALSE)
res_4326 <- st_transform(res, crs = 4326)

# Lake area
lakes_big_enough <- res_4326 %>% 
  mutate(area = as.numeric(st_area(.))) %>% # m2
  filter(area >= 10000) # 100 x 100 m

home <- data.frame(latitude = 60.188081, longitude = 24.998965)
home_sf <- st_as_sf(home, coords = c("longitude", "latitude"), 
                  crs = 4326)

names <- as.data.frame(lakes_big_enough[3]) %>% 
  mutate(
    lon = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry,  ~st_centroid(.x)[[2]])
  ) 

names_sample <- names %>% 
  sample_n(50)

distance_to_lake <- st_distance(home_sf, lakes_big_enough)
distance_to_lake_sorted <- sort(distance_to_lake)

most_near <- lakes_big_enough[which.min(st_distance(home_sf, lakes_big_enough[1:nrow(lakes_big_enough),])),]

# TO DO Second most near?         

name_most_near <- as.data.frame(most_near[3]) %>% 
  mutate(
    lon = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
    lat = map_dbl(geometry,  ~st_centroid(.x)[[2]])
  ) 

names_sample_and_nearest <- rbind(names_sample, name_most_near)

ggplot(data = lakes_big_enough) +
  geom_sf(fill = "#3498db") +
  geom_label_repel(data = names_sample_and_nearest, 
                   aes(x = lon, y = lat, label = vesisto_nimi_s),
                   box.padding = unit(0.8, "lines"),
                   max.overlaps = Inf,
                   size = 2,
                   nudge_x = 0.02,
                   nudge_y = 0.02,
                   xlim = c(-Inf, Inf), ylim = c(-Inf, Inf)) +
  geom_point(data = names_sample[names_sample$vesisto_nimi_s != "",], 
             x = names_sample$lon,
             y = names_sample$lat, 
             color = "blue") +
  # geom_sf(data = home_sf, color = "orange") +
  geom_sf(data = most_near, color = "green", size = 2) +
  theme_minimal() +
  coord_sf(clip = 'off') +
  theme(plot.title = element_text(size = 12),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "transparent")) +
    labs(x = NULL, y = NULL, 
         title = "Lakes around here big enough for skiing (100 x 100 m)",
         caption = "Data: Helsinki region map\nhttps://hri.fi/data/dataset/seutukartta")


ggsave(
  "skiinglakes.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)

