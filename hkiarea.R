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

distance_to_lake <- st_distance(home_sf, lakes_big_enough)
most_near <- lakes_big_enough[which.min(st_distance(home_sf, lakes_big_enough[1:nrow(lakes_big_enough),])),]

ggplot(data = lakes_big_enough) +
  geom_sf(fill = "#3498db") +
  geom_sf(data = home_sf, color = "orange") +
  geom_sf(data = most_near, color = "red", size = 2) +
  theme_minimal() +
  theme(plot.title = element_text(size = 22),
        legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "transparent")) +
  labs(x = NULL, y = NULL, 
       title = "Lakes in the Helsinki region, and the nearest one to where I live",
       caption = "Data: Helsinki region map\nhttps://hri.fi/data/dataset/seutukartta")



ggsave(
  "hkilakes.png",
  width = 35, 
  height = 25, 
  dpi = 72, 
  units = "cm", 
  device = 'png'
)

