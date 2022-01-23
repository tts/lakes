library(tidyverse)
library(sf)
library(ggrepel)

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "avoindata:Seutukartta_maankaytto_jarvet"
wfs_request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json")
res <- st_read(wfs_request, quiet = TRUE, stringsAsFactors = FALSE)
res_4326 <- st_transform(res, crs = 4326)

lakes_big_enough <- res_4326 %>% 
  mutate(area = as.numeric(st_area(.))) %>% # m2
  filter(area >= 10000) # 100 x 100 m

get_names <- function(df) {
  res <- as.data.frame(df["vesisto_nimi_s"]) %>% 
    mutate(
      lon = map_dbl(geometry,  ~st_centroid(.x)[[1]]),
      lat = map_dbl(geometry,  ~st_centroid(.x)[[2]])
    )
}

here <- data.frame(latitude = 60.186497, longitude = 25.006670)
here_sf <- st_as_sf(here, coords = c("longitude", "latitude"), crs = 4326)

lakes_big_enough_dist <- lakes_big_enough %>% 
  rowwise() %>% 
  mutate(d_from_here = st_distance(here_sf, geometry)) %>% 
  arrange(d_from_here)

names_closest <- get_names(lakes_big_enough_dist[1:5,])

# https://ropengov.github.io/geofi/articles/geofi_making_maps.html
polygon <- geofi::get_municipalities(year = 2021, scale = 4500)
point <- geofi::municipality_central_localities
point$municipality_code <- as.integer(point$kuntatunnus)

polygon_helsinki_area <- polygon %>% 
  filter(maakunta_name_fi %in% "Uusimaa" & 
           !nimi %in% c("Karkkila", "Siuntio", "Raasepori", "Hanko", "Inkoo", "Lohja", "Loviisa", 
                        "Porvoo", "Myrskylä", "Lapinjärvi", "Pukkila", "Askola"))

point_helsinki_area <- point %>% 
  filter(municipality_code %in% polygon_helsinki_area$municipality_code)

ggplot(data = lakes_big_enough) +
  geom_sf(fill = "#3498db") +
  geom_sf(data = polygon_helsinki_area, alpha = .3) + 
  geom_sf_text(data = point_helsinki_area, aes(label = stringr::str_to_title(teksti)), size = 2) +
  geom_label_repel(data = names_closest, 
                   aes(x = lon, y = lat, label = vesisto_nimi_s),
                   box.padding = unit(0.8, "lines"),
                   segment.color = "orange",
                   max.overlaps = Inf,
                   size = 3,
                   nudge_x = 0.02,
                   nudge_y = 0.02,
                   xlim = c(-Inf, Inf), ylim = c(-Inf, Inf)) +
  geom_point(data = names_closest[names_closest$vesisto_nimi_s != "",], 
             x = names_closest$lon,
             y = names_closest$lat, 
             color = "orange") +
  theme_minimal() +
  coord_sf(clip = 'off') +
  theme(plot.title = element_text(size = 18),
        legend.position = "none",
        plot.caption = element_text(vjust = 4, size = 8),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "transparent")) +
    labs(x = NULL, y = NULL, 
         title = "Closests lakes big enough for cross-country skiing (100x100 m)",
         caption = "Data: Helsinki region map https://hri.fi/data/dataset/seutukartta ; Statistics Finland via geofi | @ttso")


ggsave(
  "skiinglakes.png",
  width = 35, 
  height = 25, 
  dpi = 320, 
  units = "cm", 
  device = 'png'
)

