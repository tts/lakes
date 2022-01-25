library(sf)
library(tidyverse)

# Lakes in the bigger Helsinki area, and municipality borders
#
# Source: Helsinki region map. The maintainer of the dataset is Helsingin kaupunkiympäristön toimiala / 
# Kaupunkimittauspalvelut and the original author is Helsingin kaupunkiympäristön toimiala / Kaupunkimittauspalvelut 
# yhdessä HSY:n ja alueen muiden kuntien mittausorganisaatioiden kanssa. The dataset has been downloaded from 
# Helsinki Region Infoshare service on 23.01.2022 under the license Creative Commons Attribution 4.0.

baseurl <- "https://kartta.hel.fi/ws/geoserver/avoindata/wfs?request=GetFeature&service=WFS&version=2.0.0"
type <- "avoindata:Seutukartta_maankaytto_jarvet"
wfs_request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json")
res <- st_read(wfs_request, quiet = TRUE, stringsAsFactors = FALSE)
res_4326 <- st_transform(res, crs = 4326)

lakes <- res_4326 %>% 
  mutate(area = as.numeric(st_area(.)),
         size = cut(area, breaks = c(1, 10000, 100000, 1000000, 100000000), 
                    labels = c("1-10.000", "10.000-100.000", "100.000-1.000.000", "1.000.000-"),
                    include.lowest = TRUE))

lakes1 <- lakes %>% 
  filter(size == "1-10.000",
         str_squish(vesisto_nimi_s) != "Lippajärvi", 
         str_squish(vesisto_nimi_s) != "Pitkäjärvi")
lakes2 <- lakes %>% 
  filter(size == "10.000-100.000")
lakes3 <- lakes %>% 
  filter(size == "100.000-1.000.000")
lakes4 <- lakes %>% 
  filter(size == "1.000.000-")

write_rds(lakes1, "lakes1.RDS")
write_rds(lakes2, "lakes2.RDS")
write_rds(lakes3, "lakes3.RDS")
write_rds(lakes4, "lakes4.RDS")

type <- "Seutukartta_aluejako_kuntarajat"
wfs_request <- paste0(baseurl, "&typeName=", type, "&outputFormat=json")
res_area <- st_read(wfs_request, quiet = TRUE, stringsAsFactors = FALSE)
res_area_4326 <- st_transform(res_area, crs = 4326)

area <- res_area_4326 %>%
  st_combine() %>% 
  st_cast(., 'MULTILINESTRING')

write_rds(area, "area.RDS")