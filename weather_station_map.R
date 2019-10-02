library(sf)
library(riem)
library(tidyverse)

central_coast <- c("Ventura","Santa Barbara", "San Luis Obispo", "Monterey",
                   "San Benito", "Santa Clara", "Santa Cruz", "San Mateo",
                   "Alameda","Contra Costa", "San Francisco")

north_coast <- c("Marin", "Napa", "Sonoma", "Lake", "Mendocino")

n_sj_valley <- c("Yolo", "Solano", "Sacramento", "San Joaquin", 
                 "Stanislaus","Merced")
s_sj_valley <- c("Alpine","Mono","Inyo","Tulare","Kern","Kings","Fresno","Madera")

counties <- st_read(here::here("data/CA_Counties/CA_Counties_TIGER2016.shp")) %>% 
  st_transform(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>% 
  mutate(`Wine areas` = if_else(NAME %in% central_coast, "Central coast", 
                                if_else(NAME %in% north_coast, "North coast",
                                if_else(NAME %in% n_sj_valley, "N. San Joaquin Valley",
                        if_else(NAME %in% s_sj_valley, "S. San Joaquin Valley", "NA")))))
  


ca_stations <- riem::riem_stations("CA_ASOS") %>% 
  st_as_sf(.,coords = c("lon","lat"),crs = raster::crs(counties)) %>% 
  st_transform(crs = raster::crs(counties)) %>% 
  st_intersection(.,counties %>% filter(`Wine areas` != "NA"))

ggplot() +
  geom_sf(data = counties, alpha = 0.01) +
  geom_sf(data = counties %>% filter(`Wine areas` != "NA"),
          aes(fill = `Wine areas`)) +
  geom_sf(data = ca_stations) +
  coord_sf(xlim = c(-124,-114), ylim = c(32,42)) +
  ecodata::theme_map() +
  ggtitle("California Wine Regions and Weather Stations")

#Get stations 
ca_stations

