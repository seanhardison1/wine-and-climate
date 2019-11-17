library(sf)
library(riem)
library(tidyverse)
library(lubridate)

#Counties in CA wine growing region
central_coast <- c("Ventura","Santa Barbara", "San Luis Obispo", "Monterey",
                   "San Benito", "Santa Clara", "Santa Cruz", "San Mateo",
                   "Alameda","Contra Costa", "San Francisco")

north_coast <- c("Marin", "Napa", "Sonoma", "Lake", "Mendocino")

n_sj_valley <- c("Yolo", "Solano", "Sacramento", "San Joaquin", 
                 "Stanislaus","Merced")

s_sj_valley <- c("Alpine","Mono","Inyo","Tulare","Kern","Kings","Fresno","Madera")

#Label counties inside the wine growing region of CA
counties <- st_read(here::here("data/CA_Counties/CA_Counties_TIGER2016.shp")) %>% 
  st_transform(crs = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") %>% 
  mutate(`Wine areas` = ifelse(NAME %in% central_coast, "Central coast", 
                                ifelse(NAME %in% north_coast, "North coast",
                                ifelse(NAME %in% n_sj_valley, "N. San Joaquin Valley",
                        ifelse(NAME %in% s_sj_valley, "S. San Joaquin Valley", NA)))))
  
#intersect ASOS stations with wine counties
ca_stations <- riem::riem_stations("CA_ASOS") %>% 
  st_as_sf(.,coords = c("lon","lat"),crs = raster::crs(counties)) %>% 
  st_transform(crs = raster::crs(counties)) %>% 
  st_intersection(.,counties %>% filter(!is.na(`Wine areas`)))

central_coast_stations <- ca_stations %>% 
  filter(Wine.areas == "Central coast")

# save(central_coast_stations, file = here::here("data/central_coast_stations.rdata"))
#generate map of wine counties and ASOS stations
ggplot() +
  geom_sf(data = counties, alpha = 0.01) +
  geom_sf(data = counties %>% filter(`Wine areas` != "NA"),
          aes(fill = `Wine areas`)) +
  geom_sf(data = ca_stations) +
  coord_sf(xlim = c(-124,-114), ylim = c(32,42)) +
  ecodata::theme_map() +
  ggtitle("California Wine Regions and Weather Stations")

#Get station location data
# save(ca_stations, file = here::here('data/wine_country_riem_stations.rdata'))
# load(file = here::here('data/wine_country_riem_stations.rdata'))
load(file = here::here("data/central_coast_stations.rdata"))
#download time series from each station. Write to csv
out <- NULL
for (i in 1:length(unique(central_coast_stations$id))){
  df <- riem::riem_measures(station = central_coast_stations$id[i],
                      date_start = "1989-01-01")
  message(head(df))
  
  df_int <- 
    df %>% group_by(station, lon, lat, year(valid), month(valid),
                    day(valid)) %>% 
    dplyr::summarise_at(vars(tmpf:mslp), mean, na.rm = TRUE)
  message(paste0(central_coast_stations$id[i], " downloaded"))
  
  write.csv(df_int, file = here::here("data/central_coast_stations",paste0(central_coast_stations$id[i],
                                                    "_ASOS.csv")),
            row.names = F)
  assign('out', rbind(out, df_int))
}

