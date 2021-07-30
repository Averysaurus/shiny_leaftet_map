library(tidyverse)
library(RSocrata)
library(sp)
library(rgdal)
library(sf)
library(tmap)
library(leaflet)
library(leaflet.extras)


# Link to SF neighborhoods geojson
# https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=Shapefile

# Load geojson with both packages. Not the most elegant approach, but it works! 

sfnhoods <- readOGR("SF Find Neighborhoods.geojson") 
sfnhoods$link <- NULL

sfnhoods_sf <- st_read("SF Find Neighborhoods.geojson")
sfnhoods_sf$link <- NULL

# Imoort dataset from api. 
ev_df <- read.socrata("https://data.sfgov.org/resource/5cei-gny5.json")

# clean dataset. 
names(ev_df) %>% 
  stringr::str_replace_all("\\s","_") %>% tolower

map_data <- ev_df %>% 
  # select columns of interest.
  dplyr::select("file_date",
                "non_payment",
                "breach",
                "nuisance",
                "illegal_use",
                "failure_to_sign_renewal",
                "access_denial",
                "late_payments",
                "client_location.longitude",
                "client_location.latitude") %>%                             
  mutate(file_date = str_extract(file_date, "....")) %>% 
  mutate(lat = as.numeric(client_location.latitude)) %>% 
  mutate(lng = as.numeric(client_location.longitude)) %>% 
  # filter date post CA-Shelter in Place dates.
  filter(file_date >= "2000" & 
           file_date < "2021") %>% 
  # purge NA in location vector.
  filter(client_location.longitude != "NA" &
           client_location.latitude != "NA") %>% 
  dplyr::select(file_date, 
                lat, 
                lng) 



# Use sp to compute events "over" polygons/
ev_spdf <- sp::SpatialPointsDataFrame(coords = map_data[,c("lng", "lat")], data = map_data["file_date"], proj4string = CRS("+init=epsg:4326"))

ev_spdf <- spTransform(ev_spdf, crs(sfnhoods))

point_in_hood <- sp::over(ev_spdf, sfnhoods)

map_data$name <- point_in_hood$name


# Summarize data. 
map_data_summary <- map_data %>% 
  group_by(name, file_date) %>% 
  summarise(file_date, n = n()) %>% 
  distinct(file_date, .keep_all = TRUE)

# Join 
joined_summary <- 
  left_join(map_data_summary, sfnhoods_sf, by = "name") %>% 
  ungroup() %>% 
  st_as_sf() %>% 
  filter(file_date =='2000') %>% 
  na.omit()

# test map. 
colorPal <- colorNumeric("viridis", 
                         joined_summary$n)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data=joined_summary, 
              col=colorPal(joined_summary$n),
              fillOpacity=0.6, weight = 0.75, 
              popup = joined_summary$name, 
              highlight = highlightOptions(
                bringToFront = TRUE)) %>%
  addLegend(pal = colorPal, 
            values = joined_summary$n,
            position = "bottomleft",
            title = "Evictions") %>% 
  addResetMapButton() 

# Write data. 
st_write(joined_summary, "sf_ev_map.geojson")

# test data import. 
json_test = st_read("sf_ev_map.geojson")
