library(dplyr)
library(ggplot2)
library(sf)

library(readxl)
library(tigris)
library(terra)    # if your USDA data is raster (.tif)

getwd()
setwd("//home//schnablelab//Documents//N trials//data files//")
dat <- read_excel("Ndata2.0.xlsx",
                  sheet = "RD",
                  skip = 2,
                  na = ".") %>%
  rename(year  = "StudyYear"
  ) %>%
  mutate(year = as.factor(year)) %>% select(n_rate, Rep, yield, StudyID_Yr, year,lon, lat) %>% mutate(lon = as.numeric(lon), lat = as.numeric(lat))

head(dat)
# --- Step 2: Filter ---
study_counts <- dat %>%
  group_by(StudyID_Yr) %>%
  summarise(n_Nrates = n_distinct(n_rate), .groups = "drop") %>%
  filter(n_Nrates >= 3)

#cleaning data
dat_clean <- dat %>%
  semi_join(study_counts, by = "StudyID_Yr") %>%
  filter(!(StudyID_Yr %in% c(216, 1713, 128))) %>%
  filter(yield > 0) %>% mutate(lon = as.numeric(lon), lat = as.numeric(lat))

# --- Step 3: Summarize mean yield ---
nit_summary <- dat_clean %>%
  group_by(year, StudyID_Yr, n_rate, lon, lat) %>%
  summarise(mean_yield = mean(yield, na.rm = TRUE), .groups = "drop")

# Extrdat_clean# Extract unique study locations (must have longitude & latitude columns)
studies_sf <- dat_clean %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  distinct(StudyID_Yr, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


## Convert data to a spatial object
dat_sf <- st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)

#county data
county<-read.csv("county.csv", na=".")
county<-county%>%slice(-71) 
tail(county)
head(county)
nrow(county)
names(county)
county <- county %>% select(State,County, Year, corn_acres_planted) %>%
  mutate(
    corn_acres_planted = as.numeric(gsub(",", "", corn_acres_planted))
  ) %>%
  filter(State == "NEBRASKA", County != "OTHER COUNTIES") 

#load Ne counties on map

ne_counties <- counties(state = "NE", cb = TRUE, class = "sf")
#set tigris options
options(tigris_use_cache = TRUE)

#we have ne_counties previously 
library(stringr)

#calculating land area (in square miles)
ne_counties <- ne_counties %>% mutate(area_sqmi = as.numeric(st_area(geometry))/2.59e6, NAME = str_to_upper(NAME))

#combine USDA data with county shapes
ne_map <- ne_counties %>%
  left_join(county, by = c("NAME" = "County"))

#compute % of county area planted 
ne_map <- ne_map %>%
  mutate(
    corn_area_sqmi = corn_acres_planted / 640,         # convert acres → square miles
    pct_corn = (corn_area_sqmi / area_sqmi) * 100      # % of county planted in corn
  )

#map for overlay and pct_corn
pct_corn <- ggplot(ne_map) + geom_sf(aes(fill= pct_corn)) + scale_fill_viridis_c(option = "plasma", direction= -1, na.value= "grey90", name = "% corn Area") + geom_sf(data= studies_sf, color= "green", size= 2, alpha = 0.8)  + 
  labs(title = "Percent of Corn Planted per County (NE)",
       caption = "Data : USDA QuickStats &  TIGER Counties") +
  theme_minimal(base_size=12) +
  theme(axis.title = element_text(color = 'black', face = 'bold'),
        axis.text = element_text(color = 'black'),
        axis.line = element_line(color = 'black'))
pct_corn

#plot the map, absolute values
ggplot() +
  geom_sf(data = ne_map, aes(fill = corn_acres_planted), color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "grey90") +
  labs(
    title = "Nebraska Corn Planted Acres (USDA NASS)",
    fill = "Acres planted"
  ) +
  theme_minimal()

ggsave(
  "~/Documents/N trials/pct_corn.png", plot = pct_corn,
  width = 12,     # inches
  height = 8,
  dpi = 400,      # high-resolution
  units = "in", bg= "white"
)

#plotting with corn grain acres harvested #repeat everything till line 37

#county data
setwd("//home//schnablelab//Downloads//data sheets for N data//")
county_acre_harvested <-read.csv("corn acres harvested.csv", na=".")
 
tail(county)
head(county)
nrow(county)
names(county_acre_harvested)
county_harvested <- county_acre_harvested %>% select(State,County,Year, Value)
library(dplyr)

county_harvested <- county_harvested %>%
  rename(acres_harv = Value)   # Value = corn acres harvested

names(county_harvested)

#load Ne counties on map
ne_counties_harv <- counties(state = "NE", cb = TRUE, class = "sf")

#combine USDA data with county shapes
ne_map_harv <- ne_counties_harv %>%
  left_join(county_harvested, by = c("NAME" = "County"))

names(ne_map_harv)
ne_map_harv
#set tigris options
options(tigris_use_cache = TRUE)

#we have ne_counties previously 
#standardize data for counties
library(stringr)

ne_counties_harv <- ne_counties_harv %>%
  mutate(NAME = str_to_upper(NAME))

names(ne_counties_harv)

county_harvested <- county_harvested %>%
  mutate(County = str_to_upper(County))


#combine USDA data with county shapes
ne_map_harv <- ne_counties_harv %>% select(GEOID,NAME, area_sqmi, geometry) %>%
  left_join(county_harvested, by = c("NAME" = "County"))
names(ne_map_harv)

library(dplyr)
library(stringr)

# Clean numeric column BEFORE plotting
ne_map_harv <- ne_map_harv %>%
  mutate(
    # treat " ." as missing
    acres_harv = na_if(acres_harv, " ."),
    # remove commas
    acres_harv = gsub(",", "", acres_harv),
    # convert to numeric
    acres_harv = as.numeric(acres_harv)
  )

#overlay with N data 
ggplot() +
  geom_sf(data = ne_map_harv, aes(fill = acres_harv), color = "white") +
  scale_fill_viridis_c(option = "plasma", direction= -1, na.value = "grey90", guide= guide_colorbar()) +
  geom_sf(data = studies_sf, color = "blue", size = 2, alpha = 0.8) +
  labs(
    title = "Nebraska Corn Acres Harvested and Experiment Locations",
    caption= "Source: USDA NASS QuickStats (2022) & UNL Field DATA", 
    fill = "Corn acres Harvested"
  ) +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.caption = element_text( size=9))
ggsave(
  "~/Documents/N trials/corn maps/harvest_acre.png",
  width = 12,     # inches
  height = 8,
  dpi = 400,      # high-resolution
  units = "in", bg= "white"
)

#compute % of county acres harvested 
#calculating land area (in square miles)
ne_counties_harv <- ne_counties_harv %>% mutate(area_sqmi = as.numeric(st_area(geometry))/2.59e6, NAME = str_to_upper(NAME))

str(ne_map_harv$acres_harv)
head(ne_map_harv$acres_harv)

ne_map_harv <- ne_map_harv %>%
  mutate(
    corn_area_sqmi = acres_harv / 640,         # convert acres → square miles
    pct_corn = (corn_area_sqmi / area_sqmi) * 100      # % of county harvested in corn
  )

names(ne_map_harv)

#map for overlay and pct_corn
pct_corn_harv <- ggplot(ne_map_harv) + geom_sf(aes(fill= pct_corn)) + scale_fill_viridis_c(option = "plasma", direction= -1, na.value= "grey90", name = "% corn Area") + geom_sf(data= studies_sf, color= "green", size= 2, alpha = 0.8)  + 
  labs(title = "Percent of Corn Harvesred per County (NE)",
       caption = "Data : USDA QuickStats & TIGER Counties") +
  theme_minimal(base_size=12) +
  theme(axis.title = element_text(color = 'black', face = 'bold'),
        axis.text = element_text(color = 'black'),
        axis.line = element_line(color = 'black'))
pct_corn_harv

ggsave(
  "~/Documents/N trials/corn maps/pct_corn_harv.png", plot = pct_corn_harv,
  width = 12,     # inches
  height = 8,
  dpi = 400,      # high-resolution
  units = "in", bg= "white"
)


#plot the map, absolute values
ggplot() +
  geom_sf(data = ne_map, aes(fill = corn_acres_planted), color = "white") +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "grey90") +
  labs(
    title = "Nebraska Corn Planted Acres (USDA NASS)",
    fill = "Acres planted"
  ) +
  theme_minimal()

#check data 
unique(ne_counties$NAME) |> head(10)
unique(county$County)   |> head(10)

#standardize data for counties
library(stringr)

ne_counties <- ne_counties %>%
  mutate(NAME = str_to_upper(NAME))

county <- county %>%
  mutate(
    County = str_to_upper(County),
    County = str_remove_all(County, "COUNTY"),
    County = str_trim(County)
  )

# how many matched counties
sum(!is.na(ne_map$corn_acres_planted))   

#how many did not match
ne_map %>%
  filter(is.na(corn_acres_planted)) %>%
  select(NAME)


#overlay with N data 
ggplot() +
  geom_sf(data = ne_map, aes(fill = corn_acres_planted), color = "white") +
  scale_fill_viridis_c(option = "plasma", direction= -1, na.value = "grey90", guide= guide_colorbar()) +
  geom_sf(data = studies_sf, color = "blue", size = 2, alpha = 0.8) +
  labs(
    title = "Nebraska Corn Planted Acres (USDA) and Experiment Locations",
    caption= "Source: USDA NASS QuickStats & UNL Field DATA\n2079800 corn acres planted (not specified)", 
    fill = "Corn acres planted"
  ) +
  theme_minimal() + theme(panel.grid = element_blank(),
                          plot.caption = element_text( size=9))

ggsave(
  "~/Documents/N trials/pct_corn.png", plot = pct_corn,
  width = 12,     # inches
  height = 8,
  dpi = 400,      # high-resolution
  units = "in", bg= "white"
)

#load USDA layer 
usda_sf <- st_read("USDA_layer.shp")

#using land area to plot percent of corn produced in NE
library(tidyverse)
install.packages("AOI")
library(AOI)
install.packages("climateR")
library(climateR)
library(zonal)
library(sf)
library(patchwork)
# library(MoMAColors)
library(viridis)
library(scales)
library(grid)
library(gridExtra)
library(cowplot)
source('src/Functions.R')



# get sf geom of NE counties; this gives a column of land_area and water_area, but we don't know the units
aoi <- aoi_get(state = c('NE'), county = 'all') %>%
  mutate(acres = land_area*0.000247105)
# see what coordinate system it's in and what length unit it uses-- this is what the units of area are defined by (but we can also see it in docs of aoi_get())
st_crs(aoi)
# LENGTHUNIT = 'metre', so land_area and water_area are in sq meters
