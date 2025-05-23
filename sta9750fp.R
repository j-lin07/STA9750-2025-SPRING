# load necessary libraries
library(tidyverse)    # For data manipulation and plotting
library(sf)           # For spatial data manipulation
library(lubridate)    # For handling dates/times
library(ggplot2)      # For plotting
library(viridis)      # For a nice color scale in maps

# load the 311 Noise Complaints CSV file
noise_data <- read_csv("311_Noise_Complaints.csv")

# check the structure of the 311 data
glimpse(noise_data)

# filter data for a recent time period (e.g., 2022-2025)
# make sure the created date column is properly parsed. Replace 'Created_Date' with the actual column name.
noise_data <- noise_data %>%
  mutate(Created_Date = as.Date(`Created Date`, format = "%m/%d/%Y")) %>%  # adjust format if needed
  filter(Created_Date >= as.Date("2022-01-01"))

# inspect missing values in key columns (e.g., latitude, longitude)
summary(noise_data$Latitude)
summary(noise_data$Longitude)

# remove rows without valid location data
noise_data <- noise_data %>% 
  filter(!is.na(Latitude) & !is.na(Longitude))

# convert noise_data to an sf object (points) using latitude/longitude coordinates 
noise_sf <- st_as_sf(noise_data, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# load the NYC Neighborhood Tabulation Areas (NTAs) shapefile
# modify the path to point to unzipped shapefile folder.
nta <- st_read("NYC_NTAs/nynta2020.shp")

# inspect the spatial dataset
print(nta)
plot(st_geometry(nta), main = "NYC Neighborhood Tabulation Areas")\



# data integration and spatial join

# ensure both spatial datasets share the same CRS, if not, transform the noise data
noise_sf <- st_transform(noise_sf, st_crs(nta))

# perform a spatial join: find which noise complaint point falls inside each NTA polygon
noise_with_nta <- st_join(noise_sf, nta, join = st_intersects)

# check results: the joined data now should include attributes from the NTA shapefile (e.g., NTA name, borough, etc.)
glimpse(noise_with_nta)

# drop geometry after the join to aggregate using non-spatial functions
noise_df <- noise_with_nta %>% 
  st_set_geometry(NULL)

# aggregation: count number of complaints per NTA
nta_summary <- noise_df %>% 
  group_by(NTAName) %>%    # Adjust NTA_Name to the correct field from the shapefile
  summarise(total_complaints = n())

# merge summary above back with the spatial NTA data for mapping
nta_map <- nta %>%
  left_join(nta_summary, by = c("NTAName" = "NTAName"))





# data visualization
ggplot() +
  geom_sf(data = nta_map, aes(fill = total_complaints), color = "white", size = 0.2) +
  scale_fill_viridis(option = "magma", na.value = "grey90", 
                     name = "Noise Complaints") +
  labs(title = "Density of 311 Noise Complaints by Neighborhood",
       subtitle = "Data from 2022 to 2025") +
  theme_minimal()

# enhanced choropleth map with improved labels and theme
ggplot() +
  geom_sf(data = nta_map, aes(fill = total_complaints), color = "white", size = 0.1) +
  scale_fill_viridis(option = "magma", na.value = "grey80",
                     name = "Total Noise Complaints",
                     trans = "sqrt") +  # Apply transformation if data are skewed
  labs(title = "311 Noise Complaints Density",
       subtitle = "NYC Neighborhoods (2022-2025)",
       caption = "Data sources: NYC Open Data & NYC Planning") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )








# normalize neighborhood & population

library(tidyverse)    # data wrangling + ggplot2
library(sf)           # spatial data
library(lubridate)    # date parsing
library(viridis)      # color scales
library(tidycensus)   # pull ACS
library(tigris)       # tract geometries

# enable caching for tigris shapefiles
options(tigris_use_cache = TRUE)

# only need to do this once; replace with own key:
census_api_key("22a5ec760da61309b7cc897d007f1f24004b768e", install = TRUE)
readRenviron("~/.Renviron")

# pull ACS population at the TRACT level 
# we'll get total population (variable B01003_001) for all NYC tracts
pop_tracts <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  year      = 2021,
  state     = "NY",
  county    = c("Bronx","Kings","New York","Queens","Richmond"),
  geometry  = TRUE,
  cache_table = TRUE
) %>%
  st_transform(crs = st_crs(nta_map))

# assign each tract to an NTA via centroid → spatial join 
tract_centroids <- st_centroid(pop_tracts)

tracts_with_nta <- st_join(
  tract_centroids,
  nta_map %>% select(NTAName),
  join = st_intersects
)

nta_pop <- tracts_with_nta %>%
  st_set_geometry(NULL) %>%
  group_by(NTAName) %>%
  summarise(
    population = sum(estimate, na.rm = TRUE)
  )

# compute NTA area (sq miles) & merge population 
nta_map <- nta_map %>%
  mutate(
    area_m2 = st_area(geometry),                        # in square meters
    area_sq_mi = as.numeric(area_m2) * 0.000000386102159
  ) %>%
  left_join(nta_pop, by = "NTAName")

# normalize complaints by population & area 
nta_map <- nta_map %>%
  mutate(
    complaints_per_1000  = total_complaints / (population / 1000),
    complaints_per_sq_mi = total_complaints / area_sq_mi
  )

# visualize normalized metrics

# complaints per 1,000 residents
ggplot(nta_map) +
  geom_sf(aes(fill = complaints_per_1000), color = "white", size = 0.1) +
  scale_fill_viridis(
    option   = "plasma",
    name     = "Complaints\nper 1,000",
    na.value = "grey90"
  ) +
  labs(
    title    = "311 Noise Complaints (per 1,000 residents)",
    subtitle = "NYC NTAs, 2022–2025"
  ) +
  theme_minimal()

# complaints per square mile
ggplot(nta_map) +
  geom_sf(aes(fill = complaints_per_sq_mi), color = "white", size = 0.1) +
  scale_fill_viridis(
    option   = "magma",
    name     = "Complaints\nper sq mile",
    na.value = "grey90"
  ) +
  labs(
    title    = "311 Noise Complaints (per sq mile)",
    subtitle = "NYC NTAs, 2022–2025"
  ) +
  theme_minimal()



