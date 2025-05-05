# 1. Set up the directory
data_dir <- "data/mp04"
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
}

# 2. Define URLs and file paths
# Finest resolution (1:500k) county boundaries for 2023
zip_url   <- "https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip"
zip_file  <- file.path(data_dir, basename(zip_url))
shp_file  <- file.path(data_dir, "cb_2023_us_county_500k.shp")

# 3. Download the ZIP if not already present
if (!file.exists(zip_file)) {
  message("Downloading county shapefile (500k resolution)...")
  download.file(url = zip_url,
                destfile = zip_file,
                mode = "wb")
}

# 4. Unzip the shapefile components if the .shp isn't already there
if (!file.exists(shp_file)) {
  message("Extracting shapefile components...")
  unzip(zip_file, exdir = data_dir)
}

message("County shapefile is ready in ", data_dir)

#Task 2 Acquire 2024 US Presidential Election Results

# Install & load all needed packages

needed <- c(
  "httr2", "rvest", "xml2",
  "dplyr", "stringr", "readr",
  "purrr", "janitor", "tibble"
)
for(pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# 1. URL‑ready state names

states <- c(
  "Alabama","Alaska","Arizona","Arkansas","California","Colorado",
  "Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho",
  "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana",
  "Maine","Maryland","Massachusetts","Michigan","Minnesota",
  "Mississippi","Missouri","Montana","Nebraska","Nevada",
  "New_Hampshire","New_Jersey","New_Mexico","New_York",
  "North_Carolina","North_Dakota","Ohio","Oklahoma","Oregon",
  "Pennsylvania","Rhode_Island","South_Carolina","South_Dakota",
  "Tennessee","Texas","Utah","Vermont","Virginia","Washington",
  "West_Virginia","Wisconsin","Wyoming"
)

# 2. Scraper function for one state

get_state_results <- function(state) {
  safe       <- str_replace_all(state, " ", "_")
  url        <- paste0(
    "https://en.wikipedia.org/wiki/2024_United_States_presidential_election_in_",
    safe
  )
  cache_dir  <- "data/mp04/wiki"
  cache_file <- file.path(cache_dir, paste0("2024_", safe, ".html"))
  
  # ensure cache dir
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # download once
  if (!file.exists(cache_file)) {
    message("Downloading ", state)
    resp <- httr2::request(url) |> httr2::req_perform()
    readr::write_file(httr2::resp_body_string(resp), cache_file)
  }
  
  # parse the saved HTML
  page   <- xml2::read_html(cache_file)
  tables <- page %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE)
  
  # drop empty tables
  tables <- purrr::keep(tables, ~ nrow(.) > 0 && ncol(.) > 0)
  
  # identify which table has a County/Parish/Borough/Census area column
  has_county <- purrr::map_lgl(tables, ~
                                 any(stringr::str_detect(names(.),
                                                         regex("County|Parish|Borough|Census area", ignore_case = TRUE)
                                 ))
  )
  if (!any(has_county)) {
    warning("No county‑level table found for ", state)
    return(tibble::tibble(
      state  = str_replace_all(state, "_", " "),
      county = character(),
      .rows  = 0
    ))
  }
  tbl <- tables[[ which(has_county)[1] ]]
  
  # drop a stray header row if its first cell is "#"
  if (as.character(tbl[[1]][1]) == "#") {
    tbl <- tbl[-1, ]
  }
  
  # clean names, rename the county‑equivalent column to 'county'
  tbl <- tbl %>%
    janitor::clean_names()
  
  county_col <- names(tbl)[
    str_detect(names(tbl),
               regex("county|parish|borough|census_area", ignore_case = TRUE)
    )
  ][1]
  
  df <- tbl %>%
    rename(county = all_of(county_col)) %>%       # unify name
    filter(                                       # drop any leftover header labels
      !is.na(county),
      !tolower(county) %in% c("county","parish","borough","census area")
    ) %>%
    mutate(
      across(                                    # parse all vote columns
        -county,
        ~ readr::parse_number(as.character(.))
      ),
      state = str_replace_all(state, "_", " ")   # add state label
    )
  
  return(df)
}

# 3. Fetch & combine all states

all_results_2024 <- purrr::map_dfr(states, get_state_results)


# 4. Inspect

dplyr::glimpse(all_results_2024)

all_results_2024 <- all_results_2024 %>%
  select(-matches("^1$")) %>%  # drop that stray “1” column if it exists
  rename(
    harris_votes = kamala_harris_democratic,
    harris_pct   = kamala_harris_democratic_2,
    trump_votes  = donald_trump_republican,
    trump_pct    = donald_trump_republican_2,
    other_votes  = various_candidates_other_parties,
    other_pct    = various_candidates_other_parties_2,
    margin_votes = margin,
    margin_pct   = margin_2,
    total_votes  = total,       
  )

#Task 3 2020 Acquire 2020 US Presidential Election Results

needed <- c(
  "httr2", "rvest", "xml2",
  "dplyr", "stringr", "readr",
  "purrr", "janitor", "tibble"
)
for(pkg in needed) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}


# 1. Re‑use the same URL‑ready list of states

states <- c(
  "Alabama","Alaska","Arizona","Arkansas","California","Colorado",
  "Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho",
  "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana",
  "Maine","Maryland","Massachusetts","Michigan","Minnesota",
  "Mississippi","Missouri","Montana","Nebraska","Nevada",
  "New_Hampshire","New_Jersey","New_Mexico","New_York",
  "North_Carolina","North_Dakota","Ohio","Oklahoma","Oregon",
  "Pennsylvania","Rhode_Island","South_Carolina","South_Dakota",
  "Tennessee","Texas","Utah","Vermont","Virginia","Washington",
  "West_Virginia","Wisconsin","Wyoming"
)


# 2. Scraper for 2020 results

get_state_results_2020 <- function(state) {
  safe       <- str_replace_all(state, " ", "_")
  url        <- paste0(
    "https://en.wikipedia.org/wiki/2020_United_States_presidential_election_in_",
    safe
  )
  cache_dir  <- "data/mp04/wiki2020"
  cache_file <- file.path(cache_dir, paste0("2020_", safe, ".html"))
  
  # make cache dir
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  
  # download once
  if (!file.exists(cache_file)) {
    message("Downloading 2020 results for ", state)
    resp <- httr2::request(url) |> httr2::req_perform()
    readr::write_file(httr2::resp_body_string(resp), cache_file)
  }
  
  # parse HTML & grab all tables
  page   <- xml2::read_html(cache_file)
  tables <- page %>%
    rvest::html_nodes("table") %>%
    rvest::html_table(fill = TRUE)
  
  # drop empty tables
  tables <- purrr::keep(tables, ~ nrow(.) > 0 && ncol(.) > 0)
  
  # find the county‑equivalent table
  has_county <- purrr::map_lgl(tables, ~
                                 any(str_detect(names(.),
                                                regex("County|Parish|Borough|Census area", ignore_case = TRUE)
                                 ))
  )
  if (!any(has_county)) {
    warning("No county‑level table found for 2020 ", state)
    return(tibble(state = str_replace_all(state, "_", " "),
                  county = character(), .rows = 0))
  }
  tbl <- tables[[ which(has_county)[1] ]]
  
  # drop stray header‐row if its first cell is "#"
  if (as.character(tbl[[1]][1]) == "#") tbl <- tbl[-1, ]
  
  # clean up
  df <- tbl %>%
    janitor::clean_names() %>%
    { 
      # detect & rename the county column
      cn <- names(.)[str_detect(names(.),
                                regex("county|parish|borough|census_area",
                                      ignore_case = TRUE))][1]
      rename(., county = all_of(cn))
    } %>%
    filter(
      !is.na(county),
      !tolower(county) %in% c("county","parish","borough","census area")
    ) %>%
    mutate(
      across(
        -county,
        ~ parse_number(as.character(.))
      ),
      state = str_replace_all(state, "_", " ")
    )
  
  return(df)
}

# 3. Fetch & bind all 2020 results

all_results_2020 <- purrr::map_dfr(states, get_state_results_2020)

# 4. Quick sanity‐check

dplyr::glimpse(all_results_2020)


all_results_2020 <- all_results_2020 %>%
  # drop stray “1” column if present
  select(-matches("^1$")) %>%
  # rename with 2020 candidates
  rename(
    biden_votes  = joe_biden_democratic,
    biden_pct    = joe_biden_democratic_2,
    trump_votes  = donald_trump_republican,
    trump_pct    = donald_trump_republican_2,
    other_votes  = various_candidates_other_parties,
    other_pct    = various_candidates_other_parties_2,
    margin_votes = margin,
    margin_pct   = margin_2,
    total_votes  = total,            
  )

# Map County‑level Trump‑share Swing (2020→2024)
library(sf); library(dplyr); library(ggplot2); library(stringr)

# 1. Re‑join and prepare, as before
state_fips_df <- tibble::tibble(
  STATEFP = sprintf("%02d", c(1,2,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,
                              25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,
                              44,45,46,47,48,49,50,51,53,54,55,56)),
  state   = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
              "Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho",
              "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana",
              "Maine","Maryland","Massachusetts","Michigan","Minnesota",
              "Mississippi","Missouri","Montana","Nebraska","Nevada",
              "New Hampshire","New Jersey","New Mexico","New York",
              "North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
              "Pennsylvania","Rhode Island","South Carolina","South Dakota",
              "Tennessee","Texas","Utah","Vermont","Virginia","Washington",
              "West Virginia","Wisconsin","Wyoming")
)
counties_sf <- sf::st_read("data/mp04/cb_2023_us_county_500k.shp", quiet=TRUE) %>%
  left_join(state_fips_df, by="STATEFP")

res24 <- all_results_2024 %>% mutate(county = str_to_title(county))
res20 <- all_results_2020 %>% mutate(county = str_to_title(county))

shift_df <- res24 %>%
  select(state, county, trump24 = trump_votes, tot24 = total_votes) %>%
  inner_join(
    res20 %>% select(state, county, trump20 = trump_votes, tot20 = total_votes),
    by = c("state","county")
  ) %>%
  mutate(
    pct24 = trump24 / tot24,
    pct20 = trump20 / tot20,
    delta_trump_pct = (pct24 - pct20) * 100
  )

swing_sf <- counties_sf %>%
  left_join(shift_df, by = c("NAME"="county","state"="state"))

# 2. Plot, cropped to the Lower 48
ggplot(swing_sf) +
  geom_sf(fill = "grey95", color = "white", size = 0.05) +
  geom_sf(aes(fill = delta_trump_pct), color = NA) +
  coord_sf(
    datum = NA,
    xlim = c(-125, -66),        # continental US longitude
    ylim = c(24, 50),           # continental US latitude
    expand = FALSE
  ) +
  scale_fill_viridis_c(option="B", na.value="grey90") +
  theme_void(base_size = 11) +
  theme(
    legend.position   = c(0.85, 0.25),
    legend.direction  = "vertical",
    legend.key.width  = unit(0.25, "cm"),
    legend.key.height = unit(1.0,  "cm"),
    legend.text       = element_text(size = 6),
    legend.title      = element_text(size = 7),
    plot.margin       = margin(0,0,0,0)
  ) +
  labs(fill = "Δ Trump %", caption = "Contiguous U.S. counties only")

# Task 4: Initial Question Analysis
# 0) Install & load required packages

needed <- c("sf","dplyr","stringr","readr","purrr","janitor","tibble","datasets")
for(pkg in needed){
  if(!requireNamespace(pkg, quietly=TRUE)) install.packages(pkg)
  library(pkg, character.only=TRUE)
}

# 1) Read in the county shapefile

counties_sf <- sf::st_read("data/mp04/cb_2023_us_county_500k.shp", quiet=TRUE)


# 2) Build own STATEFP → state lookup

library(tibble)
state_fips_df <- tibble(
  STATEFP = sprintf("%02d", c(
    1, 2, 4, 5, 6, 8, 9,10,12,13,15,16,17,18,19,20,21,22,23,24,
    25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,
    46,47,48,49,50,51,53,54,55,56
  )),
  state = c(
    "Alabama","Alaska","Arizona","Arkansas","California","Colorado",
    "Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho",
    "Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana",
    "Maine","Maryland","Massachusetts","Michigan","Minnesota",
    "Mississippi","Missouri","Montana","Nebraska","Nevada",
    "New Hampshire","New Jersey","New Mexico","New York",
    "North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
    "Pennsylvania","Rhode Island","South Carolina","South Dakota",
    "Tennessee","Texas","Utah","Vermont","Virginia","Washington",
    "West Virginia","Wisconsin","Wyoming"
  )
)

# Then immediately after reading the shapefile:
counties_sf <- sf::st_read("data/mp04/cb_2023_us_county_500k.shp", quiet=TRUE) %>%
  left_join(state_fips_df, by = "STATEFP")



# 3) Prepare two result tables (run this only if not already in env)

# load("all_results_2024.RData")  # if you saved them
# load("all_results_2020.RData")

# Make sure county names are Title Case to match shapefile NAME
res24 <- all_results_2024 %>%
  mutate(county = str_to_title(county))

res20 <- all_results_2020 %>%
  mutate(county = str_to_title(county))


# 4) Join shapefile → results

join24 <- counties_sf %>%
  left_join(res24, by = c("NAME"="county","state"="state"))

join20 <- counties_sf %>%
  left_join(res20, by = c("NAME"="county","state"="state"))


# 5) Answer each question


# 1. Most Trump votes in 2024
q1 <- join24 %>%
  slice_max(trump_votes, n = 1) %>%
  st_drop_geometry() %>%
  select(county = NAME, state, trump_votes)

# 2. Highest Biden vote‐share in 2020
q2 <- join20 %>%
  mutate(biden_share = biden_votes / total_votes) %>%
  slice_max(biden_share, n = 1) %>%
  st_drop_geometry() %>%
  select(county = NAME, state, biden_share)

# 3. Largest Trump‐share gain 2020→2024
shift_df <- res24 %>%
  select(state, county, trump24 = trump_votes, tot24 = total_votes) %>%
  inner_join(
    res20 %>% select(state, county, trump20 = trump_votes, tot20 = total_votes),
    by = c("state","county")
  ) %>%
  mutate(
    pct24 = trump24 / tot24,
    pct20 = trump20 / tot20,
    delta_trump_pct = pct24 - pct20
  )

q3 <- shift_df %>%
  slice_max(delta_trump_pct, n = 1) %>%
  select(county, state, delta_trump_pct)

# 4. State that swung most towards Harris (i.e., smallest ΔTrump‐share)
state_shift <- shift_df %>%
  group_by(state) %>%
  summarize(
    share24 = sum(trump24) / sum(tot24),
    share20 = sum(trump20) / sum(tot20),
    delta    = share24 - share20,
    .groups = "drop"
  )

q4 <- state_shift %>%
  slice_min(delta, n = 1) %>%
  select(state, delta)

# 5. Largest county by area
q5 <- counties_sf %>%
  mutate(area_km2 = as.numeric(ALAND) / 1e6) %>%
  slice_max(area_km2, n = 1) %>%
  st_drop_geometry() %>%
  select(county = NAME, state, area_km2)

# 6. Highest 2020 voter density (use total_votes / area)
q6 <- join20 %>%
  mutate(
    area_km2        = as.numeric(ALAND) / 1e6,
    turnout_density = total_votes / area_km2
  ) %>%
  slice_max(turnout_density, n = 1) %>%
  st_drop_geometry() %>%
  select(county = NAME, state, turnout_density)

# 7. Biggest raw turnout increase 2020→2024
# 7a) Collapse each year down to exactly one row per (state, county)
res24_unique <- res24 %>%
  group_by(state, county) %>%
  summarize(
    tot24 = sum(total_votes, na.rm = TRUE),
    .groups = "drop"
  )

res20_unique <- res20 %>%
  group_by(state, county) %>%
  summarize(
    tot20 = sum(total_votes, na.rm = TRUE),
    .groups = "drop"
  )

# 7b) Inner‐join the two summary tables
turnout_change <- res24_unique %>%
  inner_join(res20_unique, by = c("state", "county"))

# 7c) Compute the delta and pick the top county
q7 <- turnout_change %>%
  mutate(delta_turnout = tot24 - tot20) %>%
  slice_max(delta_turnout, n = 1)


# 6) Print them out

q1; q2; q3; q4; q5; q6; q7





# Install missing packages
pkgs <- c("sf","dplyr","leaflet","viridisLite","stringr","scales")
for (p in pkgs) if (!requireNamespace(p, quietly=TRUE)) install.packages(p)

library(sf)
library(dplyr)
library(leaflet)
library(viridisLite)
library(stringr)
library(scales)

# 1. Read & crop to the Lower 48
state_fips_df <- tibble::tibble(
  STATEFP = sprintf("%02d", c(
    1,4,5,6,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,
    25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,
    46,47,48,49,50,51,53,54,55,56
  )),
  state   = c(
    "Alabama","Arizona","Arkansas","California","Colorado","Connecticut",
    "Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana",
    "Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts",
    "Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska",
    "Nevada","New Hampshire","New Jersey","New Mexico","New York",
    "North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
    "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah",
    "Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"
  )
)

counties_sf <- sf::st_read(
  "data/mp04/cb_2023_us_county_500k.shp", quiet=TRUE
) %>%
  left_join(state_fips_df, by="STATEFP") %>%
  st_crop(xmin=-125, xmax=-66, ymin=24, ymax=50)

# 2. Prep election results
res24 <- all_results_2024 %>% mutate(county = str_to_title(county))
res20 <- all_results_2020 %>% mutate(county = str_to_title(county))

# 3. Compute Δ pp
shift_df <- res24 %>%
  select(state, county, trump24=trump_votes, tot24=total_votes) %>%
  inner_join(
    res20 %>% select(state, county, trump20=trump_votes, tot20=total_votes),
    by = c("state","county")
  ) %>%
  mutate(delta_pp = (trump24/tot24 - trump20/tot20) * 100)

# 4. Build arrow_lines
L <- 0.2
arrow_lines <- counties_sf %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(counties_sf %>% st_set_geometry(NULL) %>% select(NAME, state)) %>%
  left_join(shift_df, by=c("NAME"="county","state"="state")) %>%
  filter(!is.na(delta_pp)) %>%
  rowwise() %>%
  mutate(
    angle = if_else(delta_pp>=0, pi/4, -3*pi/4),
    xend  = X + L*cos(angle),
    yend  = Y + L*sin(angle),
    popup = paste0(
      "<strong>", NAME, ", ", state, "</strong><br/>",
      sprintf("%.1f pp more Republican than 2020<br/>", delta_pp),
      "Trump 2024: ", comma(trump24), "<br/>",
      "Trump 2020: ", comma(trump20)
    ),
    geometry = st_sfc(
      st_linestring(matrix(c(X, Y, xend, yend), ncol=2, byrow=TRUE)),
      crs = 4326
    )
  ) %>%
  st_as_sf() %>%
  ungroup()

# 5. Color palette
pal <- colorNumeric(
  palette = viridisLite::viridis(100, direction=-1),
  domain  = arrow_lines$delta_pp,
  na.color = "transparent"
)

# 6. Draw interactive map with hover & click
leaflet(arrow_lines) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(
    color            = ~pal(delta_pp),
    weight           = 2,
    opacity          = 0.8,
    popup            = ~popup,               # on click
    label            = ~popup,               # on hover
    highlightOptions = highlightOptions(
      weight    = 4,
      color     = "yellow",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = pal,
    values   = ~delta_pp,
    title    = "Δ Trump (pp)"
  ) %>%
  setView(lng=-95, lat=37, zoom=4)


