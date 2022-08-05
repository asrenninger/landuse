##################################
## Regression Data
##################################

## packages
library(tidyverse)
library(tidymodels)
library(sf)
library(tigris)
library(tidycensus)
library(tmap)
library(tmaptools)
library(spdep)

## PATHS

STATE_STR = 'NJ'
STATE_FIPS = '34'

# PATHS

ESI_DIR <- "\\\\econha01/client/CLIENT/1 Consulting/Freddie Mac/Prediction Modeling"
STATE_DIR <- paste0(ESI_DIR, "/", STATE_STR)
setwd(STATE_DIR)

zoning_csv <- "data/density/ID_summary.bg-05262022.csv"
# if income is pre-processed
## cbg_income_csv <- "~/Desktop/R/git/philamonitor/data/census/data/cbg_b19.csv"
cbg_household_csv = "~/Desktop/R/git/philamonitor/data/census/data/cbg_b25.csv"
cbg_pop_csv = "~/Desktop/R/git/philamonitor/data/census/data/cbg_b02.csv"
area_character_csv = "python/area_characteristics_nj.csv"




SENSING_DIR <- paste0(STATE_DIR, "/data/sensing")
# created in EXTRACT
remote_sensing_csv =
    glue::glue("{SENSING_DIR}/remote_sensing_{STATE_STR}.csv")

comp_tif = glue("comp_{STATE_FIPS}.tif")

cities_csv = "data/cities/uscities.csv"




## import blocks

blocks <-
  block_groups(state = "34", cb = TRUE, class = 'sf') %>%
  st_transform(3857) %>%
  transmute(GEOID,
            area_total = units::set_units(st_area(geometry), acres),
            centroid = st_centroid(geometry)) %>%
  mutate(X = st_coordinates(centroid)[, 1],
         Y = st_coordinates(centroid)[, 2]) %>%
  select(-centroid)

plot(st_geometry(blocks))

cbg_zoning <-
  read_csv(
    zoning_csv,
    col_type = cols("GEOID10" = col_character())) %>%
  glimpse() %>%
  transmute(GEOID = GEOID10,
            maxhd_micro = MAXDEN.micro,
            maxhd_macro = MAXDEN.macro)

state_cbg_geoids = blocks$GEOID

# import csv and filter by GEOIDs
cbg_intake <- function(csv_path, cbg_geoids = state_cbg_geoids) {
  vroom::vroom(csv_path) %>%
    rename(GEOID = census_block_group) %>%
    filter(GEOID %in% cbg_geoids)
}

#cbg_income <-
#  cbg_intake(cbg_income_csv) %>%
#  select(GEOID, B19301e1, B19301m1) %>%
#  transmute(median_income = B19301e1)
cbg_income <- get_acs(
  geography="block group",
  table = "B19013",
  year=2019,
  state=STATE_STR,
  geometry=FALSE
) %>%
  dplyr::select(GEOID, estimate)%>%
  rename(median_income = estimate)

cbg_hhsize <-
  cbg_intake(cbg_household_csv) %>%
  select(GEOID, B25010e1) %>%
  transmute(household_size = B25010e1)

cbg_pop <-
  cbg_intake(cbg_pop_csv) %>%
  select(GEOID, B02001e1, B02001e2) %>%
  transmute(
    population = B02001e1,
    pct_nonwhite = 1 - (B02001e2 / B02001e1)) %>%
  left_join(blocks) %>%
  transmute(GEOID,
            density = population / units::drop_units(area_total),
            pct_nonwhite)

# OSMnx & Buildings
cbg_urban_morphology <-
  read_csv(
    area_character_csv,
    col_type = cols("GEOID" = col_character())) %>%
  left_join(blocks) %>%
  transmute(
    GEOID,
    isoperi_mean,
    building_size = area_normed,
    intersection_density =
      node_count / units::drop_units(area_total),
    prop_4way,
    orientation_order)

# extract.R results
# remote sensing => development patterns
cbg_remote_sensing <-
  blocks_complete %>%
  st_drop_geometry() %>%
  transmute(GEOID,
            ndvi_mean,
            built_intensity = total_development_norm,
            built_change = case_when(development_change > 0 ~ 1,
                                     TRUE ~ 0))

## cities and towns file
towns <-
  read_csv(cities_csv) %>%
    st_as_sf(
      coords = c("lng", "lat"), crs = 4326, remove = FALSE) %>%
    filter(population > 10000) %>%
    st_transform(3857) %>%
    getXYcols(.)

cities <-
  towns %>%
    filter(population > 100000)

## distance to cities
d_city <-
  FNN::get.knnx(select(st_drop_geometry(cities), X, Y), select(st_drop_geometry(blocks), X, Y), k = 1) %>%
  magrittr::use_series("nn.dist") %>%
  as_tibble() %>%
  rownames_to_column(var = "id") %>%
  left_join(rownames_to_column(blocks, var = "id")) %>%
  transmute(GEOID, dist_city = V1)

## distance to smaller towns
d_town <-
  FNN::get.knnx(select(st_drop_geometry(towns), X, Y), select(st_drop_geometry(blocks), X, Y), k = 10) %>%
  magrittr::use_series("nn.dist") %>%
  as_tibble() %>%
  rownames_to_column(var = "id") %>%
  pivot_longer(!id) %>%
  group_by(id) %>%
  summarise(dist_towns = mean(value)) %>%
  left_join(rownames_to_column(blocks, var = "id")) %>%
  select(GEOID, dist_towns)

## data for the left side of the regression
regression <-
  cbg_zoning %>%
  left_join(cbg_income) %>%
  left_join(cbg_hhsize) %>%
  left_join(cbg_pop) %>%
  left_join(cbg_urban_topo) %>%
  left_join(area_morphology) %>%
  left_join(d_town) %>%
  left_join(d_city) %>%
  mutate(built_change = factor(built_change)) #%>%
  #write_csv(glue("regression_dataframe_{STATE}_updated.csv")) %>%
  #glimpse()

## data to put through the spatial lag function
lag_data <-
  cbg_income %>%
  left_join(cbg_hhsize) %>%
  left_join(cbg_pop) %>%
  left_join(cbg_urban_topo) %>%
  left_join(area_morphology) %>%
  left_join(d_town) %>%
  left_join(d_city) %>%
  mutate(built_change = factor(built_change)) %>%
  left_join(blocks) %>%
  mutate(area_total = units::drop_units(area_total)) %>%
  st_as_sf()

## smoothing
infill <- nn_interpolate(lag_data, 3)

## combine data
reg.data <-
  left_join(
    regression,
    infill %>%
      select(., -geometry)) %>%
  left_join(
    lag_data %>%
      st_drop_geometry() %>%
      dplyr::select(-area_total,-X,-Y)
  )

save(regression,
  file="R/regression_data.Rda")