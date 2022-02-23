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

zoning <- 
  # read_csv("~/Dropbox/ESI-Wharton Freddie Project/Deliverables/MD_2021-10-25/MD_BlockGroup_Dataset_2021-10-25.csv",
  #          col_type = cols("BG2010" = col_character())) %>% 
  read_csv("~/Dropbox/ESI-Wharton Freddie Project/NJ Dataset/NJ.maxden.bg_0128.csv",
           col_type = cols("GEOID10" = col_character())) %>%
  glimpse() %>% 
  transmute(GEOID = GEOID10, 
            maxhd_micro = MAXDEN.micro,
            maxhd_macro = MAXDEN.macro)

hist(log(zoning$maxhd_macro))
hist(log(zoning$maxhd_micro))

income <- 
  vroom::vroom("~/Desktop/R/git/philamonitor/data/census/data/cbg_b19.csv") %>% 
  filter(census_block_group %in% blocks$GEOID) %>%
  select(census_block_group, B19301e1, B19301m1) %>% 
  transmute(GEOID = census_block_group,
            median_income = B19301e1)

size <- 
  vroom::vroom("~/Desktop/R/git/philamonitor/data/census/data/cbg_b25.csv") %>% 
  filter(census_block_group %in% blocks$GEOID) %>%
  select(census_block_group, B25010e1) %>%
  transmute(GEOID = census_block_group, 
            household_size = B25010e1)

population <- 
  vroom::vroom("~/Desktop/R/git/philamonitor/data/census/data/cbg_b02.csv") %>% 
  filter(census_block_group %in% blocks$GEOID) %>%
  select(census_block_group, B02001e1, B02001e2) %>%
  transmute(GEOID = census_block_group, 
            population = B02001e1,
            pct_nonwhite = 1 - (B02001e2 / B02001e1)) %>%
  left_join(blocks) %>%
  transmute(GEOID, 
            density = population / units::drop_units(area_total),
            pct_nonwhite)

area_topology <- 
  read_csv("python/area_characteristics_nj.csv", col_type = cols("GEOID" = col_character())) %>%
  left_join(blocks) %>%
  transmute(GEOID, 
            isoperi_mean, 
            building_size = area_normed,
            intersection_density = node_count / units::drop_units(area_total),
            prop_4way, 
            orientation_order)

area_morphology <-
  blocks_complete %>% 
  st_drop_geometry() %>%
  transmute(GEOID, 
            ndvi_mean,
            built_intensity = total_development_norm,
            built_change = case_when(development_change > 0 ~ 1,
                                     TRUE ~ 0))

## cities and towns file
cities <- 
  read_csv("data/cities/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE) %>%
  filter(population > 100000) %>%
  st_transform(3857) %>%
  mutate(X = st_coordinates(geometry)[, 1],
         Y = st_coordinates(geometry)[, 2]) %>%
  glimpse()

towns <- 
  read_csv("data/cities/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE) %>%
  filter(population > 10000) %>%
  st_transform(3857) %>%
  mutate(X = st_coordinates(geometry)[, 1],
         Y = st_coordinates(geometry)[, 2]) %>%
  glimpse()

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
  zoning %>% 
  left_join(income) %>% 
  left_join(size) %>% 
  left_join(population) %>% 
  left_join(area_topology) %>% 
  left_join(area_morphology) %>%
  left_join(d_town) %>%
  left_join(d_city) %>%
  mutate(built_change = factor(built_change)) %>%
  write_csv("regression_dataframe_nj_updated.csv") %>%
  glimpse()

## data to put through the spatial lag function
lag_data <- 
  income %>% 
  left_join(size) %>% 
  left_join(population) %>% 
  left_join(area_topology) %>% 
  left_join(area_morphology) %>%
  left_join(d_town) %>%
  left_join(d_city) %>%
  mutate(built_change = factor(built_change)) %>%
  left_join(blocks) %>%
  mutate(area_total = units::drop_units(area_total)) %>%
  st_as_sf()

## smoothing
infill <- nn_interpolate(lag_data, 3)

     