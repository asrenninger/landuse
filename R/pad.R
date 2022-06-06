###############################
## Protected areas
###############################

## packages
library(tidyverse)
library(sf)
library(tigris)

## set the state
geography <- "30"

# get name
geog_name <- 
  tigris::fips_codes %>%
  filter(state_code == geography) %>%
  distinct(state_name) %>%
  pull()

# get local crs
geog_proj <-
  crsuggest::crs_sf %>%
  filter(str_detect(crs_name, "NAD83\\(HARN\\)"),
         crs_units == "m") %>%
  filter(str_detect(crs_name, geog_name)) %>%
  pull(crs_proj4)

# another way...
# crsuggest::suggest_crs(block_groups(state = geography, cb = TRUE, class = 'sf'))

## block groups
blocks <- 
  block_groups(state = geography, cb = TRUE, class = 'sf') %>% 
  st_transform(geog_proj) %>% 
  transmute(GEOID, 
            area_total = units::set_units(st_area(geometry), m^2))

plot(st_geometry(blocks))

## state boundary
border <- 
  blocks %>%
  st_union() %>%
  st_combine()

## which layers do we want?
layers <- st_layers("data/pad/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg")
layers <- layers$name[c(6:8, 10)]

## load all layers 
pad <- 
  map(layers,
      ~st_read("data/pad/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg",
               layer = .x) %>%
        st_transform(st_crs(border)) %>%
        st_intersection(border))

## difference out the protected areas 
protections <- 
  reduce(imap(pad, 
              function(x, y){
                designation <- 
                  x %>%
                  st_union() %>% 
                  st_combine()
                
                layer_label <- layers[y]
                
                tictoc::tic()
                designation_area <- 
                  blocks %>%
                  st_difference(designation) %>%
                  mutate(area_protected = area_total - units::set_units(st_area(geometry), acres),
                         protection = layer_label) %>%
                  st_drop_geometry() %>%
                  select(-area_total)
                tictoc::toc()
                
                return(designation_area)
                
              }),
         rbind)

## aggregate all protections
protections %>%
  left_join(blocks) %>%
  mutate(area_total = units::drop_units(area_total), 
         area_protected = units::drop_units(area_protected),
         area_protected = round(area_protected, 4)) %>%
  pivot_wider(id_cols = GEOID:area_total,
              names_from = protection,
              values_from = area_protected) %>%
  mutate_at(vars(starts_with("PAD")), funs(case_when(is.na(.) ~ area_total, TRUE ~ .))) %>%
  write_csv("protections_mt.csv")

