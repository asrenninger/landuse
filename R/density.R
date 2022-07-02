###############################
## Population densities
###############################

## packages
library(tidyverse)
library(tidycensus)
library(tigris)
library(crsuggest)
library(tmap)
library(tmaptools)
library(sf)

options(tigris_use_cache = TRUE)

## choose states
states <- c("04", "12", "15", "16", "24", "30", "34", "35", "44")

## map it
map(states, function(state){
  
  geography <- state
  
  geog_name <- 
    tigris::fips_codes %>%
    filter(state_code == geography) %>%
    distinct(state_name) %>%
    pull()
  
  print(geog_name)
  
  # get local crs
  geog_proj <-
    crsuggest::crs_sf %>%
    # filter(str_detect(crs_name, "NAD83\\(HARN\\)"),
    #        crs_units == "m") %>%
    filter(str_detect(crs_name, "NAD83"),
           !str_detect(crs_name, "\\("),
           crs_units == "m") %>%
    filter(str_detect(crs_name, geog_name)) %>%
    slice(1) %>%
    pull(crs_proj4)
  
  # another way...
  # crsuggest::suggest_crs(block_groups(state = geography, cb = TRUE, class = 'sf'))
  
  ## block groups
  blocks <-
    get_acs(geography = 'tract', state = geography, variables = "B01001_001", year = 2019, geometry = TRUE) %>% 
    st_transform(2163) %>%
    mutate(area = units::drop_units(units::set_units(st_area(geometry), mi^2))) %>%
    filter(area > 0.01,
           !GEOID %in% cut_tracts) %>%
    transmute(area,
              population = estimate,
              density = round(population / area),
              state = geog_name, 
              geometry) %>%
    st_transform(geog_proj)
  
  map <- 
    tm_shape(blocks) +
    tm_fill(col = "density", style = 'jenks', n = 7, palette = RColorBrewer::brewer.pal(n = 8, name = 'PuBu')[2:8],
            legend.outside = TRUE, legend.hist = TRUE) +
    tm_layout(title = geog_name,
              frame = NA,
              legend.outside = TRUE,
              legend.hist.height = 0.25,
              legend.hist.width = 0.75)
  
  tmap_save(map, filename = glue::glue("density_{geography}.png"), height = 8, width = 8)
  
  
})
                                  