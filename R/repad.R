###############################
## Protected areas
###############################

## packages
library(tidyverse)
library(sf)
library(tigris)

states <- c("04", "12", "15", "16", "24", "30", "34", "44")
map(states, get_pad)

states <- unique(tigris::fips_codes$state_code)[1:51]
states <- states[!states %in% c("02", "04", "12", "15", "16", "24", "30", "34", "44")]
map(states[5:length(states)], get_pad)

## set the state
get_pad <-
  function(geography) {
    
    print(geography)
    start <- Sys.time() 
    
    # get name
    geog_name <- 
      tigris::fips_codes %>%
      filter(state_code == geography) %>%
      distinct(state_name) %>%
      pull()
    
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
      block_groups(state = geography, cb = TRUE, class = 'sf', year = 2019) %>% 
      st_transform(geog_proj) %>% 
      transmute(GEOID, 
                area_total = units::set_units(st_area(geometry), m^2))
    
    plot(st_geometry(blocks))
    
    ## state boundary
    border <- 
      blocks %>%
      st_transform(2163) %>%
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
            st_transform(2163) %>%
            st_intersection(border) %>% 
            st_transform(geog_proj) %>%
            transmute(geometry = SHAPE))
    
    ## difference out the protected areas 
    pad <- 
      pad %>% 
      reduce(rbind) %>%
      st_union() %>% 
      st_combine()
    
    protected_area <- 
      blocks %>%
      st_difference(pad) %>%
      mutate(area_protected = area_total - units::set_units(st_area(geometry), m^2)) %>%
      st_drop_geometry() %>%
      mutate(area_total = units::drop_units(area_total),
             area_protected = units::drop_units(area_protected)) %>%
      mutate(area_protected = if_else(area_protected < 0, 0, area_protected))
    
    end <- Sys.time()
    print(end - start)
    
    ## write it out...
    write_csv(protected_area, glue::glue("~/Desktop/R/git/landuse/data/pad/processed/protections_{geography}.csv"))
    
  }
  
