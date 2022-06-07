###################################
## Helper Functions
###################################

## packages
library(tidyverse)
library(sf)
library(tigris)
library(crsuggest)

## smoothing
nn_interpolate <- function(data, depth = 5) {
  
  play <-
    data %>%
    mutate(X = st_coordinates(st_centroid(geometry))[, 1],
           Y = st_coordinates(st_centroid(geometry))[, 2]) %>%
    st_transform(3857) %>% 
    arrange(Y, X) %>%
    rownames_to_column() %>%
    mutate(rowname = as.numeric(rowname)) %>%
    select(-X, -Y) %>%
    st_as_sf()
  
  crosswalk <- 
    play %>% 
    transmute(id = rowname, 
              GEOID)
  
  frst_degree <- 
    play %>% 
    st_touches() %>% 
    tibble() %>%
    rownames_to_column() %>%
    janitor::clean_names() %>%
    unnest(x) %>%
    transmute(row_id = as.numeric(rowname),
              col_id = x)
  
  scnd_degree <- frst_degree %>% 
    rename(id = row_id,
           row_id = col_id) %>%
    left_join(frst_degree) 
  
  for (i in 1:depth) {
    
    scnd_degree <-
      scnd_degree %>%
      transmute(id = id,
                row_id = col_id) %>%
      left_join(frst_degree)
    
  }
  
  final <-
    scnd_degree %>%
    transmute(id = id,
              rowname = col_id) %>%
    group_by(id) %>%
    distinct(rowname, .keep_all = TRUE) %>%
    ungroup() %>%
    left_join(play) %>%
    st_as_sf() %>% 
    select(-rowname) %>%
    st_drop_geometry() %>%
    group_by(id) %>%
    summarise_if(is.numeric, ~mean(.x, na.rm = TRUE)) %>% 
    ungroup() %>%
    left_join(crosswalk) %>%
    select(-id) %>%
    select(GEOID, everything()) %>%
    rename_if(is.numeric, ~paste0(., "_lag", depth))
  
  return(final)
  
}

## get CRS
get_crs <-
  function(state){
    
    # get name
    geog_name <- 
      tigris::fips_codes %>%
      filter(state_code == geography) %>%
      distinct(state) %>%
      pull()
    
    # get local crs
    geog_proj <-
      crsuggest::crs_sf %>%
      filter(str_detect(crs_name, "NAD83\\(HARN\\)"),
             crs_units == "m") %>%
      filter(str_detect(crs_name, geog_name)) %>%
      slice(1) %>%
      pull(crs_proj4)
    
    return(geog_proj)
    
  }



