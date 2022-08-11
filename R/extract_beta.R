###############################
## Area Characteristics
###############################

# packages
library(tidyverse)
library(sf)
library(stars)
library(terra)
library(tmap)
library(tmaptools)
library(exactextractr)
library(tigris)

## set the state
geography <- "12"
reference <- get_crs(geography)

name <- "FLORIDA"

## get the images from the (combined) drive folder
images <- fs::dir_ls(glue::glue("~/Desktop/R/git/landuse/data/raster/{name}"))

## aggregate the images
tictoc::tic()
# load them all with terra::rast, iterating over the paths
parts <- map(images, ~terra::rast(.x))
# create raster collection with terra::sprc and terra::merge them to a single raster
whole <- terra::merge(terra::sprc(parts))
# take the time for good measure: florida was 45 minutes...?
tictoc::toc()

tictoc::tic()
terra::writeRaster(whole, filename = "comp_12.tif")
tictoc::toc()

## plot to see all the bands
plot(whole)

## block groups
blocks <- 
  block_groups(state = geography, cb = TRUE, class = 'sf', year = 2019) %>% 
  # use local reference for most accurate area measurement
  st_transform(reference) %>% 
  transmute(GEOID, 
            STATEFP, 
            COUNTYFP,
            area_total = units::drop_units(units::set_units(st_area(geometry), mi^2))) %>%
  # get back to the GEE CRS
  st_transform(3857)

# plot to check
plot(st_geometry(blocks))

counties <- group_split(blocks, COUNTYFP)
zonal_stats <- map_dfr(counties, process_county)

write_csv(st_drop_geometry(zonal_stats), glue::glue("zonal_stats_{geography}.csv"))

## SAVE THESE FUNCTIONS SO THE SCRIPT WILL RUN
get_crs <-
  function(state){
    
    # get name
    geog_name <- 
      tigris::fips_codes %>%
      filter(state_code == !!state) %>%
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
    
    return(geog_proj)
    
  }

process_county <- 
  function(county){
  
  plot(county)
  
  tictoc::tic()
  clipped <- terra::crop(whole, st_combine(st_union(county)))
  tictoc::toc()
  
  ## zonal statistics, a lot of them
  tictoc::tic()
  # start with a total pixel count to use  as the dividend in normalizations 
  pixels <- terra::classify(clipped$landcover_2016, matrix(c(0, 100, 1), ncol = 3, byrow =TRUE))
  county$pixels <- exact_extract(pixels, county, 'sum')
  
  # various spectral indicies
  county$ndvi_mean <- exact_extract(clipped$NDVI, county, 'mean')
  county$ndwi_mean <- exact_extract(clipped$NDWI, county, 'mean')
  county$ndbi_mean <- exact_extract(clipped$NDBI, county, 'mean')
  
  # impervious surface
  county$impervious_mean <- exact_extract(clipped$impervious_2016, county, 'mean')
  
  # slope
  county$slope_mean <- exact_extract(clipped$slope, county, 'mean')
  county$slope_sum <- exact_extract(clipped$slope, county, 'sum')
  
  # night lights
  county$nightlights_2016_mean <- exact_extract(clipped$avg_rad_1, county, 'mean')
  county$nightlights_2016_sum <- exact_extract(clipped$avg_rad_1, county, 'sum')
  county$nightlights_2021_mean <- exact_extract(clipped$avg_rad, county, 'mean')
  county$nightlights_2021_sum <- exact_extract(clipped$avg_rad, county, 'sum')
  
  county$stablelights_y <- exact_extract(clipped$stablelights_y, county, 'mean')
  county$stablelights_b <- exact_extract(clipped$stablelights_b, county, 'mean')
  
  # population
  county$daytime_pop_mean <- exact_extract(clipped$avg_rad, county, 'mean')
  county$daytime_pop_sum <- exact_extract(clipped$avg_rad, county, 'sum')
  county$nighttime_pop_mean <- exact_extract(clipped$avg_rad_1, county, 'mean')
  county$nighttime_pop_sum <- exact_extract(clipped$avg_rad_1, county, 'sum')
  
  # more ways to get at impervious, as summed "high impervious" pixels
  rewhole <- terra::classify(clipped$impervious_2016, matrix(c(0, 50, 0,  50, 100, 1), ncol = 3, byrow =TRUE))
  county$impervious_count <- exact_extract(rewhole, county, 'sum')
  
  # another way to get at slope in the same way
  rewhole <- terra::classify(clipped$slope, matrix(c(0, 20, 0,  20, 100, 1), ncol = 3, byrow =TRUE))
  county$slope_count <- exact_extract(rewhole, county, 'sum')
  
  # reclassify USGS landcover to simple ordinal framework for easier math
  total_dev <- terra::classify(clipped$landcover_2016, matrix(c(10, 20, 0, # not built : 0
                                                                20, 21, 1, # low built : 1
                                                                21, 22, 2, # built more: 2
                                                                22, 23, 3, # built more: 3
                                                                23, 24, 4, # built more: 4
                                                                30, 95, 0), # not built: 0 
                                                              ncol = 3, byrow =TRUE))
  
  # sum it
  county$total_development <- exact_extract(total_dev, county, 'sum')
  
  # just get a high intensity
  rewhole <- terra::classify(clipped$landcover_2016, matrix(c(0, 23, 0, 23, 24, 1, 30, 100, 0), ncol = 3, byrow =TRUE))
  county$high_development <- exact_extract(rewhole, county, 'sum')
  
  # just get at medium intensity
  rewhole <- terra::classify(clipped$landcover_2016, matrix(c(0, 22, 0, 22, 23, 1, 23, 100, 0), ncol = 3, byrow =TRUE))
  county$medium_development <- exact_extract(rewhole, county, 'sum')
  
  # just get at low intensity
  rewhole <- terra::classify(clipped$landcover_2016, matrix(c(0, 21, 0, 21, 22, 1, 22, 100, 0), ncol = 3, byrow =TRUE))
  county$low_development <- exact_extract(rewhole, county, 'sum')
  
  ## historic
  total_dev <- terra::classify(clipped$landcover_2006, matrix(c(10, 20, 0,
                                                                20, 21, 1, 
                                                                21, 22, 2, 
                                                                22, 23, 3,
                                                                23, 24, 4,
                                                                30, 95, 0), 
                                                              ncol = 3, byrow =TRUE))
  
  rewhole <- terra::classify(clipped$impervious_2006, matrix(c(0, 50, 0,  50, 100, 1), ncol = 3, byrow =TRUE))
  
  county$historic_development <- exact_extract(total_dev, county, 'sum')
  county$historic_impervious <- exact_extract(rewhole, county, 'sum')
  
  tictoc::toc()
  
  ## aggregating
  county_complete <- 
    county %>%
    st_as_sf() %>% 
    mutate(total_development_norm = total_development / pixels,
           total_development_norm_historic = historic_development / pixels,
           high_development_norm = high_development / pixels,
           medium_development_norm = medium_development / pixels,
           low_development_norm = low_development / pixels,
           daytime_pop_norm = daytime_pop_sum / area_total, 
           nighttime_pop_norm = nighttime_pop_sum / area_total) %>% 
    mutate(slope_sum_norm = slope_sum / pixels,
           slope_count_norm = slope_count / pixels) %>% 
    mutate(impervious_count_norm = impervious_count / pixels,
           impervious_count_norm_historic = historic_impervious / pixels) %>% 
    mutate(development_change = (total_development_norm - total_development_norm_historic) / total_development_norm_historic,
           impervious_change = (impervious_count_norm - impervious_count_norm_historic) / impervious_count_norm_historic,
           nightlights_mean_change = nightlights_2021_mean - nightlights_2016_mean,
           nightlights_sum_change = (nightlights_2021_sum / area_total) - (nightlights_2016_sum / area_total))  %>%
    mutate(daytime_pop_norm = daytime_pop_sum / area_total,
           nighttime_pop_norm = nighttime_pop_sum / area_total,
           pop_difference_norm = (daytime_pop_sum - nighttime_pop_sum) / area_total)
  
  return(county_complete)
  
  }
