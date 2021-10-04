########################################
## State by state developability
########################################

## packages
library(tidycensus)
library(tigris)
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)

## every block group in the country
contiguous <- unique(fips_codes$state)[1:51][-c(2, 12)]

united_blocks <-
  reduce(map(contiguous,
             function(x){
               block_groups(state = x, cb = TRUE, class = 'sf')
             }
  ), 
  rbind)

united_states <-
  states(cb = TRUE, class = 'sf') %>% 
  filter(STUSPS %in% contiguous)

## populations for them
united_population <-
  map_df()

## Plotting all protected areas
pad <- 
  st_read("data/nation/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg", 
          layer = "PADUS2_1Combined_Fee_Designation_Easement")

# filtering  
contiguous <- unique(tigris::fips_codes$state)[1:51][-c(2, 12)]

states <- 
  states(cb = TRUE, class = 'sf') %>%
  mutate(state_area = units::set_units(st_area(geometry), ha)) %>%
  filter(STUSPS %in% contiguous) %>%
  st_transform(2163)

counties <- 
  counties(cb = TRUE, class = 'sf') %>% 
  st_transform(3857)

# map it
tmap_save(
  pad %>% 
    st_transform(2163) %>% 
    rownames_to_column(var = "id") %>% 
    select(id) %>%
    st_join(states) %>% 
    drop_na(GEOID) %>%
    group_by(id) %>% 
    slice(1) %>% 
    ungroup() %>% 
    st_geometry() %>%
    tm_shape() +
    tm_polygons(col = "#000000", fill = NA, lwd = 0.0001) +
    tm_layout(frame = FALSE),
  filename = "protected_lines.png", height = 20, units = "in", dpi = 300)

# aggregate it
centroid <- 
  pad %>% 
  transmute(protected_area = units::set_units(st_area(SHAPE), ha)) %>% 
  rownames_to_column(var = 'id') %>%
  st_centroid()

# joining
joined <- st_join(st_transform(states, 2163), st_transform(centroid, 2163))
difference <- st_difference(st_transform(states, 2163), st_transform(st_combine(st_union(pad)), 2163))

# plotting
tmap_save(
  joined %>% 
    st_drop_geometry() %>% 
    group_by(NAME, state_area) %>% 
    summarise(protected_area = sum(protected_area)) %>%
    mutate(`percent protected` = 1 - units::drop_units((state_area - protected_area) / state_area)) %>%
    mutate(`percent protected` = if_else(`percent protected` > 0.98, 0.98, `percent protected`)) %>% 
    mutate(`percent protected` = round(`percent protected` * 100)) %>%
    drop_na() %>%
    left_join(states) %>% 
    filter(STUSPS %in% contiguous) %>%
    as.data.frame() %>%
    st_as_sf() %>% 
    transmute(`percent protected`,
              value = glue::glue("{`percent protected`}%")) %>% 
    st_transform(2163) %>%
    tm_shape() +
    tm_fill(col = "percent protected") +
    tm_text(text = "value", size = 1.5, remove.overlap = TRUE) + 
    tm_borders(col = "#000000", lty = 2) +
    tm_layout(frame = FALSE,
              legend.show = FALSE),
  filename = "protected_areas_redux.png", height = 8, dpi = 300)

## fl
fl <- 
  states %>% 
  filter(STUSPS == "FL") %>% 
  st_union() %>% 
  st_combine()

pad_fl <- st_intersection(st_transform(pad, 2163), fl)

# map it
tmap_save(
  pad_fl %>% 
    st_transform(2163) %>% 
    st_geometry() %>%
    tm_shape() +
    tm_fill(col = "#000000") +
    tm_shape(counties %>% 
               filter(STATEFP == "12")) +
    tm_borders(col = '#4a4a4a', lwd = 2, lty = 3) + 
    tm_layout(frame = FALSE),
  filename = "protected_fl_redux.png", height = 10, units = "in", dpi = 300)

# md
md <- 
  states %>% 
  filter(STUSPS == "MD") %>% 
  st_union() %>% 
  st_combine()

pad_md <- st_intersection(st_transform(pad, 2163), md)

# map it
tmap_save(
  pad_md %>% 
    st_transform(3857) %>% 
    st_geometry() %>%
    tm_shape() +
    tm_fill(col = "#000000") +
    tm_shape(counties %>% 
               filter(STATEFP == "24")) +
    tm_borders(col = '#4a4a4a', lwd = 2, lty = 3) + 
    tm_layout(frame = FALSE),
  filename = "protected_md_redux.png", height = 5, units = "in", dpi = 300)

# nj
nj <- 
  states %>% 
  filter(STUSPS == "NJ") %>% 
  st_union() %>% 
  st_combine()

pad_nj <- st_intersection(st_transform(pad, 2163), nj)

# map it
tmap_save(
  pad_nj %>% 
    st_transform(3857) %>% 
    st_geometry() %>%
    tm_shape() +
    tm_fill(col = "#000000") +
    tm_shape(counties %>% 
               filter(STATEFP == "34")) +
    tm_borders(col = '#4a4a4a', lwd = 2, lty = 3) + 
    tm_layout(frame = FALSE),
  filename = "protected_nj_redux.png", height = 10, units = "in", dpi = 300)

## all together now
blades <- united_states %>% group_by(STUSPS) %>% group_split() %>% map(~st_combine(st_union(.x)))

state_areas <- 
  reduce(map(blades,
             function(blade){
               return(st_combine(st_union(st_intersection(pad, blade))))
             }
             
  ), 
  rbind)




