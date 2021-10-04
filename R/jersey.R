################################################
## How much land in New Jersey can be developed?
################################################

## packages
library(tidyverse)
library(sf)
library(tigris)
library(tmap)

## criteria
pad <- st_read("data/nation/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg", 
               layer = "PADUS2_1Combined_Fee_Designation_Easement")

sewer <- st_read("data/nj/Statewide_Sewer_Service_Area_for_New_Jersey/Statewide_Sewer_Service_Area_for_New_Jersey.shp")

pinelands <- st_read("data/nj/PinelandsOutline/PinelandsOutline.shp")
highlands <- st_read("data/nj/HighlandsOutline/HighlandsOutline.shp")

## state boudanries
blocks <- block_groups(state = "NJ", cb = TRUE, class = 'sf')

boundary <-
  blocks %>% 
  st_union() %>%
  st_combine()

## areas 
state_area <- st_area(boundary) 
units::set_units(state_area, mi^2)

protected <- 
  pad %>%
  st_transform(st_crs(boundary)) %>% 
  st_intersection(boundary) %>% 
  st_union() %>%
  st_combine()

protected_area <- st_area(protected)
units::set_units(protected_area, mi^2)

connected <-
  sewer %>% 
  st_transform(st_crs(boundary)) %>% 
  st_union() %>%
  st_combine()

connected_area <- st_area(connected)
units::set_units(connected_area, mi^2)

pinelands <- 
  st_read("~/Downloads/PinelandsOutline/PinelandsOutline.shp") %>% 
  st_transform(st_crs(boundary)) %>% 
  st_union() %>% 
  st_combine()

highlands <-
  st_read("~/Downloads/HighlandsOutline/HighlandsOutline.shp") %>%
  st_transform(st_crs(boundary)) %>% 
  st_union() %>% 
  st_combine()

difference <- 
  boundary %>% 
  st_difference(connected)

remove_lands <- 
  boundary %>%
  st_difference(pinelands) %>% 
  st_difference(highlands)

remove_area <- st_area(remove_lands)
units::set_units(remove_area, mi^2)

remove_lands <- 
  boundary %>%
  st_difference(pinelands) %>% 
  st_difference(highlands) %>%
  st_difference(protected) 

remove_area <- st_area(remove_lands)
units::set_units(remove_area, mi^2)

remove_lands <- 
  boundary %>%
  st_difference(pinelands) %>% 
  st_difference(highlands) %>%
  st_difference(protected) %>% 
  st_intersection(connected)

remove_area <- st_area(remove_lands)
units::set_units(remove_area, mi^2)

units::set_units(remove_area, mi^2) / units::set_units(state_area, mi^2)

coverage <- 
  st_read("~/Downloads/OrdCoverage_09-23-21.shp") %>%
  st_transform(st_crs(boundary)) %>%
  st_union() %>% 
  st_combine()

coverage_area <- st_area(coverage)
units::set_units(coverage_area, mi^2)

final_coverage <- st_intersection(remove_lands, coverage)
final_area <- st_area(final_coverage)
units::set_units(final_area, mi^2)

units::set_units(final_area, mi^2) / units::set_units(remove_area, mi^2)

## population
population <- 
  st_read("~/Downloads/NJ_BGjoined_09-21-21.shp") %>%
  st_transform(st_crs(boundary)) %>%
  transmute(GEOID = GEOID_1, 
            state_pop = sum(pop),
            local_pop = pop) %>% 
  st_intersection(boundary) %>% 
  mutate(area_total = units::set_units(st_area(geometry), mi^2)) %>% 
  select(everything(), geometry)

numbers <-
  population %>% 
  st_intersection(remove_lands) %>%
  mutate(area_developable = units::set_units(st_area(geometry), mi^2),
         proportion_developable = area_developable / area_total) %>%
  mutate(local_pop_developable = local_pop * proportion_developable, 
         state_pop_developable = sum(local_pop_developable))

numbers_redux <-
  numbers %>% 
  st_intersection(coverage) %>%
  mutate(area_covered = units::set_units(st_area(geometry), mi^2),
         proportion_covered = area_covered / area_developable) %>%
  mutate(local_pop_covered = local_pop_developable * proportion_covered, 
         state_pop_covered = sum(local_pop_covered))

options(scipen = 999)

numbers_redux %>% 
  st_drop_geometry() %>% 
  select_if(str_detect(names(.), "area")) %>%
  arrange(desc(area_covered))
  transmute(percent_coverage = state_pop_covered / state_pop)

plot(st_geometry(numbers_redux))

numbers %>% 
  st_drop_geometry() %>% 
  transmute(state_pop,
            state_pop_developable= sum(local_pop))

numbers_redux %>% 
  st_drop_geometry() %>% 
  transmute(state_pop,
            state_pop_covered = sum(local_pop))

difference_area <- st_area(difference)
units::set_units(difference_area, mi^2)

intersection_area <- st_area(intersection)
units::set_units(intersection_area, ha)

## map it
RColorBrewer::brewer.pal(n = 9, name = 'YlOrBr')

tmap_save(tm_shape(boundary) +
            tm_fill(col = '#a9491d', alpha = 1) + 
            tm_shape(remove_lands) +
            tm_fill(col = '#ffffcb', alpha = 1) +
            tm_shape(final_coverage) +
            tm_fill(col = '#a9491d', alpha = 1) +
            tm_layout(frame = FALSE),
          filename = "coverage_nj_redux.png")
  
## dummies
c <- transmute(coverage, ordinance = 1)
p <- transmute(pinelands, pinelands = 1)
h <- transmute(highlands, highlands = 1)
s <- transmute(sewer, sewer = 1)
l <- transmute(pad, protected = 1)

binaries <- 
  blocks %>%
  st_join(st_transform(c, st_crs(blocks))) %>%
  group_by(GEOID) %>% 
  slice(1) %>%
  ungroup() %>% 
  st_join(st_transform(p, st_crs(blocks))) %>%
  group_by(GEOID) %>% 
  slice(1) %>%
  ungroup() %>%
  st_join(st_transform(h, st_crs(blocks))) %>%
  group_by(GEOID) %>% 
  slice(1) %>% 
  ungroup() %>% 
  st_join(st_transform(s, st_crs(blocks))) %>%
  group_by(GEOID) %>% 
  slice(1) %>% 
  ungroup() %>%
  st_join(st_transform(l, st_crs(blocks))) %>%
  group_by(GEOID) %>% 
  slice(1) %>% 
  ungroup() %>%
  replace_na(list(sewer = 0, highlands = 0, pinelands = 0, ordinance = 0, protected = 0)) %>% 
  glimpse()

binaries %>% 
  select(sewer, highlands, pinelands, ordinance, protected) %>%
  plot()

binaries %>% 
  select(sewer, highlands, pinelands, ordinance, protected) %>% 
  pivot_longer(!geometry) %>% 
  st_as_sf()

tmap_save(
  binaries %>% 
    select(sewer, highlands, pinelands, ordinance, protected) %>% 
    pivot_longer(!geometry) %>% 
    st_as_sf() %>%
    tm_shape() +
    tm_fill("value") +
    tm_facets("name") +
    tm_layout(frame = FALSE),
  filename = "classifications.png", height = 10
)

st_write(binaries, "dummies.gpkg")

c <- 
  blocks %>% 
  st_difference(coverage) %>%
  transmute(GEOID,
            area_coverage = st_area(geometry)) %>%
  st_drop_geometry()

l <- 
  blocks %>% 
  st_difference(protected) %>%
  transmute(GEOID,
            area_protected = st_area(geometry)) %>%
  st_drop_geometry()
  
p <- 
  blocks %>% 
  st_difference(pinelands) %>%
  transmute(GEOID,
            area_pinelands = st_area(geometry)) %>%
  st_drop_geometry()

h <- 
  blocks %>% 
  st_difference(highlands) %>%
  transmute(GEOID,
            area_highlands = st_area(geometry)) %>%
  st_drop_geometry()

s <- 
  blocks %>% 
  st_difference(connected) %>%
  transmute(GEOID,
            area_connected = st_area(geometry)) %>%
  st_drop_geometry()

cover <- 
  blocks %>%
  mutate(area = st_area(geometry)) %>%
  left_join(c) %>%
  left_join(l) %>%
  left_join(p) %>%
  left_join(h) %>%
  left_join(s) %>%
  mutate(area_coverage = (1 - units::drop_units(area_coverage / area)) * 100,
         area_protected = (1 - units::drop_units(area_protected / area)) * 100,
         area_pinelands = (1 - units::drop_units(area_pinelands / area)) * 100,
         area_highlands = (1 - units::drop_units(area_highlands / area)) * 100,
         area_connected = (1 - units::drop_units(area_connected / area)) * 100) %>%
  glimpse() %>% 
  mutate(area = units::drop_units(units::set_units(area, km^2))) %>%
  rename(area_km2 = area,
         area_ordinance = area_coverage) %>% 
  mutate_if(is.numeric, function(x){ case_when(x < 0 ~ 0,
                                               is.na(x) ~ 100,
                                               TRUE ~ x) }) %>%
  select(everything(), geometry)

st_write(cover, "nj_coverages.gpkg")

tmap_save(
  cover %>% 
    mutate_if(is.numeric, function(x){ case_when(x < 0 ~ 0,
                                                 is.na(x) ~ 100,
                                                 TRUE ~ x) }) %>%
    rename(area_ordinance_examined = area_ordinance,
           area_protected_areas = area_protected,
           area_sewer_connection = area_connected) %>%
    select(area_ordinance_examined, area_protected_areas, area_pinelands, area_highlands, area_sewer_connection) %>%
    pivot_longer(!geometry, values_to = "percent covered") %>% 
    mutate(name = str_to_title(str_replace_all(name, pattern = "area_|_", replacement = " "))) %>% 
    mutate(name = str_sub(name, 2, -1)) %>% 
    mutate(name = factor(name, levels = c("Ordinance Examined", "Highlands", "Pinelands", "Sewer Connection", "Protected Areas"))) %>% 
    st_as_sf() %>%
    tm_shape() +
    tm_fill("percent covered") +
    tm_facets("name") +
    tm_layout(frame = FALSE,
              panel.label.color = '#ffffff',
              panel.label.bg.color = '#4a4a4a', 
              panel.label.size = 3,
              legend.show = FALSE),
  filename = "classifications.png", height = 10, dpi = 300
)

tmap_save(
  cover %>% 
    mutate_if(is.numeric, function(x){ case_when(x < 0 ~ 0,
                                                 is.na(x) ~ 100,
                                                 TRUE ~ x) }) %>%
    rename(area_ordinance_examined = area_ordinance,
           area_protected_areas = area_protected,
           area_sewer_connection = area_connected) %>%
    select(area_ordinance_examined, area_protected_areas, area_pinelands, area_highlands, area_sewer_connection) %>%
    pivot_longer(!geometry, values_to = "percent covered") %>% 
    mutate(name = str_to_title(str_replace_all(name, pattern = "area_|_", replacement = " "))) %>% 
    mutate(name = str_sub(name, 2, -1)) %>% 
    mutate(name = factor(name, levels = c("Ordinance Examined", "Highlands", "Pinelands", "Sewer Connection", "Protected Areas"))) %>% 
    st_as_sf() %>%
    tm_shape() +
    tm_fill("percent covered") +
    tm_facets("name") +
    tm_layout(frame = FALSE,
              panel.label.color = '#ffffff',
              panel.label.bg.color = '#4a4a4a', 
              legend.only = TRUE),
  filename = "classifications_legend.png", height = 10, dpi = 300
)



