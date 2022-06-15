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
  slice(1) %>%
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
# protections <- 
#   reduce(imap(pad, 
#               function(x, y){
#                 designation <- 
#                   x %>%
#                   st_union() %>% 
#                   st_combine()
#                 
#                 layer_label <- layers[y]
#                 
#                 tictoc::tic()
#                 designation_area <- 
#                   blocks %>%
#                   st_difference(designation) %>%
#                   mutate(area_protected = area_total - units::set_units(st_area(geometry), m^2),
#                          protection = layer_label) %>%
#                   st_drop_geometry() %>%
#                   select(-area_total)
#                 tictoc::toc()
#                 
#                 return(designation_area)
#                 
#               }),
#          rbind)

## ...for total protected areas
pad <- 
  pad %>% 
  reduce(rbind) %>%
  st_union() %>% 
  st_combine()

designation_area <- 
  blocks %>%
  st_difference(designation) %>%
  mutate(area_protected = area_total - units::set_units(st_area(geometry), m^2)) %>%
  st_drop_geometry()

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

protections %>%
  left_join(blocks) %>%
  mutate(area_total = units::drop_units(area_total), 
         area_protected = units::drop_units(area_protected),
         area_protected = round(area_protected, 4)) %>%
  pivot_wider(id_cols = GEOID:area_total,
              names_from = protection,
              values_from = area_protected) %>%
  mutate_at(vars(starts_with("PAD")), funs(case_when(is.na(.) ~ area_total, TRUE ~ .))) %>%
  select(-area_total) %>%
  select(GEOID, starts_with("PAD")) %>%
  pivot_longer(!GEOID) %>%
  mutate(name = str_remove_all(name, "PADUS2_1")) %>%
  left_join(blocks) %>%
  st_as_sf() %>%
  ggplot(aes(fill = value / (1000 * 1000))) +
  geom_sf(colour = '#ffffff', size = 0) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'Greens'),
                       name = expression(bold(area~(km^2)))) +
  facet_wrap(~name) + 
  theme_void() +
  theme(strip.text = element_text(face = 'bold', size = 15))

ggsave(plot = last_plot(), filename = "montana_protected_areas.png", height = 10, width = 12, dpi = 300)