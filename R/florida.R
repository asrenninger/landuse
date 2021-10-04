## random maps
florida_prices <- 
  read_csv("~/Downloads/MedValuePerSqft_county.csv", col_types = cols("fips" = col_character())) %>% 
  filter(str_detect(monthlydate, "2018|2017")) %>% 
  mutate(GEOID = case_when(str_length(fips) < 5 ~ paste("0", fips, sep = ""),
                           TRUE ~ fips)) %>% 
  filter(str_sub(GEOID, 1, 2) == "12") %>%
  group_by(GEOID) %>% 
  summarise(`price_persf` = mean(MedValuePerSqft)) %>%
  ungroup()

florida_building <- 
  st_read("~/Downloads/FLCounties_unitsjoined/FLCounties_unitsjoined.shp") %>%
  transmute(GEOID = str_c("12", FIPS), 
            building = BU2010) %>%
  glimpse() %>%
  st_drop_geometry() 

florida_building %>%
  left_join(florida_prices) %>%
  ggplot(aes(x = building, y = price_persf)) +
  geom_smooth(method = lm, se = FALSE, colour = '#a4a4a4') +
  geom_point(colour = '#b34002') +
  labs(y = "price ($ per sf)", 
       x = "new construction (new builds, 2010)") +  
  theme_minimal() + 
  ggsave("pricexbuilding_fl.png", height = 6, width = 6, dpi = 300)

tmap_save(
  florida_building %>%
    left_join(counties(class = 'sf', cb = TRUE)) %>% 
    filter(STATEFP %in% unique(fips_codes$state_code)[1:51][-c(2, 12)]) %>% 
    st_as_sf() %>% 
    st_transform(2163) %>% 
    transmute(`new builds (2010)` = building) %>% 
    tm_shape() +
    tm_fill(col = "new builds (2010)", style = 'fisher') +
    tm_shape(united_states %>% 
               st_transform(2163)) +
    tm_borders(col = '#000000', lty = 2, lwd = 1) +
    tm_layout(frame = FALSE,
              legend.only = TRUE),
  filename = "building_fl_legend.png", height = 8)

tmap_save(
  read_csv("~/Downloads/MedValuePerSqft_county.csv", col_types = cols("fips" = col_character())) %>% 
    filter(str_detect(monthlydate, "2018|2017")) %>% 
    mutate(GEOID = case_when(str_length(fips) < 5 ~ paste("0", fips, sep = ""),
                             TRUE ~ fips)) %>% 
    filter(str_sub(GEOID, 1, 2) == "12") %>%
    group_by(GEOID) %>% 
    summarise(`price ($ per sf)` = mean(MedValuePerSqft)) %>%
    left_join(counties(class = 'sf', cb = TRUE)) %>% 
    filter(STATEFP %in%  unique(fips_codes$state_code)[1:51][-c(2, 12)]) %>% 
    st_as_sf() %>% 
    st_transform(2163) %>% 
    select(`price ($ per sf)`) %>% 
    tm_shape() +
    tm_fill(col = "price ($ per sf)", style = 'fisher') +
    tm_shape(united_states %>% 
               st_transform(2163)) +
    tm_borders(col = '#000000', lty = 2, lwd = 1) +
    tm_layout(frame = FALSE,
              legend.show = FALSE),
  filename = "price_fl.png", height = 8)

## septic
counties_fl <- 
  tigris::fips_codes %>% 
  filter(state == "FL") %>% 
  pull(county) %>% 
  str_remove_all(" County")

download.file(url = "http://ww10.doh.state.fl.us/pub/bos/Inventory/FloridaWaterManagementInventory/MiamiDade/miamidade-public.zip",
              destfile = "data/florida/Desoto.zip")

purrr::map(counties_fl[60:length(counties_fl)], 
           function(x) {
             download.file(url = glue::glue("http://ww10.doh.state.fl.us/pub/bos/Inventory/FloridaWaterManagementInventory/{x}/{str_to_lower(str_remove_all(x, pattern = ' '))}-public.zip"),
                           destfile = glue::glue("data/florida/{x}.zip"))}
)

purrr::map(fs::dir_ls("data/florida"), ~unzip(.x, exdir = "florida/extracted"))

tictoc::tic()
florida_sewage <-
  map_df(fs::dir_ls("florida/extracted", recurse = TRUE, regexp = ".shp$"),
         function(x) {
           st_read(x) %>%
             filter(WW != "NA" & WW != "UNK") %>%
             transmute(GEOID = str_c("12", COUNTYFP), 
                       connection = case_when(str_detect(as.character(WW), "Sewer") ~ "sewer",
                                              str_detect(as.character(WW), "Septic") ~ "septic",
                                              TRUE ~ "neither")) %>%
             drop_na() %>%
             st_drop_geometry() %>%
             group_by(GEOID, connection) %>%
             summarise(n = n()) %>%
             pivot_wider(id_cols = GEOID, names_from = connection, values_from = n)
         })

tictoc::toc()

volusia <- 
  st_read("florida/extracted/volusia-public/FLWMI_Volusia.shp") %>%
  filter(WW != "NA" & WW != "UNK") %>%
  transmute(GEOID = str_c("12", COUNTYFP), 
            connection = case_when(str_detect(as.character(WW), "Sewer") ~ "sewer",
                                   str_detect(as.character(WW), "Septic") ~ "septic",
                                   TRUE ~ "neither")) %>%
  drop_na() %>%
  st_drop_geometry() %>%
  group_by(GEOID, connection) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = GEOID, names_from = connection, values_from = n) %>%
  mutate(GEOID = "12127")

broward <- 
  st_read("florida/extracted/broward-public/FLWMI_Broward.shp") %>%
  filter(WW != "NA" & WW != "UNK") %>%
  transmute(GEOID = str_c("12", COUNTYFP), 
            connection = case_when(str_detect(as.character(WW), "Sewer") ~ "sewer",
                                   str_detect(as.character(WW), "Septic") ~ "septic",
                                   TRUE ~ "neither")) %>%
  drop_na() %>%
  st_drop_geometry() %>%
  group_by(GEOID, connection) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = GEOID, names_from = connection, values_from = n) %>%
  mutate(GEOID = "12011")

hillsborough <- 
  st_read("florida/extracted/hillsborough-public/FLWMI_Hillsborough.shp") %>%
  filter(WW != "NA" & WW != "UNK") %>%
  transmute(GEOID = str_c("12", COUNTYFP), 
            connection = case_when(str_detect(as.character(WW), "Sewer") ~ "sewer",
                                   str_detect(as.character(WW), "Septic") ~ "septic",
                                   TRUE ~ "neither")) %>%
  drop_na() %>%
  st_drop_geometry() %>%
  group_by(GEOID, connection) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = GEOID, names_from = connection, values_from = n)

desoto <- 
  st_read("florida/extracted/desoto-public/FLWMI_Desoto.shp") %>%
  filter(WW != "NA" & WW != "UNK") %>%
  transmute(GEOID = str_c("12", COUNTYFP), 
            connection = case_when(str_detect(as.character(WW), "Sewer") ~ "sewer",
                                   str_detect(as.character(WW), "Septic") ~ "septic",
                                   TRUE ~ "neither")) %>%
  drop_na() %>%
  st_drop_geometry() %>%
  group_by(GEOID, connection) %>%
  summarise(n = n()) %>%
  pivot_wider(id_cols = GEOID, names_from = connection, values_from = n)

sewage_xcounty <- 
  florida_sewage %>% 
  group_by(GEOID) %>% 
  slice(1) %>% 
  ungroup() %>%
  bind_rows(desoto, hillsborough, broward, volusia) %>%
  mutate(percent_septic = (septic / (septic + sewer)) * 100) %>%
  write_csv("sewagexcounty_florida.csv")

tmap_save(
  counties(state = "FL", class = 'sf', cb = TRUE) %>% 
    left_join(sewage_xcounty) %>%
    mutate(`percent septic` = (septic / (septic + sewer)) * 100) %>%
    st_as_sf() %>%  
    st_transform(2163) %>% 
    select(NAME, `percent septic`) %>%
    tm_shape() +
    tm_fill(col = "percent septic") +
    tm_shape(united_states %>% 
               st_transform(2163)) +
    tm_borders(col = '#000000', lty = 2, lwd = 1) + 
    tm_layout(frame = FALSE,
              legend.only = TRUE),
  filename = "septic_fl_legend.png", height = 8)

