## Zillow
library(tidyverse)
library(sf)

price <- 
  read_csv("https://files.zillowstatic.com/research/public_csvs/zhvi/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv?t=1652370258") %>%
  janitor::clean_names() %>%
  glimpse()

zips <- 
  tigris::zctas(cb = TRUE, class = 'sf', year = 2019) %>%
  rmapshaper::ms_simplify(0.005)

box <-
  st_point(c(-74.005974, 40.712776)) %>% 
  st_sfc(crs = 4326) %>%
  st_transform(2163) %>%
  st_buffer(2 * 10^5) %>%
  st_bbox()

price %>%
  transmute(ZCTA5CE10 = region_name,
            `2021` = (x2021_12_31 - x2021_01_31) / x2021_01_31,
            `2020` = (x2020_12_31 - x2020_01_31) / x2020_01_31) %>% 
  pivot_longer(`2021`:`2020`) %>%
  left_join(zips) %>%
  filter(!str_detect(str_sub(ZCTA5CE10, 1, 3), "967|968|969"),
         !str_detect(str_sub(ZCTA5CE10, 1, 3), "995|996|997|998|999"),
         !str_detect(str_sub(ZCTA5CE10, 1, 3), "006|007")) %>%
  st_as_sf() %>%
  st_transform(2163) %>%
  ggplot() +
  geom_sf(aes(fill = factor(ntile(value, 9))), colour = NA, size = 0) +
  geom_sf(data = tigris::states(cb = TRUE, class = 'sf') %>%
            filter(STATEFP %in% unique(tigris::fips_codes$state_code)[1:51][-c(2, 12)]) %>%
            st_transform(2163),
          aes(),
          fill = NA, colour = '#7c7c7c', linetype = 2) +
  # coord_sf(xlim = c(box[[1]], box[[3]]), ylim = c(box[[2]], box[[4]])) +
  scale_fill_brewer(palette = 'Spectral', direction = -1, guide = 'none') +
  facet_wrap(~ name) +
  labs(title = "ZHVI annual change, January to December") +
  theme_map() +
  theme(plot.title = element_text(face = 'bold', size = 15, hjust = 0.5))

ggsave(filename = "ZHVI.png", height = 8, width = 16, dpi = 300)