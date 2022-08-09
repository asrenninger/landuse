library(tidyverse)
library(tidycensus)
library(glue)

################# PATHS

GITHUB_DIR = "C:/Users/nelms/OneDrive - PennO365/Penn/Wharton/NLURI/landuse_PROD"
setwd(GITHUB_DIR)
tracts_path = "data/pop_change/tracts_pop_change.csv"

# get clean geoid functions
clean_geoid_path = "R/prepare_data/clean_geoid.R"
source(clean_geoid_path)

################# GET BLOCK GROUP GEOIDS

census_key_path = "C:/Users/nelms/Documents/Code/keys/census_api_key.txt"

key = readChar(census_key_path, file.info(census_key_path)$size)
census_api_key(
    key,
    install = TRUE,
    overwrite = TRUE)

states = fips_codes %>%
    select(state, state_code, state_name) %>%
    unique() %>%
    filter(state_code < 60) %>%
    pull(state)
bg_df =
    tibble(
        GEOID = character(),
        bg.pop19 = numeric()
    )

for (state in states) {
    bg_df =
        bg_df %>%
        rbind(
            get_acs(
                geography = "block group",
                variables = "B01001_001",
                state = state,
                geometry = FALSE,
                year = 2019,
                cb = false,
                quiet = TRUE
            ) %>%
            transmute(
                GEOID = GEOID %>%
                    as.character(),
                bg.pop19 = estimate
            ))
}
cor_bg_df =
    bg_df %>%
        mutate(
            GEOID = GEOID %>%
                format_geoid_col(., 12),
            trtid10 = GEOID %>%
                sapply(., function(t) substr(t, 1, 11))
        )

################# IMPORT COMP TRACTS

trct_df =
    read_delim(tracts_path)

## MEAN LENGTH OF GEOID/TRTID10
trct_df$trtid10 %>% sapply(., function(t) nchar(t)) %>% mean()
bg_df$GEOID %>% sapply(., function(t) nchar(t)) %>% mean()

## TOTAL LENGTH OF UNIQUE FIRST 11-CHAR
tr_ids = trct_df$trtid10 %>% unique()
bg_ids = bg_df$trtid10 %>% unique()

bg_ids[!(bg_ids %in% tr_ids)]





fix_geoid(tr_ids)
