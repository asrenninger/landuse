library(tidyverse)
library(tidycensus)
library(glue)

################# PATHS

GITHUB_DIR = "C:/Users/nelms/OneDrive - PennO365/Penn/Wharton/NLURI/landuse_PROD"
setwd(GITHUB_DIR)
tracts_path = "data/pop_change/tracts_pop_change.csv"
final_path = "data/pop_change/bg_pop_change.csv"

# get clean geoid functions
clean_geoid_path = "R/prepare_data/clean_geoid.R"
source(clean_geoid_path)

census_key_path = "C:/Users/nelms/Documents/Code/keys/census_api_key.txt"

################# GET BLOCK GROUP GEOIDS

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
bg_df =
    bg_df %>%
    mutate(
        GEOID = GEOID %>%
            format_geoid_col(., 12),
        trtid10 = GEOID %>%
            sapply(., function(t) substr(t, 1, 11)) %>%
            format_geoid_col(., 11) %>%
            fix_geoid(.)
    )



################# IMPORT COMP TRACTS

trct_df =
    read_delim(tracts_path)

## MEAN LENGTH OF GEOID/TRTID10
trct_df$trtid10 %>% sapply(., function(t) nchar(t)) %>% mean()
bg_df$trtid10 %>% sapply(., function(t) nchar(t)) %>% mean()


tr_ids = trct_df$trtid10 %>% unique()
bg_ids = bg_df$trtid10 %>% unique()
## AMOUNT OF BLOCK GROUP IDS NOT IN TRACT IDS
bg_ids[!(bg_ids %in% tr_ids)] %>% length() %>% print()
## AMOUNT OF BLOCK GROUP IDS IN TRACT IDS
### SHOULD BE MISSING 1 WATER TRACT
tr_ids[!(tr_ids %in% bg_ids)] %>% length() %>% print()

############# CUT

cols = c("pct90to19", "pct00to19", "pct10to19")

tdf = trct_df %>%
    select(cols) %>%
    filter_all(all_vars(!is.infinite(.)),all_vars(!is.na(.)))
tstat =
    rbind(
        apply(tdf, 2, max, na.rm=TRUE),
        apply(tdf, 2, min, na.rm=TRUE),
        apply(tdf, 2, mean, na.rm=TRUE),
        apply(tdf, 2, median, na.rm=TRUE),
        apply(tdf, 2, sd, na.rm=TRUE)
    ) %>%
    t() %>%
    as.tibble() %>%
    setNames(., c('max','min','mean','median','std'))

bmin = min(tstat$min)*100
bmax = max(tstat$max)*100


bin1 = 10
bin2 = 20
bins_s = c(-bin2, -bin1, bin1, bin2)
bins_l = c(bmin, bins_s, bmax)

label_b = function(bins, minb)
    sapply(1:(length(bins)-1), function(n){
        b1 = bins[[n]]
        b2 = ifelse(
            bins[[n+1]]>abs(minb),
            glue('{abs(minb)}+'), bins[[n+1]])
        return(glue("{b1} to {b2}"))
    })
labels = label_b(bins_l, bmin)
cut_b = function(column, bins, dataframe = trct_df)
        cut(dataframe[[column]]*100, bins, labels=labels, include.lowest = TRUE)
cut_b = function(column, bins) cut(column*100, bins, labels=label_b(bins_l, bmin), include.lowest = TRUE)

trct_df =
    trct_df %>%
        mutate(across(
            cols,
            .fns = list(change = ~cut_b(.x, bins_l)),
            .names = "cut{substr(col,4,5)}to19" ) )


##### JOIN TO BG

join_df =
    left_join(
        bg_df %>%
            select(GEOID, trtid10),
        trct_df,
        on='trtid10')

join_df %>%
    write.csv(final_path)
