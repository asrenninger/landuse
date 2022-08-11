library(tidyverse)
library(tidycensus)
library(glue)

################## PATHS

# LTBD SOURCE
## https://s4.ad.brown.edu/projects/diversity/Researcher/Bridging.htm
## https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/DataList.aspx

# LTDB directory
GITHUB_DIR = "C:/Users/nelms/OneDrive - PennO365/Penn/Wharton/NLURI/landuse_PROD"
setwd(GITHUB_DIR)

POP_DIR = 'data/pop_change'
LTDB_DIR = paste0(POP_DIR, '/', 'raw_LTDB')

# get clean geoid functions
clean_geoid_path = "R/prepare_data/clean_geoid.R"
source(clean_geoid_path)

census_key_path = "C:/Users/nelms/Documents/Code/keys/census_api_key.txt"

census_path = paste(
    POP_DIR,
    'tracts_pop_change.csv', sep='/')
count_path = paste(
    POP_DIR,
    'tracts_pop_change_meta.csv', sep='/')

################## INPUTS

target_col = 'pop'

# potential tract id cols
id_cols = c('trtid10', 'tractid', 'trtid2010')
# main id col
id_col = 'trtid10'

years = c('1970', '1980', '1990', '2000', '2010', '2020')

################## READ CSVs TO DF LIST

df_l = list()

for (year in years) {
    yr = substr(year,3,4)
    path = glue('{LTDB_DIR}/LTDB_Std_{year}_fullcount.csv')
    focus_col = glue('{target_col}{yr}')

    year_df = read_delim(path)
    colnames(year_df) = unlist(lapply(colnames(year_df), function(c) tolower(c)))
    cols = colnames(year_df)

    cols = cols[(focus_col == cols)|(cols %in% id_cols)]
    year_df = year_df[,cols]
    cols = replace(cols, cols %in% id_cols, id_col)
    colnames(year_df) = cols

    year_df[[id_col]] =
        year_df[[id_col]] %>%
            format_geoid_col(., ideal_length = 11) %>%
            fix_geoid(.)

    df_l[[year]] = year_df
}

################## IMPORT 2019 ACS TRACTS

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
acs_df =
    tibble(
        trtid10 = character(),
        pop19 = numeric()
    )
for (state in states) {
    acs_df =
        acs_df %>%
        rbind(
            get_acs(
                geography = "tract",
                variables = "B01001A_001",
                state = state,
                geometry = FALSE,
                year = 2019,
                cb = FALSE,
                quiet = TRUE
            ) %>%
            transmute(
                trtid10 = GEOID %>%
                    format_geoid_col(., ideal_length = 11) %>%
                    fix_geoid(.),
                pop19 = estimate
            )) %>%
        mutate(state_fips =
            substr(trtid10, 1, 2) %>%
            as.numeric()) %>%
        filter(state_fips < 60) %>%
        select(-state_fips)
}

acs_df =
   acs_df %>%
       mutate(
        trtid10 = fix_geoid_col(trtid10, ideal_length=11)
        )

df_l[['2019']] = acs_df
years = c(years, '2019')

# GET STATS

year_10 = df_l[['2010']]
ids_10 = year_10$trtid10 %>% unique()
count_10 = length(ids_10)
count_l = list()
above_pop = 1000

for (year in years) {

    year_df = df_l[[year]]
    yr = substr(year,3,4)
    focus_col = glue('{target_col}{yr}')

    ids_focus = year_df$trtid10 %>% unique()
    ids_in = ids_focus[ids_focus %in% ids_10]
    count_focus = length(ids_in)

    ids_not_in = year_10[!(year_10$trtid10 %in% ids_in),]
    pop_col = ids_not_in[['pop10']]
    mean_pop = pop_col %>% mean(., na.rm=TRUE)
    sum_pop = pop_col %>% sum()
    count_above_100 = length(pop_col[pop_col>=above_pop])

    count_l[[year]] = c(
        count_focus,
        round((count_focus/count_10)*100,2),
        mean_pop,
        sum_pop,
        count_above_100)
}

count_df =
    count_l %>%
        as.tibble() %>%
        rownames_to_column %>%
        gather(variable, value, -rowname) %>%
        spread(rowname, value)

colnames(count_df) = c(
    'year', 'num_id_in_2010', 'pct_id_in_2010',
    'mean_pop_of_missing_id', 'sum_pop_of_missing_id', glue('num_id_with_pop_over_{above_pop}'))
count_df

# CALC

full_df = year_10[,] %>%
        mutate(state_fips =
            substr(trtid10, 1, 2) %>%
            as.numeric()) %>%
        filter(state_fips < 60) %>%
        select(-state_fips)

remove_years = c('2010', '2020', '1970', '1980')
for (year in years[!(years%in%remove_years)]) {

    year_df = df_l[[year]] %>%
        mutate(
            state_fips =
                substr(trtid10, 1, 2) %>%
                as.numeric()) %>%
        filter(state_fips < 60) %>%
        select(-state_fips)
    yr = substr(year,3,4)
    focus_col = glue('{target_col}{yr}')

    full_df =
        left_join(
            full_df,
            year_df,
            by = 'trtid10')

}

## MEAN LENGTH OF GEOID/TRTID10
full_df$trtid10 %>% sapply(., function(t) nchar(t)) %>% mean()

full_df$trtid10 %>% substr(., 1, 2) %>% unique() %>% sort()


# ADD CHANGES IN YEARS

ref_year = '19'
change_yrs = c('90', '00', '10')
year_func = function(yr) glue('{target_col}{yr}')
change_cols =
    change_yrs %>%
        sapply(., year_func) %>%
        unname()
col_list = c('trtid10', change_cols, year_func(ref_year))

pct_change = function(NEWcol, OGcol) {
    OGcol = ifelse(OGcol == 0, .1, OGcol)
    dif = NEWcol - OGcol
    return( dif / abs(OGcol) )
    }
full_df =
    full_df[,col_list] %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(across(
        change_cols,
        .fns = list(change = ~pop19 - .x),
        .names = "dif{substr(col,4,5)}to{ref_year}" ) ) %>%
    mutate(across(
        change_cols,
        .fns = list(change = ~pct_change(pop19, .x)),
        .names = "pct{substr(col,4,5)}to{ref_year}" ) )

full_df %>% write.table(.,
    file = census_path,
    sep=",", row.names=FALSE)

count_df %>% write.table(.,
    file = count_path,
    sep=",", row.names=FALSE)


##############
