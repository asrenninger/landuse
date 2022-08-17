###################################
## Helper Functions
###################################

## packages
library(tidyverse)
library(sf)
library(tigris)
library(crsuggest)
library(rgdal)

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

# DATAFRAME OF FIPS CODES
fips_df <-
      tigris::fips_codes %>%
      select(state, state_code, state_name) %>%
      unique() %>%
      filter(as.numeric(state_code)<60)

# STATE FIPS CODES
get_state_row = function(geography) fips_df %>%
    filter(state_name==geography|state_code==geography|state==geography)
## GET FIPS
get_fips = function(geography) get_state_row(geography) %>% pull(state_code)
## GET ABBREVIATED STATE NAME
get_state_nm = function(geography)  get_state_row(geography) %>% pull(state)
## GET FULL STATE NAME
get_state_name = function(geography)  get_state_row(geography) %>% pull(state_name)

# GET CRS
get_crs <-
  function(geography){
    geog_name <- get_state_name(geography)

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

# GET PROJ
if (exists("geography", inherits = FALSE)) {
  geog_proj <- get_crs(geography)
}

GITHUB_DIR = "https://raw.githubusercontent.com/asrenninger/landuse/main/"
changed_path = paste0(
    GITHUB_DIR,
    "data/pop_change/change_tracts_geoid.csv")


############# FORMAT GEOID COL

# add 0 if below 11 (or 12)
format_geoid = function(item, ideal_length=11) {
    item = item %>% as.numeric() %>% as.character()
    item = ifelse(
        nchar(item) < ideal_length,
        paste0("0", item),
        item
        )
    return(item)
}
format_geoid_col = function(col, ideal_length=11) sapply(col, function(item) format_geoid(item, ideal_length)) %>% unname()

############# FIXING GEOID CHANGES

# bring in GEOID changes
changed_ids =
    read_csv(changed_path) %>%
    transmute(
        old_geoid = format_geoid_col(`2010 GEOID`),
        new_geoid = format_geoid_col(`New GEOID`)
    )

# function to replace vector items with a old and new col
df_replace = function(vector, col_old, col_new)
    plyr::mapvalues(
        vector,
        from=col_old,
        to=col_new,
        warn_missing = FALSE)

# plug in geoid changes col
fix_geoid = function(
    vector,
    col_old=changed_ids$old_geoid,
    col_new=changed_ids$new_geoid) df_replace(vector, col_old, col_new)

fix_geoid_r = function(
    vector,
    col_old=changed_ids$old_geoid,
    col_new=changed_ids$new_geoid) df_replace(vector, col_new, col_old)