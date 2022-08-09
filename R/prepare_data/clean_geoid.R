library(tidyverse)

changed_path = "data/pop_change/change_tracts_geoid.csv"

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
    read_delim(changed_path) %>%
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