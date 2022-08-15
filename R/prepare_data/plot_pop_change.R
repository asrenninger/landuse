########## PLOTS
options(scipen=999)


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

bin_chi_l = list()
choice_l = list(
    b_1_10 = c(1,10),
    b_5_20 = c(5,20),
    b_10_25 = c(10,25)
)
chi_l = list()
fi_l = list()


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

cut_df =
    trct_df %>%
        mutate(across(
            cols,
            .fns = list(change = ~cut_b(.x, bins_l)),
            .names = "cut{substr(col,4,5)}to19" ) )
acols = colnames(cut_df)
ccols = acols[matches("cut", vars=acols)]

byears = sapply(cols, function(s) gsub('pct','',s)) %>% unname()
group_l = list()
for (yrs in byears) {
    ycols = acols[matches(yrs, vars=acols)] %>%
        setNames(., gsub(yrs,'',.))
    group_df = cut_df %>%
        group_by(across(ycols[['cut']])) %>%
        summarise(
            count = n(),
            pop_mean = mean(.data[['pop19']], na.rm=T),
            pop_tot = sum(.data[['pop19']]),
            #diff.mean = mean(.data[[ycols[['dif']]]], na.rm=T),
            #pct.mean = mean(.data[[ycols[['pct']]]], na.rm=T)
            )
    gcols = colnames(group_df)
    colnames(group_df) = sapply(gcols, function(col) paste0(col,yrs))

    group_l[[yrs]] = group_df
}

cbind(
    group_l[[1]],
    group_l[[2]],
    group_l[[3]]
) %>%
    write.csv("C:/Users/nelms/OneDrive - PennO365/Penn/Wharton/NLURI/test/stat.csv")

##### NJ
nj_path = "C:/Users/nelms/OneDrive - PennO365/Penn/Wharton/NLURI/data/regression_dataframe_nj_updated_220701.csv"
nj_df = read_delim(nj_path) %>%
    select(GEOID, maxhd_micro, maxhd_macro) %>%
    mutate(
        GEOID = format_geoid_col(GEOID, ideal_length=12),
        trtid10 = GEOID %>%
            sapply(., function(t) substr(t, 1, 11)) %>%
            format_geoid_col(., 11) %>%
            fix_geoid(.)
        )

##### PLOT

pct_hist = function(
    dataframe,
    column,
    #fill_col = "#69b3a2",
    low = -1000,
    high = 1000,
    bins = bins_s) {
    dataframe %>%
    filter(
        !(is.infinite(!!sym(column))),
        !(is.na(!!sym(column)))
        ) %>%
    mutate(!!column := get(column)*100) %>%
    filter(!!sym(column) >= low, !!sym(column) <= high) %>%
    ggplot(aes(x=.data[[column]])) +
        geom_density() +
        geom_histogram(
            #binwidth=3,
            #fill=fill_col,
            aes(fill=stat),
            color="#e9ecef", alpha=0.9) +
        labs(x="Rate of Change",
            y="Count",
            title="Population Change Rates",
            subtitle="Census Tracts") +
        geom_vline(xintercept = bins, 1, 'red') +
        theme(
            plot.title = element_text(size=15),
            legend.position="none"
        ) +
        facet_grid(rows = "stat")
    ggsave(save_hist, plot = last_plot())
}

cols = c("pct90to19", "pct00to19", "pct10to19")
fixn = function(s)
        gsub('pct', '', s) %>%
        gsub('to', ' to ', .)
fcols = sapply(cols, function(s) fixn(s))
save_hist = "C:/Users/nelms/OneDrive - PennO365/Penn/Wharton/NLURI/test/visuals/hist.jpg"
trct_df %>%
    select(trtid10, cols) %>%
    gather(stat, change, 2:4) %>%
    mutate(stat = factor(fixn(stat), levels = fcols)) %>%
    filter(change<2) %>%
    pct_hist(., "change")

# JOIN BG TO TRACTS



######## TEST

pop_cols = c("pop90", "pop00","pop10", "pop19")
nj_cut =
    left_join(
        nj_df,
        cut_df,
        by='trtid10'
    )
library(corrplot)
nj_cut %>%
    select(maxhd_micro, maxhd_macro, pop_cols) %>%
    cor(.)

iv = ccols[[1]]
dv = 'maxhd_macro'
#nj_cut %>%
#    select(iv, dv) %>%
#    chisq.test(table(.))
#fisher.test(table(race, schtyp)

######
nj.tab =
nj_cut[,c(dv, iv)] %>%
    table(., useNA='ifany')

row.names(nj.tab ) =
row.names(nj.tab ) %>%
replace_na(., 'NA')

colnames(nj.tab ) =
colnames(nj.tab ) %>%
replace_na(., 'NA')



#chi_l[[bl[[1]]]] = chisq.test(nj.tab)$p.value
#fi_l[[bl[[1]]]] = fisher.test(nj.tab, simulate.p.value=TRUE)$p.value


