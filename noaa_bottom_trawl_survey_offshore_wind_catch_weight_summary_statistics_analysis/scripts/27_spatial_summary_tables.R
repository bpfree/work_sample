###################################################
###################################################
###################################################


######### Part 27 #########
## Summary spatial reporting tables
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

### data directories
bts_geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
bts_non_owf_geopackage <- "data\\geopackage\\bts_non_owf_geopackage.gpkg"
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

figure_dir <- "figures\\charts\\spatial"
csv_dir <- "data\\f_csv_tables\\spatial_summary_tables"

### pre-determinants
begin_year <- 2009
end_year <- 2019

n_strata <- 5
n_farm <- 5

seasons <- c("Spring|Fall")

wind_farms_names <- c("") # c("") to get all wind farms / c("South|North") to get only wind farms with those in their names

map_theme <- theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.title.x = element_text(size = 10),
                   axis.text.x = element_text(size = 8),
                   axis.title.y = element_text(size = 10),
                   axis.text.y = element_text(size = 5),
                   plot.subtitle = element_text(size = 8),
                   plot.caption = element_text(size = 6,
                                               lineheight = 1),
                   legend.title = element_text(size = 8),
                   legend.text = element_text(size = 5))

###################################################
###################################################
###################################################

### load data
bts_spatial <- as.data.frame(st_read(dsn = bts_geopackage, layer = "bts_spatial"))
non_owf_spatial <- as.data.frame(st_read(dsn = bts_non_owf_geopackage, layer = "bts_non_owf_spatial"))
owf_spatial <- as.data.frame(st_read(dsn = wind_geopackage, layer = "owf_spatial"))

###################################################
###################################################
###################################################

### generating summary data
## BTS strata
bts_strata <- bts_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  dplyr::arrange(stratum, desc(count_summary))

## BTS strata and season
bts_strata_season <- bts_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  dplyr::arrange(stratum, desc(count_summary))

###################################################

## BTS top strata
# count
bts_strata_top_count <- bts_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_strata, order_by = count_summary)

# weight
bts_strata_top_weight <- bts_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_strata, order_by = weight_summary)

###################################################

## BTS top strata and season
# count
bts_strata_season_top_count <- bts_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_strata, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

# weight
bts_strata_season_top_weight <- bts_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(season) %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_strata, order_by = weight_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

###################################################
###################################################

## non-OWF strata
non_strata <- non_owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  dplyr::arrange(stratum, desc(count_summary))

## non-OWF strata and season
non_strata_season <- non_owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  dplyr::arrange(stratum, desc(count_summary))

###################################################

## non-OWF top strata
# count
non_strata_top_count <- non_owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_strata, order_by = count_summary)

# weight
non_strata_top_weight <- non_owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_strata, order_by = weight_summary)

###################################################

## non-OWF top strata and season
# count
non_strata_season_top_count <- non_owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_strata, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

# weight
non_strata_season_top_weight <- non_owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(season) %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_strata, order_by = weight_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

###################################################
###################################################

## OWF strata
owf_strata <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  dplyr::arrange(stratum, desc(count_summary))

## OWF strata and season
owf_strata_season <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  dplyr::arrange(stratum, desc(count_summary))

###################################################

## OWF top strata
# count
owf_strata_top_count <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_strata, order_by = count_summary)

# weight
owf_strata_top_weight <- owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by strata
  dplyr::group_by(stratum) %>%
  # calculate aggregate count and weight for the strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_strata, order_by = weight_summary)

###################################################

## OWF top strata and season
# count
owf_strata_season_top_count <- owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_strata, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

# weight
owf_strata_season_top_weight <- owf_spatial %>%
  # subset by seasons of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by strata and season
  dplyr::group_by(stratum,
                  season) %>%
  # calculate aggregate count and weight for the strata and season combination
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(season) %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_strata, order_by = weight_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(weight_summary))

###################################################
###################################################

## Wind farm
# strata combined
farm_agg <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm
  dplyr::group_by(name) %>%
  # calculate aggregate count and weight for the wind farm
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  dplyr::arrange(desc(count_summary))

# strata contribution
farm_strata <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm and strata
  dplyr::group_by(name,
                  stratum) %>%
  # calculate aggregate count and weight for the wind farm and strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  dplyr::arrange(desc(count_summary))

# strata contribution by season
farm_strata_season <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm and strata and season
  dplyr::group_by(name,
                  stratum,
                  season) %>%
  # calculate aggregate count and weight for the wind farm and strata and season
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  dplyr::arrange(desc(count_summary))

###################################################

## Top wind farm aggregation
# count
farm_agg_top_count <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm
  dplyr::group_by(name) %>%
  # calculate aggregate count and weight for the wind farm
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  dplyr::slice_max(n = n_farm, order_by = count_summary) %>%
  dplyr::arrange(desc(count_summary))

# weight
farm_agg_top_weight <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm
  dplyr::group_by(name) %>%
  # calculate aggregate count and weight for the wind farm
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  dplyr::slice_max(n = n_farm, order_by = weight_summary) %>%
  dplyr::arrange(desc(weight_summary))


## Top wind farm contribution by strata
# count
farm_strata_top_count <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm and strata
  dplyr::group_by(name,
                  stratum) %>%
  # calculate aggregate count and weight for the wind farm and strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(stratum) %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_farm, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(stratum, desc(count_summary))

# weight
farm_strata_top_weight <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm and strata
  dplyr::group_by(name,
                  stratum) %>%
  # calculate aggregate weight and weight for the wind farm and strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(stratum) %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_farm, order_by = weight_summary) %>%
  # view data by largest to smallest weights by season
  dplyr::arrange(stratum, desc(weight_summary))


## Top wind farm contribution by season
# count
farm_season_top_count <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm and strata
  dplyr::group_by(name,
                  season) %>%
  # calculate aggregate count and weight for the wind farm and strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_farm, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

# weight
farm_season_top_weight <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm and strata
  dplyr::group_by(name,
                  season) %>%
  # calculate aggregate weight and weight for the wind farm and strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_farm, order_by = weight_summary) %>%
  # view data by largest to smallest weights by season
  dplyr::arrange(season, desc(weight_summary))

## Top wind farm contribution by strata and season
# count
farm_strata_season_top_count <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm and strata
  dplyr::group_by(name,
                  stratum,
                  season) %>%
  # calculate aggregate count and weight for the wind farm and strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(stratum,
                  season) %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_farm, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(stratum, season, desc(count_summary))

# weight
farm_strata_season_top_weight <- owf_spatial %>%
  # subset by seasons and wind farms of interest
  dplyr::filter(str_detect(season, seasons),
                str_detect(name, wind_farms_names)) %>%
  # group by individual wind farm and strata
  dplyr::group_by(name,
                  stratum,
                  season) %>%
  # calculate aggregate weight and weight for the wind farm and strata
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # group by season
  dplyr::group_by(stratum,
                  season) %>%
  # add new field that designates if a strata is offshore (01XXX) or inshore (03XXX)
  dplyr::mutate(facet = if_else(str_detect(stratum, "^01"), "Offshore (01XXX)", "Inshore (03XXX)")) %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_farm, order_by = weight_summary) %>%
  # view data by largest to smallest weights by season
  dplyr::arrange(stratum, season, desc(weight_summary))

###################################################
###################################################
###################################################

### figures
## BTS
# all strata counts
bts_stratum_count <- ggplot() +
  geom_bar(data = bts_strata, aes(x = count_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Strata",
       title = "Strata counts for all BTS tows",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none")

# all strata weights
bts_stratum_weight <- ggplot() +
  geom_bar(data = bts_strata, aes(x = weight_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Strata",
       title = "Strata weights for all BTS tows",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none")

# all strata and season counts
bts_stratum_season_count <- ggplot() +
  geom_bar(data = bts_strata_season, aes(x = count_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = "Strata counts by season for all BTS tows",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme

# all strata and season weights
bts_stratum_season_weight <- ggplot() +
  geom_bar(data = bts_strata_season, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = "Strata weights by season for all BTS tows",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme

# all strata and season counts faceted by offshore vs. inshore
bts_stratum_season_facet_count <- ggplot() +
  geom_bar(data = bts_strata_season, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = "Strata counts (Inshore and Offshore) by season for all BTS tows",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# all strata and season weights faceted by offshore vs. inshore
bts_stratum_season_facet_weight <- ggplot() +
  geom_bar(data = bts_strata_season, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = "Strata weights (Inshore and Offshore) by season for all BTS tows",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

###################################################

# top strata counts
bts_stratum_top_count <- ggplot() +
  geom_bar(data = bts_strata_top_count, aes(x = count_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Strata",
       title = paste("Top", n_strata, "strata counts for all BTS tows"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata weights
bts_stratum_top_weight <- ggplot() +
  geom_bar(data = bts_strata_top_weight, aes(x = weight_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Strata",
       title = paste("Top", n_strata, "strata weights for all BTS tows"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season counts
bts_stratum_season_top_count <- ggplot() +
  geom_bar(data = bts_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top", n_strata, "strata counts per season for all BTS tows"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season weights
bts_stratum_season_top_weight <- ggplot() +
  geom_bar(data = bts_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season for all BTS tows"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season counts faceted by offshore vs. inshore
bts_stratum_season_facet_strata_top_count <- ggplot() +
  geom_bar(data = bts_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season (Inshore and Offshore) for all BTS tows"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season weights faceted by offshore vs. inshore
bts_stratum_season_facet_strata_top_weight <- ggplot() +
  geom_bar(data = bts_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season (Inshore and Offshore) for all BTS tows"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season counts faceted by fall vs. spring
bts_stratum_season_facet_season_top_count <- ggplot() +
  geom_bar(data = bts_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season (Fall and Spring) for all BTS tows"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                     hjust = 0.5,
                                     size = 8)) +
  ggforce::facet_wrap_paginate(~ season,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season weights faceted by fall vs. spring
bts_stratum_season_facet_season_top_weight <- ggplot() +
  geom_bar(data = bts_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season (Fall and Spring) for all BTS tows"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized counts between\n",
                       begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore (01XXX) strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ season,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

###################################################
###################################################

## Non-OWF
# all strata counts
non_stratum_count <- ggplot() +
  geom_bar(data = non_strata, aes(x = count_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Strata",
       title = "Strata counts outside offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none")

# all strata weights
non_stratum_weight <- ggplot() +
  geom_bar(data = non_strata, aes(x = weight_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Strata",
       title = "Strata weights outside offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none")

# all strata and season counts
non_stratum_season_count <- ggplot() +
  geom_bar(data = non_strata_season, aes(x = count_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = "Strata counts by season outside offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme

# all strata and season weights
non_stratum_season_weight <- ggplot() +
  geom_bar(data = non_strata_season, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = "Strata weights by season outside offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme

# all strata and season counts faceted by offshore vs. inshore
non_stratum_season_facet_count <- ggplot() +
  geom_bar(data = non_strata_season, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = "Strata counts (Inshore and Offshore) by season outside offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# all strata and season weights faceted by offshore vs. inshore
non_stratum_season_facet_weight <- ggplot() +
  geom_bar(data = non_strata_season, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = "Strata weights (Inshore and Offshore) by season outside offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

###################################################

# top strata counts
non_stratum_top_count <- ggplot() +
  geom_bar(data = non_strata_top_count, aes(x = count_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Strata",
       title = paste("Top", n_strata, "strata counts outside offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata weights
non_stratum_top_weight <- ggplot() +
  geom_bar(data = non_strata_top_weight, aes(x = weight_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Strata",
       title = paste("Top", n_strata, "strata weights outside offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season counts
non_stratum_season_top_count <- ggplot() +
  geom_bar(data = non_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top", n_strata, "strata counts per season outside offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season weights
non_stratum_season_top_weight <- ggplot() +
  geom_bar(data = non_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season outside offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season counts faceted by offshore vs. inshore
non_stratum_season_facet_strata_top_count <- ggplot() +
  geom_bar(data = non_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata (Inshore and Offshore) weights per season outside offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season weights faceted by offshore vs. inshore
non_stratum_season_facet_strata_top_weight <- ggplot() +
  geom_bar(data = non_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata (Inshore and Offshore) weights by season outside offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season counts faceted by fall vs. spring
non_stratum_season_facet_season_top_count <- ggplot() +
  geom_bar(data = non_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season (Fall and Spring) outside offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ season,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season weights faceted by fall vs. spring
non_stratum_season_facet_season_top_weight <- ggplot() +
  geom_bar(data = non_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season (Fall and Spring) outside offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ season,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

###################################################
###################################################

## OWF
# all strata counts
owf_stratum_count <- ggplot() +
  geom_bar(data = owf_strata, aes(x = count_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Strata",
       title = "Strata counts within offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# all strata weights
owf_stratum_weight <- ggplot() +
  geom_bar(data = owf_strata, aes(x = weight_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Strata",
       title = "Strata weights within offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# all strata and season counts
owf_stratum_season_count <- ggplot() +
  geom_bar(data = owf_strata_season, aes(x = count_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = "Strata counts by season within offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# all strata and season weights
owf_stratum_season_weight <- ggplot() +
  geom_bar(data = owf_strata_season, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = "Strata weights by season within offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# all strata and season counts faceted by offshore vs. inshore
owf_stratum_season_facet_count <- ggplot() +
  geom_bar(data = owf_strata_season, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = "Strata counts (Inshore and Offshore) by season within offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# all strata and season weights faceted by offshore vs. inshore
owf_stratum_season_facet_weight <- ggplot() +
  geom_bar(data = owf_strata_season, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = "Strata weights (Inshore and Offshore) by season within offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

###################################################

# top strata counts
owf_stratum_top_count <- ggplot() +
  geom_bar(data = owf_strata_top_count, aes(x = count_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Strata",
       title = paste("Top", n_strata, "strata counts within offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata weights
owf_stratum_top_weight <- ggplot() +
  geom_bar(data = owf_strata_top_weight, aes(x = weight_summary, y = stratum, fill = stratum), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Strata",
       title = paste("Top", n_strata, "strata weights within offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season counts
owf_stratum_season_top_count <- ggplot() +
  geom_bar(data = owf_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top", n_strata, "strata counts by season within offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season weights
owf_stratum_season_top_weight <- ggplot() +
  geom_bar(data = owf_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights by season within offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top strata and season counts faceted by offshore vs. inshore
owf_stratum_season_facet_strata_top_count <- ggplot() +
  geom_bar(data = owf_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata (Inshore and Offshore) weights per season within offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season weights faceted by offshore vs. inshore
owf_stratum_season_facet_strata_top_weight <- ggplot() +
  geom_bar(data = owf_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata (Inshore and Offshore) weights per season within offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season counts faceted by fall vs. spring
owf_stratum_season_facet_season_top_count <- ggplot() +
  geom_bar(data = owf_strata_season_top_count, aes(x = count_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per season (Fall and Spring) within offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ season,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

# top strata and season weights faceted by fall vs. spring
owf_stratum_season_facet_season_top_weight <- ggplot() +
  geom_bar(data = owf_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Season",
       title = paste("Top",n_strata, "strata weights per by season (Fall and Spring) within offshore wind farm areas"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  ggforce::facet_wrap_paginate(~ season,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1)

###################################################
###################################################

## Wind Farm
# offshore wind farm counts
farm_agg_count <- ggplot() +
  geom_bar(data = farm_agg_top_count, aes(x = count_summary, y = name, fill = name), stat = "identity") +
  labs(x = "Count",
       y = "Offshore Wind Farm",
       title = paste("Top", n_farm, "counts by offshore wind farm area"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values were calculated by aggregating normalized counts\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# offshore wind farm weights
farm_agg_weight <- ggplot() +
  geom_bar(data = farm_agg_top_weight, aes(x = weight_summary, y = name, fill = name), stat = "identity") +
  labs(x = "Weight",
       y = "Offshore Wind Farm",
       title = paste("Top", n_farm, "weights by offshore wind farm area"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values were calculated by aggregating normalized weights\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 1200, 200)) +
  theme_bw() + 
  map_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

###################################################

# offshore wind farm strata counts by wind area
farm_agg_strata_area_count <- ggplot() +
  geom_bar(data = farm_strata, aes(x = count_summary, y = stratum, fill = name), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Wind Farm Area",
       title = "Strata counts by offshore wind farm area",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by aggregating normalized counts\n
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() +
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  guides(fill = guide_legend(ncol = 1))

# offshore wind farm strata weights by wind area
farm_agg_strata_area_weight <- ggplot() +
  geom_bar(data = farm_strata, aes(x = weight_summary, y = stratum, fill = name), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Wind Farm Area",
       title = "Strata weights by offshore wind farm area",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by aggregating normalized weights\n
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() +
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  guides(fill = guide_legend(ncol = 1))

###################################################

# offshore wind farm strata and season counts
farm_agg_strata_season_count <- ggplot() +
  geom_bar(data = farm_strata_season, aes(x = count_summary, y = stratum, fill = stratum, alpha = season),  stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       alpha = "Season",
       title = "Strata counts by season within offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by aggregating normalized counts\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  guides(fill = FALSE)

# offshore wind farm strata and season weights
farm_agg_strata_season_weight <- ggplot() +
  geom_bar(data = farm_strata_season, aes(x = weight_summary, y = stratum, fill = stratum, alpha = season),  stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       alpha = "Season",
       title = "Strata weights by season within offshore wind farm areas",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by aggregating normalized weights\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  guides(fill = FALSE)

###################################################

# offshore wind farm area and season counts
farm_agg_area_season_count <- ggplot() +
  geom_bar(data = farm_strata_season, aes(x = count_summary, y = name, fill = name, alpha = season),  stat = "identity") +
  labs(x = "Count",
       y = "Wind Farm Area",
       alpha = "Season",
       title = "Offshore wind farm counts by season",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values were calculated by aggregating normalized counts\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = FALSE)

# offshore wind farm area and season weights
farm_agg_area_season_weight <- ggplot() +
  geom_bar(data = farm_strata_season, aes(x = weight_summary, y = name, fill = name, alpha = season),  stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Wind Farm Area",
       alpha = "Season",
       title = "Offshore wind farm weights by season",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values were calculated by aggregating normalized weights\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(size = 8)) +
  guides(fill = FALSE)

###################################################

# offshore wind farm area with strata and season counts
farm_agg_area_strata_season_count <- ggplot() +
  geom_bar(data = farm_strata_season, aes(x = count_summary, y = name, fill = stratum, alpha = season),  stat = "identity") +
  labs(x = "Count",
       y = "Wind Farm Area",
       alpha = "Season",
       fill = "Stratum",
       title = "Wind Farm counts by stratum and season",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values were calculated by aggregating normalized counts\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(size = 8))

# offshore wind farm area with strata and season weights
farm_agg_area_strata_season_weight <- ggplot() +
  geom_bar(data = farm_strata_season, aes(x = weight_summary, y = name, fill = stratum, alpha = season),  stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Wind Farm Area",
       alpha = "Season",
       fill = "Stratum",
       title = "Wind Farm counts by stratum and season",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values were calculated by aggregating normalized weights\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 1200, 200)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(size = 8))

###################################################

# offshore wind farm strata by wind farm area contribution and season
farm_strata_area_season_count <- ggplot() +
  geom_bar(data = farm_strata_season, aes(x = count_summary, y = stratum, fill = name, alpha = season),  stat = "identity") +
  labs(x = "Count",
       y = "Stratum",
       alpha = "Season",
       fill = "Wind Farm Area",
       title = "Strata counts by offshore wind farm area and season",
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values were calculated by aggregating normalized counts\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  guides(fill = guide_legend(ncol = 1))

# offshore wind farm area with strata and season weights
farm_strata_area_season_weight <- ggplot() +
  geom_bar(data = farm_strata_season, aes(x = weight_summary, y = stratum, fill = name, alpha = season),  stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Stratum",
       alpha = "Season",
       fill = "Wind Farm Area",
       title = "Strata weights by offshore wind farm area and season",
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values were calculated by aggregating normalized weights\n 
                       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX)\n 
                       and offshore (01XXX) strata of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 250)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  guides(fill = guide_legend(ncol = 1))

###################################################

# top offshore wind farm areas by count
# farm_top_agg_count <- ggplot() +
#   geom_bar(data = farm_agg_top_count, aes(x = count_summary, y = name, fill = name), stat = "identity") +
#   labs(x = "Count",
#        y = "Wind Farm Area",
#        fill = "Wind Farm Area",
#        title = paste("Top", n_farm, "counts by offshore wind farm area"),
#        subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
#        caption = paste("Count values for strata were calculated by aggregating normalized counts\n
#        between", begin_year, "and", end_year, "in all of the northern inshore (03XXX) and offshore\n
#        (01XXX) strata with offshore wind farm areas.")) +
#   # change limits
#   scale_x_continuous(breaks = seq(0, 25000, 2500)) +
#   theme_bw() + 
#   map_theme +
#   theme(legend.position = "none",
#         axis.text.y = element_text(angle = 90,
#                                    hjust = 0.5,
#                                    size = 8))
# 
# # top offshore wind farm areas by weight
# farm_top_agg_weight <- ggplot() +
#   geom_bar(data = farm_agg_top_weight, aes(x = weight_summary, y = name, fill = name), stat = "identity") +
#   labs(x = "Weight (kg)",
#        y = "Wind Farm Area",
#        fill = "Wind Farm Area",
#        title = paste("Top", n_farm, "weights by offshore wind farm area"),
#        subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
#        caption = paste("Weight values for strata were calculated by aggregating normalized weights\n
#        between", begin_year, "and", end_year, "in all of the northern inshore (03XXX) and offshore\n
#        (01XXX) strata with offshore wind farm areas.")) +
#   # change limits
#   scale_x_continuous(breaks = seq(0, 1400, 200)) +
#   theme_bw() + 
#   map_theme +
#   theme(legend.position = "none",
#         axis.text.y = element_text(angle = 90,
#                                    hjust = 0.5,
#                                    size = 8))

###################################################

# top offshore wind farm strata by count
farm_top_agg_strata_count <- ggplot() +
  geom_bar(data = farm_strata_top_count, aes(x = count_summary, y = stratum, fill = name), stat = "identity") +
  labs(x = "Count",
       y = "Strata",
       fill = "Wind Farm Area",
       title = paste("Top", n_farm, "offshore wind farms contributing to strata counts"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by aggregating normalized counts\n
       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata with offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 25000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  guides(fill = guide_legend(ncol = 1))

# top offshore wind farm strata by weight
farm_top_agg_strata_weight <- ggplot() +
  geom_bar(data = farm_strata_top_weight, aes(x = weight_summary, y = stratum, fill = name), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Wind Farm Area",
       title = paste("Top", n_farm, "offshore wind farms contributing to strata weights"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by aggregating normalized weights\n
       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata with offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 1800, 200)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8)) +
  guides(fill = guide_legend(ncol = 1))

###################################################

# top offshore wind farm season by count
farm_top_agg_season_count <- ggplot() +
  geom_bar(data = farm_season_top_count, aes(x = count_summary, y = season, fill = name), stat = "identity") +
  labs(x = "Count",
       y = "Season",
       fill = "Wind Farm Area",
       title = paste("Top", n_farm, "offshore farms contributing to season counts"),
       subtitle = paste("Representative bottom trawl survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by aggregating normalized counts\n
       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata with offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 50000, 2500)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))

# top offshore wind farm season by weight
farm_top_agg_season_weight <- ggplot() +
  geom_bar(data = farm_season_top_weight, aes(x = weight_summary, y = season, fill = name), stat = "identity") +
  labs(x = "Weight (kg)",
       y = "Season",
       fill = "Wind Farm Area",
       title = paste("Top", n_farm, "offshore farms contributing to season weights"),
       subtitle = paste("Representative bottom trawl survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by aggregating normalized weights\n
       between", begin_year, "and", end_year, "in all of the northern inshore (03XXX) and offshore\n
       (01XXX) strata with offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 3000, 200)) +
  theme_bw() + 
  map_theme +
  theme(axis.text.y = element_text(angle = 90,
                                   hjust = 0.5,
                                   size = 8))


###################################################


# all strata and season counts faceted by offshore vs. inshore
farm_top_agg_strata_season_area_facet_count <- ggplot() +
  geom_bar(data = farm_strata_season_top_count, aes(x = count_summary, y = stratum, fill = name, alpha = season), stat = "identity") + 
  labs(x = "Count",
       y = "Strata",
       fill = "Wind Farm Area",
       alpha = "Season",
       title = paste("Top", n_farm, "offshore wind farms contributing to counts by strata in each season"),
       caption = paste("Count values for strata were calculated by summing normalized counts
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore
       (01XXX) strata with offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 23000, 1000)) +
  theme_bw() + 
  map_theme +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1) +
  guides(fill = guide_legend(ncol = 1))

# all strata and season weights faceted by offshore vs. inshore
farm_top_agg_strata_season_area_facet_weight <- ggplot() +
  geom_bar(data = farm_strata_season_top_weight, aes(x = weight_summary, y = stratum, fill = name, alpha = season), stat = "identity") + 
  labs(x = "Weight (kg)",
       y = "Strata",
       fill = "Wind Farm Area",
       alpha = "Season",
       title = paste("Top", n_farm, "offshore wind farm contributing to weights by strata in each season"),
       caption = paste("Weight values for strata were calculated by summing normalized weights
       between", begin_year, "and", end_year, "across all of the northern inshore (03XXX) and offshore
       (01XXX) strata with offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(0, 2000, 100)) +
  theme_bw() + 
  map_theme +
  ggforce::facet_wrap_paginate(~ facet,
                               # have y-axis scale dependent on inshore vs. offshore
                               scales = "free",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1) +
  guides(fill = guide_legend(ncol = 1))


###################################################
###################################################
###################################################

### export data
## CSV
# Bottom trawl survey
write.csv(bts_strata, file.path(csv_dir, paste("bts_strata.csv", sep = "")))
write.csv(bts_strata_season, file.path(csv_dir, paste("bts_strata_season.csv", sep = "")))

write.csv(bts_strata_top_count, file.path(csv_dir, paste("bts_strata_top",n_strata,"_count.csv", sep = "")))
write.csv(bts_strata_top_weight, file.path(csv_dir, paste("bts_strata_top",n_strata,"_weight.csv", sep = "")))

write.csv(bts_strata_season_top_count, file.path(csv_dir, paste("bts_strata_season_top",n_strata,"_count.csv", sep = "")))
write.csv(bts_strata_season_top_weight, file.path(csv_dir, paste("bts_strata_season_top",n_strata,"_weight.csv", sep = "")))

# Non-offshore wind farm bottom trawl survey
write.csv(non_strata, file.path(csv_dir, paste("non_strata.csv", sep = "")))
write.csv(non_strata_season, file.path(csv_dir, paste("non_strata_season.csv", sep = "")))

write.csv(non_strata_top_count, file.path(csv_dir, paste("non_strata_top",n_strata,"_count.csv", sep = "")))
write.csv(non_strata_top_weight, file.path(csv_dir, paste("non_strata_top",n_strata,"_weight.csv", sep = "")))

write.csv(non_strata_season_top_count, file.path(csv_dir, paste("non_strata_season_top",n_strata,"_count.csv", sep = "")))
write.csv(non_strata_season_top_weight, file.path(csv_dir, paste("non_strata_season_top",n_strata,"_weight.csv", sep = "")))

# Offshore wind farm bottom trawl survey
write.csv(owf_strata, file.path(csv_dir, paste("owf_strata.csv", sep = "")))
write.csv(owf_strata_season, file.path(csv_dir, paste("owf_strata_season.csv", sep = "")))

write.csv(owf_strata_top_count, file.path(csv_dir, paste("owf_strata_top",n_strata,"_count.csv", sep = "")))
write.csv(owf_strata_top_weight, file.path(csv_dir, paste("owf_strata_top",n_strata,"_weight.csv", sep = "")))

write.csv(owf_strata_season_top_count, file.path(csv_dir, paste("owf_strata_season_top",n_strata,"_count.csv", sep = "")))
write.csv(owf_strata_season_top_weight, file.path(csv_dir, paste("owf_strata_season_top",n_strata,"_weight.csv", sep = "")))

# Offshore wind farm areas
write.csv(farm_agg, file.path(csv_dir, paste("farm_aggregated.csv")))
write.csv(farm_strata, file.path(csv_dir, paste("farm_strata_aggregated.csv")))
write.csv(farm_strata_season, file.path(csv_dir, paste("farm_strata_season_aggregated.csv")))

write.csv(farm_agg_top_count, file.path(csv_dir, paste("farm_aggregated_top", n_farm,"_count.csv", sep = "")))
write.csv(farm_agg_top_weight, file.path(csv_dir, paste("farm_aggregated_top", n_farm,"_weight.csv", sep = "")))

write.csv(farm_strata_top_count, file.path(csv_dir, paste("farm_strata_top", n_farm,"_count.csv", sep = "")))
write.csv(farm_strata_top_weight, file.path(csv_dir, paste("farm_strata_top", n_farm,"_weight.csv", sep = "")))

write.csv(farm_season_top_count, file.path(csv_dir, paste("farm_season_top", n_farm,"_count.csv", sep = "")))
write.csv(farm_season_top_weight, file.path(csv_dir, paste("farm_season_top", n_farm,"_weight.csv", sep = "")))

write.csv(farm_strata_season_top_count, file.path(csv_dir, paste("farm_strata_season_top", n_farm,"_count.csv", sep = "")))
write.csv(farm_strata_season_top_weight, file.path(csv_dir, paste("farm_strata_season_top", n_farm,"_weight.csv", sep = "")))

## RDS

## shapefile

###################################################

### export figures
## BTS figures
# count
ggsave(bts_stratum_count, filename = file.path(figure_dir, paste("bts_strata_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_count, filename = file.path(figure_dir, paste("bts_strata_season_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_facet_count, filename = file.path(figure_dir, paste("bts_strata_season_facet_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


ggsave(bts_stratum_top_count, filename = file.path(figure_dir, paste("bts_strata_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_top_count, filename = file.path(figure_dir, paste("bts_strata_season_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")
       
ggsave(bts_stratum_season_facet_strata_top_count, filename = file.path(figure_dir, paste("bts_strata_season_facet_strata_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_facet_season_top_count, filename = file.path(figure_dir, paste("bts_strata_season_facet_season_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(bts_stratum_weight, filename = file.path(figure_dir, paste("bts_strata_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_weight, filename = file.path(figure_dir, paste("bts_strata_season_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_facet_weight, filename = file.path(figure_dir, paste("bts_strata_season_facet_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


ggsave(bts_stratum_top_weight, filename = file.path(figure_dir, paste("bts_strata_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_top_weight, filename = file.path(figure_dir, paste("bts_strata_season_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_facet_strata_top_weight, filename = file.path(figure_dir, paste("bts_strata_season_facet_strata_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(bts_stratum_season_facet_season_top_weight, filename = file.path(figure_dir, paste("bts_strata_season_facet_season_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

###################################################

## non-owf figures
# count
ggsave(non_stratum_count, filename = file.path(figure_dir, paste("non_strata_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_count, filename = file.path(figure_dir, paste("non_strata_season_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_facet_count, filename = file.path(figure_dir, paste("non_strata_season_facet_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


ggsave(non_stratum_top_count, filename = file.path(figure_dir, paste("non_strata_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_top_count, filename = file.path(figure_dir, paste("non_strata_season_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_facet_strata_top_count, filename = file.path(figure_dir, paste("non_strata_season_facet_strata_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_facet_season_top_count, filename = file.path(figure_dir, paste("non_strata_season_facet_season_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(non_stratum_weight, filename = file.path(figure_dir, paste("non_strata_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_weight, filename = file.path(figure_dir, paste("non_strata_season_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_facet_weight, filename = file.path(figure_dir, paste("non_strata_season_facet_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


ggsave(non_stratum_top_weight, filename = file.path(figure_dir, paste("non_strata_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_top_weight, filename = file.path(figure_dir, paste("non_strata_season_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_facet_strata_top_weight, filename = file.path(figure_dir, paste("non_strata_season_facet_strata_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(non_stratum_season_facet_season_top_weight, filename = file.path(figure_dir, paste("non_strata_season_facet_season_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

###################################################

## OWF figures
# count
ggsave(owf_stratum_count, filename = file.path(figure_dir, paste("owf_strata_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_count, filename = file.path(figure_dir, paste("owf_strata_season_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_facet_count, filename = file.path(figure_dir, paste("owf_strata_season_facet_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


ggsave(owf_stratum_top_count, filename = file.path(figure_dir, paste("owf_strata_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_top_count, filename = file.path(figure_dir, paste("owf_strata_season_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_facet_strata_top_count, filename = file.path(figure_dir, paste("owf_strata_season_facet_strata_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_facet_season_top_count, filename = file.path(figure_dir, paste("owf_strata_season_facet_season_top",n_strata,"_strata_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(owf_stratum_weight, filename = file.path(figure_dir, paste("owf_strata_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_weight, filename = file.path(figure_dir, paste("owf_strata_season_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_facet_weight, filename = file.path(figure_dir, paste("owf_strata_season_facet_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


ggsave(owf_stratum_top_weight, filename = file.path(figure_dir, paste("owf_strata_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_top_weight, filename = file.path(figure_dir, paste("owf_strata_season_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_facet_strata_top_weight, filename = file.path(figure_dir, paste("owf_strata_season_facet_strata_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_stratum_season_facet_season_top_weight, filename = file.path(figure_dir, paste("owf_strata_season_facet_season_top",n_strata,"_strata_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

###################################################

# Wind farm figures
# count
ggsave(farm_agg_count, filename = file.path(figure_dir, paste("farm_agg_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_agg_strata_area_count, filename = file.path(figure_dir, paste("farm_agg_strata_area_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


ggsave(farm_agg_strata_season_count, filename = file.path(figure_dir, paste("farm_agg_strata_season_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_agg_area_season_count, filename = file.path(figure_dir, paste("farm_agg_area_season_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_agg_area_strata_season_count, filename = file.path(figure_dir, paste("farm_agg_area_strata_season_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_strata_area_season_count, filename = file.path(figure_dir, paste("farm_strata_area_season_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


# ggsave(farm_top_agg_count, filename = file.path(figure_dir, paste("farm_agg_top", n_farm, "_areas_count_", begin_year, "_", end_year, ".tiff", sep = "")),
#        width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_top_agg_strata_count, filename = file.path(figure_dir, paste("farm_agg_top", n_farm, "_areas_strata_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_top_agg_season_count, filename = file.path(figure_dir, paste("farm_agg_top", n_farm, "_areas_season_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_top_agg_strata_season_area_facet_count, filename = file.path(figure_dir, paste("farm_agg_top", n_farm, "_strata_season_area_facet_count_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(farm_agg_weight, filename = file.path(figure_dir, paste("farm_agg_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_agg_strata_area_weight, filename = file.path(figure_dir, paste("farm_agg_strata_area_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_agg_strata_season_weight, filename = file.path(figure_dir, paste("farm_agg_strata_season_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_agg_area_season_weight, filename = file.path(figure_dir, paste("farm_agg_area_season_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_agg_area_strata_season_weight, filename = file.path(figure_dir, paste("farm_agg_area_strata_season_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_strata_area_season_weight, filename = file.path(figure_dir, paste("farm_strata_area_season_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")


# ggsave(farm_top_agg_weight, filename = file.path(figure_dir, paste("farm_agg_top", n_farm, "_areas_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
#        width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_top_agg_strata_weight, filename = file.path(figure_dir, paste("farm_agg_top", n_farm, "_areas_strata_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_top_agg_season_weight, filename = file.path(figure_dir, paste("farm_agg_top", n_farm, "_areas_season_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

ggsave(farm_top_agg_strata_season_area_facet_weight, filename = file.path(figure_dir, paste("farm_agg_top", n_farm, "_strata_season_area_facet_weight_", begin_year, "_", end_year, ".tiff", sep = "")),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")
