###################################################
###################################################
###################################################


######### Part 26 #########
## Summary species reporting tables
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr,tidyr)

### data directories
bts_geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
bts_non_owf_geopackage <- "data\\geopackage\\bts_non_owf_geopackage.gpkg"
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

summary_geopackage <- "data\\geopackage\\summary_bts_owf_geopackage.gpkg"

figure_dir <- "figures\\charts\\species"
csv_dir <- "data\\f_csv_tables\\species_summary_tables"

### pre-determinants
begin_year <- 1963
end_year <- 2019

seasons <- c("Spring|Fall")

wind_farms_names <- c("South|North")

# grab the top number of species of interest
n_species <- 5

map_theme <- theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.title.x = element_text(size = 10),
                   axis.text.x = element_text(size = 8),
                   axis.title.y = element_text(size = 10),
                   plot.subtitle = element_text(size = 8),
                   plot.caption = element_text(size = 6,
                                               lineheight = 0.5),
                   legend.title = element_text(size = 8,
                                               hjust = 0.5),
                   legend.text = element_text(size = 5),
                   legend.key.width = unit(0.4, "cm"),
                   legend.key.height = unit(0.3, "cm"))

###################################################
###################################################
###################################################

### load data
bts_species <- as.data.frame(st_read(dsn = bts_geopackage, layer = "bts_species"))
non_owf_species <- as.data.frame(st_read(dsn = bts_non_owf_geopackage, layer = "bts_non_owf_species"))
owf_species <- as.data.frame(st_read(dsn = wind_geopackage, layer = "owf_species"))

###################################################
###################################################
###################################################

### generating summary data
## BTS species
# count
bts_top_species_count <- bts_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by species and common_name (both are included in case there are species that use the same common name)
  dplyr::group_by(species,
                  common_name) %>%
  # calculate the total count and weights by species
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_species, order_by = count_summary)

# weight
bts_top_species_weight <- bts_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by species and common_name (both are included in case there are species that use the same common name)
  dplyr::group_by(species,
                  common_name) %>%
  # calculate the total count and weights by species
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary)

## BTS species by season
# count
bts_top_species_season_count <- bts_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by season and species
  dplyr::group_by(season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each season
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # reorder to have largest count at the top
  dplyr::arrange(season, desc(count_summary)) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

# weight
bts_top_species_season_weight <- bts_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by season and species
  dplyr::group_by(season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each season
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # reorder to have largest weight at the top
  dplyr::arrange(season, desc(weight_summary)) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by season
  dplyr::arrange(season, desc(weight_summary))

## BTS species by year
# count
bts_top_species_year_count <- bts_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year and species
  dplyr::group_by(year,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest counts by year
  dplyr::arrange(year, desc(count_summary)) %>%
  # group by year
  dplyr::group_by(year) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by year
  dplyr::arrange(year, desc(count_summary))

bts_top_species_year_graphic_count <- bts_top_species_year_count %>%
  # make as a data frame
  as.data.frame() %>%
  # fill in any combination of year and common name and fill in with 0 data
  tidyr::complete(year, nesting(common_name), fill = list(species = 0,
                                                          count_summary = 0,
                                                          weight_summary = 0))

# weight
bts_top_species_year_weight <- bts_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year and species
  dplyr::group_by(year,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest weights by year
  dplyr::arrange(year, desc(weight_summary)) %>%
  # group by year
  dplyr::group_by(year) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by year
  dplyr::arrange(year, desc(count_summary))

bts_top_species_year_graphic_weight <- bts_top_species_year_weight %>%
  # make as a data frame
  as.data.frame() %>%
  # fill in any combination of year and common name and fill in with 0 data
  tidyr::complete(year, nesting(common_name), fill = list(species = 0,
                                                          count_summary = 0,
                                                          weight_summary = 0))

## Non-OWF species by year and season
# count
bts_top_species_year_season_count <- bts_species %>%
  # # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year, season, and species
  dplyr::group_by(year,
                  season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year and season combination
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest counts by year and season combinations
  dplyr::arrange(year, season, desc(count_summary)) %>%
  # group by year and season
  dplyr::group_by(year,
                  season) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by year and season combinations
  dplyr::arrange(year, season, desc(count_summary))

# weight
bts_top_species_year_season_weight <- bts_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year, season, and species
  dplyr::group_by(year,
                  season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year and season combination
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest weights by year and season combinations
  dplyr::arrange(year, season, desc(weight_summary)) %>%
  # group by year and season
  dplyr::group_by(year,
                  season) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by year and season combinations
  dplyr::arrange(year, season, desc(weight_summary))

###################################################
###################################################

## Non-OWF species
# count
non_top_species_count <- non_owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by species and common_name (both are included in case there are species that use the same common name)
  dplyr::group_by(species,
                  common_name) %>%
  # calculate the total count and weights by species
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_species, order_by = count_summary)

# weight
non_top_species_weight <- non_owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by species and common_name (both are included in case there are species that use the same common name)
  dplyr::group_by(species,
                  common_name) %>%
  # calculate the total count and weights by species
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary)

## Non-OWF species by season
# count
non_top_species_season_count <- non_owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by season and species
  dplyr::group_by(season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each season
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # reorder to have largest count at the top
  dplyr::arrange(season, desc(count_summary)) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

# weight
non_top_species_season_weight <- non_owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by season and species
  dplyr::group_by(season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each season
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # reorder to have largest weight at the top
  dplyr::arrange(season, desc(weight_summary)) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by season
  dplyr::arrange(season, desc(weight_summary))

## Non-OWF species by year
# count
non_top_species_year_count <- non_owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year and species
  dplyr::group_by(year,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest counts by year
  dplyr::arrange(year, desc(count_summary)) %>%
  # group by year
  dplyr::group_by(year) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by year
  dplyr::arrange(year, desc(count_summary))

non_top_species_year_graphic_count <- non_top_species_year_count %>%
  # make as a data frame
  as.data.frame() %>%
  # fill in any combination of year and common name and fill in with 0 data
  tidyr::complete(year, nesting(common_name), fill = list(species = 0,
                                                          count_summary = 0,
                                                          weight_summary = 0))

# weight
non_top_species_year_weight <- non_owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year and species
  dplyr::group_by(year,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest weights by year
  dplyr::arrange(year, desc(weight_summary)) %>%
  # group by year
  dplyr::group_by(year) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by year
  dplyr::arrange(year, desc(count_summary))

non_top_species_year_graphic_weight <- non_top_species_year_weight %>%
  # make as a data frame
  as.data.frame() %>%
  # fill in any combination of year and common name and fill in with 0 data
  tidyr::complete(year, nesting(common_name), fill = list(species = 0,
                                                          count_summary = 0,
                                                          weight_summary = 0))

## Non-OWF species by year and season
# count
non_top_species_year_season_count <- non_owf_species %>%
  # # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year, season, and species
  dplyr::group_by(year,
                  season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year and season combination
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest counts by year and season combinations
  dplyr::arrange(year, season, desc(count_summary)) %>%
  # group by year and season
  dplyr::group_by(year,
                  season) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by year and season combinations
  dplyr::arrange(year, season, desc(count_summary))

# weight
non_top_species_year_season_weight <- non_owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year, season, and species
  dplyr::group_by(year,
                  season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year and season combination
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest weights by year and season combinations
  dplyr::arrange(year, season, desc(weight_summary)) %>%
  # group by year and season
  dplyr::group_by(year,
                  season) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by year and season combinations
  dplyr::arrange(year, season, desc(weight_summary))

###################################################
###################################################

## OWF species
# count
owf_top_species_count <- owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by species and common_name (both are included in case there are species that use the same common name)
  dplyr::group_by(species,
                  common_name) %>%
  # calculate the total count and weights by species
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by count
  dplyr::slice_max(n = n_species, order_by = count_summary)

# weight
owf_top_species_weight <- owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by species and common_name (both are included in case there are species that use the same common name)
  dplyr::group_by(species,
                  common_name) %>%
  # calculate the total count and weights by species
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # return top (as desired) species by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary)

## OWF species by season
# count
owf_top_species_season_count <- owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by season and species
  dplyr::group_by(season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each season
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # reorder to have largest count at the top
  dplyr::arrange(season, desc(count_summary)) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by season
  dplyr::arrange(season, desc(count_summary))

# weight
owf_top_species_season_weight <- owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by season and species
  dplyr::group_by(season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each season
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # reorder to have largest weight at the top
  dplyr::arrange(season, desc(weight_summary)) %>%
  # group by season
  dplyr::group_by(season) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by season
  dplyr::arrange(season, desc(weight_summary))

## OWF species by year
# count
owf_top_species_year_count <- owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year and species
  dplyr::group_by(year,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest counts by year
  dplyr::arrange(year, desc(count_summary)) %>%
  # group by year
  dplyr::group_by(year) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by year
  dplyr::arrange(year, desc(count_summary))

owf_top_species_year_graphic_count <- owf_top_species_year_count %>%
  # make as a data frame
  as.data.frame() %>%
  # fill in any combination of year and common name and fill in with 0 data
  tidyr::complete(year, nesting(common_name), fill = list(species = 0,
                                                          count_summary = 0,
                                                          weight_summary = 0))

# weight
owf_top_species_year_weight <- owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year and species
  dplyr::group_by(year,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # view data by largest to smallest weights by year
  dplyr::arrange(year, desc(weight_summary)) %>%
  # group by year
  dplyr::group_by(year) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by year
  dplyr::arrange(year, desc(weight_summary))

owf_top_species_year_graphic_weight <- owf_top_species_year_weight %>%
  # make as a data frame
  as.data.frame() %>%
  # fill in any combination of year and common name and fill in with 0 data
  tidyr::complete(year, nesting(common_name), fill = list(species = 0,
                                                          count_summary = 0,
                                                          weight_summary = 0))

## OWF species by year and season
# count
owf_top_species_year_season_count <- owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year, season, and species
  dplyr::group_by(year,
                  season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year and season combination
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # vied data by largest to smallest counts by year and season combinations
  dplyr::arrange(year, season, desc(count_summary)) %>%
  # group by year and season
  dplyr::group_by(year,
                  season) %>%
  # return top number of species wanted by count
  dplyr::slice_max(n = n_species, order_by = count_summary) %>%
  # view data by largest to smallest counts by year and season combinations
  dplyr::arrange(year, season, desc(count_summary))

# weight
owf_top_species_year_season_weight <- owf_species %>%
  # subset by seasons and years of interest
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  # group by year, season, and species
  dplyr::group_by(year,
                  season,
                  species,
                  common_name) %>%
  # calculate the total count and weights by species for each year and season combination
  dplyr::summarise(count_summary = sum(count_normalize),
                   weight_summary = sum(weight_normalize)) %>%
  # remove grouping
  dplyr::ungroup() %>%
  # vied data by largest to smallest weights by year and season combinations
  dplyr::arrange(year, season, desc(weight_summary)) %>%
  # group by year and season
  dplyr::group_by(year,
                  season) %>%
  # return top number of species wanted by weight
  dplyr::slice_max(n = n_species, order_by = weight_summary) %>%
  # view data by largest to smallest weights by year and season combinations
  dplyr::arrange(year, season, desc(weight_summary))

###################################################
###################################################
###################################################

### Figures
## BTS
# top species counts
top_species_bts_count <- ggplot() + 
  geom_bar(data = bts_top_species_count, aes(x = common_name, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Species (common name)",
       y = "Count",
       title = paste("Top", n_species, "species counts for all representative BTS tows"),
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata.")) +
  # add theme
  theme_bw() +
  theme(legend.position = "none") +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 350000, 50000))

# top species weights
top_species_bts_weight <- ggplot() + 
  geom_bar(data = bts_top_species_weight, aes(x = common_name, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Species (common name)",
       y = "Weight (kg)",
       title = paste("Top", n_species, "species weights for all representative BTS tows"),
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata.")) +
  # add theme
  theme_bw() +
  theme(legend.position = "none") +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 10000, 1000))

# top species counts by season
season_species_bts_count <- ggplot() + 
  geom_bar(data = bts_top_species_season_count, aes(x = season, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Season",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts (Fall vs. Spring) for all representative BTS tows"),
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata.")) +
  # add theme
  theme_bw() +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 400000, 50000))

# top species weights by season
season_species_bts_weight <- ggplot() + 
  geom_bar(data = bts_top_species_season_weight, aes(x = season, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Season",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights (Fall vs. Spring) for all representative BTS tows"),
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata.")) +
  # add theme
  theme_bw() +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 22500, 2500))

# top species counts by year (bar graph)
year_species_bar_bts_count <- ggplot() + 
  geom_bar(data = bts_top_species_year_graphic_count, aes(x = year, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  # flip the x- and y-axis
  coord_flip() +
  labs(x = "Year",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts by year for all representative BTS tows"),
       paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 40000, 2500)) +
  guides(fill = guide_legend(ncol = 1)) + 
  # add theme
  theme_bw() +
  # map theme
  map_theme

# top species weights by year (bar graph)
year_species_bar_bts_weight <- ggplot() + 
  geom_bar(data = bts_top_species_year_graphic_weight, aes(x = year, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  # flip the x- and y-axis
  coord_flip() +
  labs(x = "Year",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights by year for all representative BTS tows"),
       paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  guides(fill = guide_legend(ncol = 1)) + 
  # add theme
  theme_bw() +
  # map theme
  map_theme

# top species counts by year (area graph)
year_species_area_bts_count <- ggplot() + 
  geom_area(data = bts_top_species_year_graphic_count, aes(x = year, y = count_summary, fill = str_to_title(common_name))) +
  labs(x = "Year",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts by year for all representative BTS tows"),
       paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 35000, 5000)) +
  # add theme
  theme_bw() +
  guides(fill = guide_legend(ncol = 1)) +
  # map theme
  map_theme

# top species weights by year (area graph)
year_species_area_bts_weight <- ggplot() + 
  geom_area(data = bts_top_species_year_graphic_weight, aes(x = year, y = weight_summary, fill = str_to_title(common_name))) +
  labs(x = "Year",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights by year for all representative BTS tows"),
       paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  # add theme
  theme_bw() +
  guides(fill = guide_legend(ncol = 1)) +
  # map theme
  map_theme

###################################################
###################################################

## Non-OWF
# top species counts
top_species_non_count <- ggplot() + 
  geom_bar(data = non_top_species_count, aes(x = common_name, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Species (common name)",
       y = "Count",
       title = paste("Top", n_species, "species counts outside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       outside of the offshore wind farm areas.")) +
  # add theme
  theme_bw() +
  theme(legend.position = "none") +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 350000, 50000))

# top species weights
top_species_non_weight <- ggplot() + 
  geom_bar(data = non_top_species_weight, aes(x = common_name, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Species (common name)",
       y = "Weight (kg)",
       title = paste("Top", n_species, "species weights outside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       outside of the offshore wind farm areas.")) +
  # add theme
  theme_bw() +
  theme(legend.position = "none") +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 7000, 1000))

# top species counts by season
season_species_non_count <- ggplot() + 
  geom_bar(data = non_top_species_season_count, aes(x = season, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Season",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts (Fall vs. Spring) outside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       outside of the offshore wind farm areas.")) +
  # add theme
  theme_bw() +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 400000, 50000))

# top species weights by season
season_species_non_weight <- ggplot() + 
  geom_bar(data = non_top_species_season_weight, aes(x = season, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Season",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights (Fall vs. Spring) outside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       outside of the offshore wind farm areas.")) +
  # add theme
  theme_bw() +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 20000, 2500))

# top species counts by year (bar graph)
year_species_bar_non_count <- ggplot() + 
  geom_bar(data = non_top_species_year_graphic_count, aes(x = year, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  # flip the x- and y-axis
  coord_flip() +
  labs(x = "Year",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts by year outside offshore wind farm areas"),
       paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 40000, 2500)) +
  guides(fill = guide_legend(ncol = 1)) + 
  # add theme
  theme_bw() +
  # map theme
  map_theme

# top species weights by year (bar graph)
year_species_bar_non_weight <- ggplot() + 
  geom_bar(data = non_top_species_year_graphic_weight, aes(x = year, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  # flip the x- and y-axis
  coord_flip() +
  labs(x = "Year",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights by year outside offshore wind farm areas"),
       paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  guides(fill = guide_legend(ncol = 1)) + 
  # add theme
  theme_bw() +
  # map theme
  map_theme

# top species counts by year (area graph)
year_species_area_non_count <- ggplot() + 
  geom_area(data = non_top_species_year_graphic_count, aes(x = year, y = count_summary, fill = str_to_title(common_name))) +
  labs(x = "Year",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts by year outside offshore wind farm areas"),
       paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 35000, 5000)) +
  # add theme
  theme_bw() +
  guides(fill = guide_legend(ncol = 1)) +
  # map theme
  map_theme

# top species weights by year (area graph)
year_species_area_non_weight <- ggplot() + 
  geom_area(data = non_top_species_year_graphic_weight, aes(x = year, y = weight_summary, fill = str_to_title(common_name))) +
  labs(x = "Year",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights by year outside offshore wind farm areas"),
       paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       outside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  # add theme
  theme_bw() +
  guides(fill = guide_legend(ncol = 1)) +
  # map theme
  map_theme

###################################################
###################################################

## OWF
# top species counts
top_species_owf_count <- ggplot() + 
  geom_bar(data = owf_top_species_count, aes(x = common_name, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Species (common name)",
       y = "Count",
       title = paste("Top", n_species, "species counts inside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       inside of the offshore wind farm areas.")) +
  # add theme
  theme_bw() +
  theme(legend.position = "none") +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 40000, 5000))

# top species weights
top_species_owf_weight <- ggplot() + 
  geom_bar(data = owf_top_species_weight, aes(x = common_name, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Species (common name)",
       y = "Weight (kg)",
       title = paste("Top", n_species, "species weights inside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       inside of the offshore wind farm areas.")) +
  # add theme
  theme_bw() +
  theme(legend.position = "none") +
  # map theme
  map_theme +
  # change limits
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10)) +
  scale_y_continuous(breaks = seq(0, 5000, 500))


# top species counts by season
season_species_owf_count <- ggplot() + 
  geom_bar(data = owf_top_species_season_count, aes(x = season, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Season",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts (Fall vs. Spring) inside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       inside of the offshore wind farm areas.")) +
  # add theme
  theme_bw() +
  # map theme
  map_theme +
  # change limits
  scale_y_continuous(breaks = seq(0, 100000, 25000)) +
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10))

# top species weights by season
season_species_owf_weight <- ggplot() + 
  geom_bar(data = owf_top_species_season_weight, aes(x = season, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  labs(x = "Season",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights (Fall vs. Spring) inside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       inside of the offshore wind farm areas.")) +
  # add theme
  theme_bw() +
  # map theme
  map_theme +
  # change limits
  scale_y_continuous(breaks = seq(0, 10000, 2500)) +
  scale_x_discrete(labels = function(common_name) str_wrap(str_to_title(common_name), width = 10))


# top species counts by year (bar graph)
year_species_bar_owf_count <- ggplot() + 
  geom_bar(data = owf_top_species_year_graphic_count, aes(x = year, y = count_summary, fill = str_to_title(common_name)), stat = "identity") +
  # flip the x- and y-axis
  coord_flip() +
  labs(x = "Year",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts by year inside offshore wind farm areas"),
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  guides(fill = guide_legend(ncol = 1)) + 
  # add theme
  theme_bw() +
  # map theme
  map_theme

# top species weights by year (bar graph)
year_species_bar_owf_weight <- ggplot() + 
  geom_bar(data = owf_top_species_year_graphic_weight, aes(x = year, y = weight_summary, fill = str_to_title(common_name)), stat = "identity") +
  # flip the x- and y-axis
  coord_flip() +
  labs(x = "Year",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights by year inside offshore wind farm areas"),
       paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 2500, 250)) +
  guides(fill = guide_legend(ncol = 1)) + 
  # add theme
  theme_bw() +
  # map theme
  map_theme


# top species counts by year (area graph)
year_species_area_owf_count <- ggplot() + 
  geom_area(data = owf_top_species_year_graphic_count, aes(x = year, y = count_summary, fill = str_to_title(common_name))) +
  labs(x = "Year",
       y = "Count",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species counts by year inside offshore wind farm areas"),
       paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for species were calculated by summing normalized counts\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  # add theme
  theme_bw() +
  guides(fill = guide_legend(ncol = 1)) +
  # map theme
  map_theme

# top species weights by year (area graph)
year_species_area_owf_weight <- ggplot() + 
  geom_area(data = owf_top_species_year_graphic_weight, aes(x = year, y = weight_summary, fill = str_to_title(common_name))) +
  labs(x = "Year",
       y = "Weight (kg)",
       fill = "Species\n(common name)",
       title = paste("Top", n_species, "species weights by year inside offshore wind farm areas"),
       paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for species were calculated by summing normalized weights\n
       between", begin_year, "and", end_year, "across all of the northern inshore and offshore strata\n
       inside of the offshore wind farm areas.")) +
  # change limits
  scale_x_continuous(breaks = seq(1963, 2020, 5)) +
  scale_y_continuous(breaks = seq(0, 2500, 250)) +
  # add theme
  theme_bw() +
  guides(fill = guide_legend(ncol = 1)) +
  # map theme
  map_theme

###################################################
###################################################
###################################################

### export data
## CSV
# Bottom trawl survey
write.csv(bts_top_species_count, file.path(csv_dir, paste("bts_top",n_species,"_species_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(bts_top_species_weight, file.path(csv_dir, paste("bts_top",n_species,"_species_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(bts_top_species_season_count, file.path(csv_dir, paste("bts_top",n_species,"_species_season_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(bts_top_species_season_weight, file.path(csv_dir, paste("bts_top",n_species,"_species_season_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(bts_top_species_year_count, file.path(csv_dir, paste("bts_top",n_species,"_species_year_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(bts_top_species_year_weight, file.path(csv_dir, paste("bts_top",n_species,"_species_year_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(bts_top_species_year_season_count, file.path(csv_dir, paste("bts_top",n_species,"_species_year_season_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(bts_top_species_year_season_weight, file.path(csv_dir, paste("bts_top",n_species,"_species_year_season_weight_",begin_year,"_",end_year,".csv", sep = "")))

# Non-offshore wind farm bottom trawl survey
write.csv(non_top_species_count, file.path(csv_dir, paste("bts_non_owf_top",n_species,"_species_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(non_top_species_weight, file.path(csv_dir, paste("bts_non_owf_top",n_species,"_species_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(non_top_species_season_count, file.path(csv_dir, paste("bts_non_owf_top",n_species,"_species_season_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(non_top_species_season_weight, file.path(csv_dir, paste("bts_non_owf_top",n_species,"_species_season_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(non_top_species_year_count, file.path(csv_dir, paste("bts_non_owf_top",n_species,"_species_year_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(non_top_species_year_weight, file.path(csv_dir, paste("bts_non_owf_top",n_species,"_species_year_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(non_top_species_year_season_count, file.path(csv_dir, paste("bts_non_owf_top",n_species,"_species_year_season_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(non_top_species_year_season_weight, file.path(csv_dir, paste("bts_non_owf_top",n_species,"_species_year_season_weight_",begin_year,"_",end_year,".csv", sep = "")))

# Offshore wind farm bottom trawl survey
write.csv(owf_top_species_count, file.path(csv_dir, paste("owf_top",n_species,"_species_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(owf_top_species_weight, file.path(csv_dir, paste("owf_top",n_species,"_species_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(owf_top_species_season_count, file.path(csv_dir, paste("owf_top",n_species,"_species_season_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(owf_top_species_season_weight, file.path(csv_dir, paste("owf_top",n_species,"_species_season_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(owf_top_species_year_count, file.path(csv_dir, paste("owf_top",n_species,"_species_year_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(owf_top_species_year_weight, file.path(csv_dir, paste("owf_top",n_species,"_species_year_weight_",begin_year,"_",end_year,".csv", sep = "")))

write.csv(owf_top_species_year_season_count, file.path(csv_dir, paste("owf_top",n_species,"_species_year_season_count_",begin_year,"_",end_year,".csv", sep = "")))
write.csv(owf_top_species_year_season_weight, file.path(csv_dir, paste("owf_top",n_species,"_species_year_season_weight_",begin_year,"_",end_year,".csv", sep = "")))


## RDS
#saveRDS(top_species_year_season, file = paste0(export_dir, "/", "bts_non_owf_species.rds"))

## shapefile
#st_write(obj = top_species_year_season, dsn = geopackage, layer = "top_bts_non_owf_species_year_season", append = F)

###################################################

### export figures
## BTS
# count
ggsave(top_species_non_count, filename = file.path(figure_dir, paste("bts_non_owf_top",n_species,"_species_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

ggsave(season_species_non_count, filename = file.path(figure_dir, paste("bts_non_owf_season_top",n_species,"_species_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_bar_non_count, filename = file.path(figure_dir, paste("bts_non_owf_year_top",n_species,"_species_bar_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_area_non_count, filename = file.path(figure_dir, paste("bts_non_owf_year_top",n_species,"_species_area_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(top_species_non_weight, filename = file.path(figure_dir, paste("bts_non_owf_top",n_species,"_species_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

ggsave(season_species_non_weight, filename = file.path(figure_dir, paste("bts_non_owf_season_top",n_species,"_species_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_bar_non_weight, filename = file.path(figure_dir, paste("bts_non_owf_year_top",n_species,"_species_bar_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_area_non_weight, filename = file.path(figure_dir, paste("bts_non_owf_year_top",n_species,"_species_area_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

###################################################

## Non-OWF
# count
ggsave(top_species_bts_count, filename = file.path(figure_dir, paste("bts_top",n_species,"_species_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

ggsave(season_species_bts_count, filename = file.path(figure_dir, paste("bts_season_top",n_species,"_species_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_bar_bts_count, filename = file.path(figure_dir, paste("bts_year_top",n_species,"_species_bar_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_area_bts_count, filename = file.path(figure_dir, paste("bts_year_top",n_species,"_species_area_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(top_species_bts_weight, filename = file.path(figure_dir, paste("bts_top",n_species,"_species_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

ggsave(season_species_bts_weight, filename = file.path(figure_dir, paste("bts_season_top",n_species,"_species_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_bar_bts_weight, filename = file.path(figure_dir, paste("bts_year_top",n_species,"_species_bar_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_area_bts_weight, filename = file.path(figure_dir, paste("bts_year_top",n_species,"_species_area_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

###################################################

## OWF
# count
ggsave(top_species_owf_count, filename = file.path(figure_dir, paste("owf_top",n_species,"_species_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

ggsave(season_species_owf_count, filename = file.path(figure_dir, paste("owf_season_top",n_species,"_species_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_bar_owf_count, filename = file.path(figure_dir, paste("owf_year_top",n_species,"_species_bar_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_area_owf_count, filename = file.path(figure_dir, paste("owf_year_top",n_species,"_species_area_count_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(top_species_owf_weight, filename = file.path(figure_dir, paste("owf_top",n_species,"_species_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

ggsave(season_species_owf_weight, filename = file.path(figure_dir, paste("owf_season_top",n_species,"_species_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_bar_owf_weight, filename = file.path(figure_dir, paste("owf_year_top",n_species,"_species_bar_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")

ggsave(year_species_area_owf_weight, filename = file.path(figure_dir, paste("owf_year_top",n_species,"_species_area_weight_",begin_year,"_",end_year,".tiff", sep = "")),
       width = 10, height = 6, units = "in", dpi = 600, compression = "lzw")
