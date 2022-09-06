###################################################
###################################################
###################################################


######### Part 23 #########
## Wind farms survey data
## Non-OWF Seasonal
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
export_geopackage <- "data\\geopackage\\bts_non_owf_geopackage.gpkg"

export_dir <- "data\\f_csv_tables\\bts_non_owf_summary_tables"
rds_dir <- "data\\g_rds_tables"

###################################################
###################################################
###################################################

### load wind farm data
bts_non_owf_survey <- st_read(dsn = geopackage, layer = "atlantic_non_owf_tows")

###################################################
###################################################
###################################################

#### Summarizing based on season (i.e., fall, spring, summer, winter)
### Seasons: nominal diversity, count, weight
## Functions
# get single table with year, diversity, count, and weight
bts_non_owf_seasonal_function <- function(bts_non_owf_survey){
  bts_non_owf_seasonal <- bts_non_owf_survey %>%
    # group data by season
    dplyr::group_by(season) %>%
    # calculate total count of caught species per season
    dplyr::summarise(count = sum(count, na.rm = TRUE),
                     diversity = n_distinct(species),
                     weight = sum(weight, na.rm = TRUE),
                     tows = length(unique(id))) %>%
    # create new field that divides (normalizes) the species count by number of tows
    dplyr::mutate(count_per_tow = count / tows,
                  weight_per_tow = weight / tows) %>%
    dplyr::select(season,
                  tows,
                  count,
                  count_per_tow,
                  weight,
                  weight_per_tow)
  return(bts_non_owf_seasonal)
}

## Seasonal: overall diversity, count, and weight
bts_non_owf_seasonal <- bts_non_owf_seasonal_function(bts_non_owf_survey)

bts_non_owf_seasonal_csv <- as.data.frame(bts_non_owf_seasonal) %>%
  dplyr::select(-geom)

###################################################
###################################################
###################################################

# export data
write.csv(bts_non_owf_seasonal_csv, file = paste0(export_dir, "/", "bts_non_owf_seasonal_data.csv"))
saveRDS(bts_non_owf_seasonal, file = paste0(rds_dir, "/", "bts_non_owf_seasonal.rds"))
