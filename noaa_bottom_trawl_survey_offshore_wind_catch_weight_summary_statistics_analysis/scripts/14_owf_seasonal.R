###################################################
###################################################
###################################################


######### Part 14 #########
## Wind farms survey data
## Offshore Wind Farm
## Seasonal
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

export_dir <- "data\\d_summary_data\\c_seasonal"
csv_dir <- "data\\f_csv_tables\\owf_summary_tables"
rds_dir <- "data\\g_rds_tables"

###################################################
###################################################
###################################################

### load wind farm data
wind_farm_survey <- st_read(dsn = wind_geopackage, layer = "atlantic_owf_tows")

###################################################
###################################################
###################################################

#### Summarizing based on season (i.e., fall, spring, summer, winter)
### Seasons: nominal diversity, count, weight
## Functions
# get single table with year, diversity, count, and weight
seasonal_function <- function(wind_farm_survey){
  seasonal <- wind_farm_survey %>%
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
  return(seasonal)
}

## Seasonal: overall diversity, count, and weight
owf_seasonal <- seasonal_function(wind_farm_survey)

owf_seasonal_csv <- as.data.frame(owf_seasonal) %>%
  dplyr::select(-geom)

###################################################
###################################################
###################################################

# export data
saveRDS(owf_seasonal, file = paste0(rds_dir, "/", "owf_seasonal.rds"))

write.csv(owf_seasonal_csv, file = paste0(csv_dir, "/", "owf_seasonal_data.csv"))
