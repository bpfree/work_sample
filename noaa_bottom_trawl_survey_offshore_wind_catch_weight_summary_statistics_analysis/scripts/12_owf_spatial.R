###################################################
###################################################
###################################################


######### Part 12 #########
## Wind farms survey data
## Offshore Wind Farm
## Spatial and Seasonal
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

export_dir <- "data\\f_csv_tables\\owf_summary_tables"
rds_dir <- "data\\g_rds_tables"

###################################################
###################################################
###################################################

### Spatial function
owf_spatial_function <- function(wind_farm_survey){
  owf_spatial <- wind_farm_survey %>%
    # group data by stratum
    dplyr::group_by(name,
                    state,
                    stratum,
                    season,
                    sq_nm) %>%
    # calculate total count of caught species per stratum
    dplyr::summarise(count = sum(count, na.rm = TRUE),
                     diversity = n_distinct(species),
                     weight = sum(weight, na.rm = TRUE),
                     tows = length(unique(id))) %>%
    # create a new field that divides (normalizes) the species weight by number of tows and area
    dplyr::mutate(count_per_tow = count / tows,
                  weight_per_tow = weight / tows,
                  count_per_sqnm = count / sq_nm,
                  diversity_per_sqnm = diversity / sq_nm,
                  weight_per_sqnm = weight / sq_nm) %>%
    dplyr::select(name, state, stratum, season,
                  count, diversity, weight,
                  tows, count_per_tow, weight_per_tow,
                  sq_nm, count_per_sqnm, diversity_per_sqnm, weight_per_sqnm)
  return(owf_spatial)
}


###################################################
###################################################
###################################################

# load Atlantic offshore wind farm survey data
wind_farm_survey <- st_read(dsn = wind_geopackage, layer = "atlantic_owf_tows")

###################################################
###################################################
###################################################

# calculate spatial indexes for offshore wind farms
owf_spatial <- owf_spatial_function(wind_farm_survey)

owf_spatial_csv <- as.data.frame(owf_spatial) %>%
  dplyr::select(-geom)

###################################################
###################################################
###################################################

# Export data
st_write(obj = owf_spatial, dsn = wind_geopackage, layer = "owf_spatial", append = F)

write.csv(owf_spatial_csv, file = paste0(export_dir, "/", "owf_spatial_data.csv"))
saveRDS(owf_spatial, file = paste0(rds_dir, "/", "owf_spatial.rds"))
