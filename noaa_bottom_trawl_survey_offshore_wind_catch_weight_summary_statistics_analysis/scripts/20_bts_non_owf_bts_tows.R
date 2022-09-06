###################################################
###################################################
###################################################


######### Part 20 #########
## Non wind farm survey data
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
export_geopackage <- "data\\geopackage\\bts_non_owf_geopackage.gpkg"

state_dir <- "data\\c_spatial_data\\us_states"

rds_dir <- "data\\g_rds_tables"

wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

###################################################
###################################################
###################################################

non_wind_farm_representative_survey_point_function <- function(bts_representative, wind_farm){
  # generate only survey points that fall within the offshore wind farm area
  non_owf_data <- st_difference(bts_representative, st_union(st_combine(wind_farm)))
  return(non_owf_data)
}

###################################################
###################################################
###################################################

### Load catch and weight data along with stratum data
bts_representative <- st_read(dsn = geopackage, layer = "bts_representative_sf")

###################################################

### Load Atlantic wind farms
wind_farm <- st_read(dsn = wind_geopackage, layer = "owf_areas")

###################################################
###################################################
###################################################

### Wind farm survey tows
## Atlantic
atlantic_non_owf_tows <- non_wind_farm_representative_survey_point_function(bts_representative, wind_farm)

###################################################
###################################################
###################################################

# Export data
# Atlantic data
st_write(obj = atlantic_non_owf_tows, dsn = export_geopackage, layer = "atlantic_non_owf_tows", append = F)
