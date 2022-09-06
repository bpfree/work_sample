###################################################
###################################################
###################################################


######### Part 21 #########
## Wind farms survey data
## Non-OWF spatial
## Spatial and Seasonal
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

### Spatial function
bts_non_owf_spatial_function <- function(bts_non_owf_survey){
  bts_non_owf_spatial <- bts_non_owf_survey %>%
    # group data by stratum
    dplyr::group_by(stratum,
                    season) %>%
    # calculate total count of caught species per stratum
    dplyr::summarise(count = sum(count, na.rm = TRUE),
                     diversity = n_distinct(species),
                     weight = sum(weight, na.rm = TRUE),
                     tows = length(unique(id))) %>%
    # create a new field that divides (normalizes) the species weight by number of tows and area
    dplyr::mutate(count_per_tow = count / tows,
                  weight_per_tow = weight / tows) %>%
    dplyr::select(stratum, season,
                  count, diversity, weight,
                  tows, count_per_tow, weight_per_tow)
  return(bts_non_owf_spatial)
}


###################################################
###################################################
###################################################

# load Atlantic offshore wind farm survey data
bts_non_owf_survey <- st_read(dsn = geopackage, layer = "atlantic_non_owf_tows")

###################################################
###################################################
###################################################

# calculate spatial indexes for offshore wind farms
bts_non_owf_spatial <- bts_non_owf_spatial_function(bts_non_owf_survey)

bts_non_owf_spatial_csv <- as.data.frame(bts_non_owf_spatial) %>%
  dplyr::select(-geom)

###################################################
###################################################
###################################################

# Export data
st_write(obj = bts_non_owf_spatial, dsn = export_geopackage, layer = "bts_non_owf_spatial", append = F)

write.csv(bts_non_owf_spatial_csv, file = paste0(export_dir, "/", "bts_non_owf_spatial_data.csv"))
saveRDS(bts_non_owf_spatial, file = paste0(rds_dir, "/", "bts_non_owf_spatial.rds"))
