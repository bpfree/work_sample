###################################################
###################################################
###################################################


######### Part 32 #########
## Tow calculations
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
bts_geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"
non_owf_geopackage <- "data\\geopackage\\bts_non_owf_geopackage.gpkg"

csv_dir <- "data\\f_csv_tables\\tow_summary_tables"

###################################################
###################################################
###################################################

# load data
bts_all_tows <- st_read(dsn = bts_geopackage, layer = "bts_all_sf")
bts_representative_tows <- st_read(dsn = bts_geopackage, layer = "bts_representative_sf")
owf_bts_tows <- st_read(dsn = wind_geopackage, layer = "atlantic_owf_tows")
non_owf_tows <- st_read(dsn = non_owf_geopackage, layer = "atlantic_non_owf_tows")

###################################################
###################################################
###################################################

### calculate tow counts
## all tows
tow_all_count <- bts_all_tows %>%
  dplyr::group_by(stratum) %>%
  dplyr::summarise(all_tows = n_distinct(id)) %>%
  as.data.frame() %>%
  dplyr::select(-geom)

## representative tows
tow_representative_count <- bts_representative_tows %>%
  dplyr::group_by(stratum) %>%
  dplyr::summarise(representative_tows = n_distinct(id)) %>%
  as.data.frame() %>%
  dplyr::select(-geom)

## within wind farm areas
tow_owf_count <- owf_bts_tows %>%
  dplyr::group_by(stratum) %>%
  dplyr::summarise(owf_tows = n_distinct(id)) %>%
  as.data.frame() %>%
  dplyr::select(-geom)

## outside wind farm areas
tow_non_count <- non_owf_tows %>%
  dplyr::group_by(stratum) %>%
  dplyr::summarise(non_owf_tows = n_distinct(id)) %>%
  as.data.frame() %>%
  dplyr::select(-geom)

## combined tows
tow_join <- tow_all_count %>%
  dplyr::full_join(tow_representative_count) %>%
  dplyr::full_join(tow_owf_count) %>%
  dplyr::full_join(tow_non_count)

###################################################

## wind farm tow counts
tow_wind_farm_count <- owf_bts_tows %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(wind_farm_tows = n_distinct(id)) %>%
  as.data.frame() %>%
  dplyr::select(-geom)

tow_wind_farm_strata_count <- owf_bts_tows %>%
  dplyr::group_by(name,
                  stratum) %>%
  dplyr::summarise(wind_farm_strata_tows = n_distinct(id)) %>%
  as.data.frame() %>%
  dplyr::select(-geom)

###################################################
###################################################
###################################################

### export data
## CSV
write.csv(tow_join, file.path(csv_dir, paste("bts_tow_count_comparison_table.csv")))
write.csv(tow_wind_farm_count, file.path(csv_dir, paste("tow_wind_farm_count_table.csv")))
write.csv(tow_wind_farm_strata_count, file.path(csv_dir, paste("tow_wind_farm_strata_count_table.csv")))
