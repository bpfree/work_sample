###################################################
###################################################
###################################################


######### Part 7 #########
## BTS Temporal
## Stratum + Year + Season
##########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
geopackage <- "data\\geopackage\\bts_geopackage.gpkg"

export_dir <- "data\\f_csv_tables\\bts_summary_tables"
rds_dir <- "data\\g_rds_tables"

###################################################
###################################################
###################################################

### load representative data along with stratum data
bts_representative <- st_read(dsn = geopackage, layer = "bts_representative_sf")

length(levels(as.factor(bts_representative$stratum))) # suggests that 121 strata surveyed

## Strata boundary
# strata <- st_read(dsn = geopackage, layer = "strata")


# unique(strata$region)
# str(strata)
# View(strata)
# length(levels(as.factor(strata$stratum))) # suggests 147 exist (1 more than surveyed)
# 
# strata_qa_qc <- strata %>%
#   dplyr::group_by(stratum) %>%
#   dplyr::summarise(freq = n()) %>%
#   dplyr::filter(freq >= 2)
# 
# View(strata_qa_qc) # stratum 03920 appears twice, thus explaining why there is an extra stratum

###################################################
###################################################
###################################################

### Spatial functions
## Stratum only
# temporal_normalize_function <- function(bts_representative){
#   # diversity for each stratum
#   temporal <- bts_representative %>%
#     # group data by stratum
#     dplyr::group_by(stratum,
#                     season,
#                     year) %>%
#     # calculate total unique number of caught species per stratum
#     dplyr::summarise(diversity = n_distinct(species),
#                      count = sum(count, na.rm = TRUE),
#                      weight = sum(weight, na.rm = TRUE),
#                      tows = n()) %>%
#     # calculate catch per tow (mean catch per tow)
#     dplyr::mutate(count_per_tow = count / tows,
#                   weight_per_tow = weight / tows) %>%
#     dplyr::select(stratum, season, year, tows,
#                   diversity,
#                   count, count_per_tow,
#                   weight, weight_per_tow)
#   return(temporal)
# }
# 
# temporal <- temporal_normalize_function(bts_representative)
# 
# length(levels(as.factor(temporal$stratum))) # 146 surveyed

###################################################

# temporal_normalize_polygon_function <- function(bts_representative){
#   # diversity for each stratum
#   polygon <- as.data.frame(bts_representative) %>%
#     # group data by stratum
#     dplyr::group_by(stratum,
#                     season,
#                     year) %>%
#     # calculate total unique number of caught species per stratum
#     dplyr::summarise(diversity = n_distinct(species),
#                      count = sum(count, na.rm = TRUE),
#                      weight = sum(weight, na.rm = TRUE),
#                      tows = n()) %>%
#     # calculate catch per tow (mean catch per tow)
#     dplyr::mutate(count_per_tow = count / tows,
#                   weight_per_tow = weight / tows) %>%
#     # join with strata data to obtain corresponding spatial data
#     dplyr::inner_join(strata,
#                       by = "stratum") %>%
#     dplyr::select(stratum, season, year, tows,
#                   diversity,
#                   count, count_per_tow,
#                   weight, weight_per_tow,
#                   geom)
#   
#   return(polygon)
# }

bts_temporal_normalize_point_function <- function(bts_representative){
  # diversity for each stratum
  bts_temporal <- bts_representative %>%
    # group data by stratum
    dplyr::group_by(stratum,
                    season,
                    year) %>%
    # calculate total unique number of caught species per stratum
    dplyr::summarise(count = sum(count, na.rm = TRUE),
                     diversity = n_distinct(species),
                     weight = sum(weight, na.rm = TRUE),
                     tows = length(unique(id))) %>%
    # calculate catch per tow (mean catch per tow)
    dplyr::mutate(count_per_tow = count / tows,
                  weight_per_tow = weight / tows) %>%
    # join with strata data to obtain corresponding spatial data
    # dplyr::inner_join(as.data.frame(strata),
    #                   by = "stratum") %>%
    # dplyr::rename(geom = geom.x) %>%
    dplyr::select(stratum, year, season,
                  count, diversity, weight,
                  tows, count_per_tow, weight_per_tow)
  return(bts_temporal)
}

# temporal_polygon <- temporal_normalize_polygon_function(bts_representative)
bts_temporal <- bts_temporal_normalize_point_function(bts_representative)

bts_temporal_csv <- as.data.frame(bts_temporal) %>%
  dplyr::select(-geom)

# length(levels(as.factor(temporal_polygon$stratum)))
length(levels(as.factor(bts_temporal$stratum)))

#### Note:
## 9 strata are missing area values, suggesting the strata changed values at some point

## Area missing strata: "01350" "01410" "01420" "01490" "01990" "03210" "03910" "03940" "03990"

###################################################
###################################################
###################################################

## Plotting
plot(bts_temporal$year,
     bts_temporal$count)
plot(bts_temporal$year,
     bts_temporal$weight)

## Normalized data
plot(bts_temporal$year,
     bts_temporal$count_per_tow)
plot(bts_temporal$year,
     bts_temporal$weight_per_tow)

###################################################
###################################################
###################################################

### Export data
## as a RDS file (to open in R)
saveRDS(bts_temporal, file = paste0(rds_dir, "/", "bts_temporal.rds"))
write.csv(bts_temporal_csv, file = paste0(export_dir, "/", "bts_temporal_data.csv"))

## within a geopackage (to open up in other spatial software)
# st_write(obj = temporal_polygon, dsn = geopackage, layer = "temporal_normalized_polygon", append = F)
st_write(obj = bts_temporal, dsn = geopackage, layer = "bts_temporal", append = F)
