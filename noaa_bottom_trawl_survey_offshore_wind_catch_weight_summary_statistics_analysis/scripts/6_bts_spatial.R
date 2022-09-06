###################################################
###################################################
###################################################


######### Part 6 #########
## BTS Spatial
## Stratum + Season
##########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
bts_geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
spatial_dir <- "data\\c_spatial_data"
strata_dir <- "data\\c_spatial_data\\bts\\bts_strata_hist_data\\Strata_wHistData"

export_dir <- "data\\f_csv_tables\\bts_summary_tables"
rds_dir <- "data\\g_rds_tables"

###################################################
###################################################
###################################################

### load representative data along with stratum data
#bts_representative <- readRDS(paste(spatial_dir, "bts_representative_non_na.rds", sep = "/"))
bts_representative <- st_read(dsn = bts_geopackage, layer = "bts_representative_sf")

length(levels(as.factor(bts_representative$stratum))) # suggests that 121 strata surveyed

## Strata boundary data along with calculating total area in square nautical miles
## Note: nautical miles change depending on location in the world, so these are
##       really estimates on area (some have defined it as exactly 1852 meters
##        = ~1.1508 statute miles, according to NOAA)
strata <- st_read(dsn = strata_dir, layer = "bts_strata") %>%
  # only for the northern strata (data where region is equal to 1 or 3) [alternative is Region <= 3]
  dplyr::filter(Region == 1 | Region == 3) %>%
  # calculate the area, which will be in square meters, for each unique stratum using the Strata_Hist object
  dplyr::mutate(area = as.numeric(st_area(.)),
                # convert area to square nautical miles [1 sq nm = 3,429,904 sq meters]
                sq_nm = area / 3429904,
                # remove "-" from strata field to be able to later join with the stratum field for catch data
                stratum = gsub("\\-", "", Strata)) %>%
  # recode two strata to correct for typing errors
  dplyr::mutate(stratum = recode(stratum,
                                 "3050" = "03050",
                                 "3080" = "03080",
                                 "3110" = "03110")) %>%
  # rename fields for easier understanding and use
  dplyr::rename(strata_number = Strata_Num,
                region = Region,
                sector = Sector,
                sample_frequency = Samp_Freq,
                depth_meters = Depth_m,
                earliest_tow = earliest_t,
                latest_tow = latest_tow) %>%
  # select fields of interest
  dplyr::select(stratum,
                region,
                sector,
                sq_nm,
                depth_meters,
                sample_frequency,
                earliest_tow,
                latest_tow)

# 
# unique(strata$region)
# str(strata)
# View(strata)
# length(levels(as.factor(strata$stratum))) # suggests 147 exist (13 more than surveyed)
# 
# strata_qa_qc <- strata %>%
#   Read dplyr::group_by(stratum) %>%
#   dplyr::summarise(freq = n()) %>%
#   dplyr::filter(freq >= 2)
# 
# View(strata_qa_qc) # stratum 03920 appears twice, thus explaining why there is an extra stratum

###################################################
###################################################
###################################################

### Spatial functions
## Stratum
# bts_spatial_function <- function(bts_representative, strata){
#   # diversity for each stratum
#   spatial <- as.data.frame(bts_representative) %>%
#     # group data by stratum
#     dplyr::group_by(stratum,
#                     season) %>%
#     # calculate total unique number of caught species per stratum
#     dplyr::summarise(count = sum(count, na.rm = TRUE),
#                      diversity = n_distinct(species),
#                      weight = sum(weight, na.rm = TRUE),
#                      tows = length(unique(id))) %>%
#     # calculate catch per tow (mean catch per tow)
#     dplyr::mutate(count_per_tow = count / tows,
#                   weight_per_tow = weight / tows) %>%
#     # join with strata data to obtain corresponding spatial data
#     dplyr::full_join(strata,
#                       by = "stratum") %>%
#     # calculate the normalized diversity by area
#     dplyr::mutate(count_per_sqnm = count / sq_nm,
#                   diversity_per_sqnm = diversity / sq_nm,
#                   weight_per_sqnm = weight / sq_nm) %>%
#     # select fields of interest
#     dplyr::select(stratum, season,
#                   count, diversity, weight,
#                   tows, count_per_tow, weight_per_tow,
#                   sq_nm, count_per_sqnm, diversity_per_sqnm, weight_per_sqnm,
#                   geometry)
# 
#   return(spatial)
# }

## point
bts_spatial_function_point <- function(bts_representative, strata){
  # diversity for each stratum
  point <- bts_representative %>%
    # group data by stratum
    dplyr::group_by(stratum,
                    season) %>%
    # calculate total unique number of caught species per stratum
    dplyr::summarise(count = sum(count, na.rm = TRUE),
                     diversity = n_distinct(species),
                     weight = sum(weight, na.rm = TRUE),
                     tows = length(unique(id))) %>%
    # calculate catch per tow (mean catch per tow)
    dplyr::mutate(count_per_tow = count / tows,
                  weight_per_tow = weight / tows) %>%
    # # join with strata data to obtain corresponding spatial data
    # dplyr::inner_join(as.data.frame(strata),
    #                   by = "stratum") %>%
    # # calculate the normalized diversity by area
    # dplyr::mutate(count_per_sqnm = count / sq_nm,
    #               diversity_per_sqnm = diversity / sq_nm,
    #               weight_per_sqnm = weight / sq_nm) %>%
    # select fields of interest
    dplyr::select(stratum, season,
                  count, diversity, weight,
                  tows, count_per_tow, weight_per_tow,
                  # sq_nm, count_per_sqnm, diversity_per_sqnm, weight_per_sqnm,
                  geom)
  
  return(point)
}

# spatial_complete <- bts_spatial_function(bts_representative, strata)
bts_spatial <- bts_spatial_function_point(bts_representative, strata)

bts_spatial_csv <- as.data.frame(bts_spatial) %>%
  dplyr::select(-geom)

length(levels(as.factor(bts_spatial$stratum))) # 121 surveyed

#### Note:
## With a full_join there are 156 strata, however 10 are missing catch data;
## while another 9 are missing area values, suggesting the strata changed values at some point


## Catch missing strata: "01352" "03470" "03480" "03500" "03530" "03570" "03620" "03670" "03700" "03908"
## Area missing strata: "01350" "01410" "01420" "01490" "01990" "03210" "03910" "03940" "03990"

###################################################
###################################################
###################################################

### Export data
## as a RDS file (to open in R)
# saveRDS(strata, file = paste0(spatial_dir, "/", "strata.rds"))

# saveRDS(spatial_complete, file = paste0(export_dir, "/", "spatial_normalized_strata.rds"))
saveRDS(bts_spatial, file = paste0(rds_dir, "/", "bts_spatial.rds"))
write.csv(bts_spatial_csv, file = paste0(export_dir, "/", "bts_spatial_data.csv"))

## within a geopackage (to open up in other spatial software)
st_write(obj = strata, dsn = bts_geopackage, layer = "strata", append = F)
# st_write(obj = spatial_complete, dsn = bts_geopackage, layer = "bts_spatial_polygon", append = F)
st_write(obj = bts_spatial, dsn = bts_geopackage, layer = "bts_spatial", append = F)
