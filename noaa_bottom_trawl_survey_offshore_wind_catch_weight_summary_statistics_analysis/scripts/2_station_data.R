###################################################
###################################################
###################################################


######### Part 2 #########
## Station data
##########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
data_dir <- "data\\a_raw_data"
bts_dir <- "data\\b_bts_data"
list.files(data_dir)

# data parameters
preHB_year <- 2008
HB_year <- 2009

shg_code <- 136
toga_code <- 1324
not_toga <- c(1141,1142,1241,1242)

begin_year <- 1963
end_year <- 2021

months <- c("03|04|05|09|10|11") # Spring (March, April, May) and Fall (September, October, November)
vessels <- c("AL|DE|HB|PC") # Albatross IV, Delaware, Henry Bigelow, Pisces

###################################################
###################################################
###################################################

#### Load complete raw data (cruise, catch, station)
#### Read RDS files
station <- readRDS(paste(data_dir, "station.rds", sep = "/"))

###################################################
###################################################
###################################################

station_north <- station %>%
  # strata that are in the northern area (where wind farms are located)
  # northern stratum start with either 01 or 03 (01 = offshore, 03 = inshore)
  dplyr::filter(str_detect(stratum, "^01|^03")) %>%
  # years of interest
  dplyr::filter(year >= begin_year &
                year <= end_year) %>%
  # audited tows
  dplyr::filter(status == 10) %>%
  # seasons of interest
  dplyr::filter(str_detect(month, months)) %>%
  # vessels of interest
  dplyr::filter(str_detect(vessel, vessels))

preHB_station <- station_north %>%
  # tows would have occurred before 2008
  dplyr::filter(year <= preHB_year) %>%
  # representative tows are any that were less than 136
  dplyr::filter(shg <= shg_code)

HB_station <- station_north %>%
  # tows would have occurred since 2009
  dplyr::filter(year >= HB_year) %>%
  # representative tows are ones that are less than 1324
  dplyr::filter(toga <= toga_code) %>%
  # but not equal to 1141, 1142, 1241, 1242 (gear codes to be 2 or less)
  dplyr::filter(!toga %in% not_toga)

station_representative <- rbind(preHB_station,
                                HB_station)

###################################################
###################################################
###################################################

### testing data
test_year <- station_representative %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(count = n_distinct(station))

test_cruise <- station_representative %>%
  dplyr::group_by(cruise,
                  vessel) %>%
  dplyr::summarise(count = n_distinct(station))

###################################################
###################################################
###################################################


## Export data
# Subsetted data as RDS format for opening in R
saveRDS(station_north, file = paste0(bts_dir, "/", "station_north.rds"))
saveRDS(station_representative, file = paste0(bts_dir, "/", "station_representative.rds"))

saveRDS(preHB_station, file = paste0(bts_dir, "/", "preHB_station.rds"))
saveRDS(HB_station, file = paste0(bts_dir, "/", "HB_station.rds"))

## data for Jakub
write.csv(station_representative, file = paste0(bts_dir, "/", "station_representative_jakub.csv"))
write.csv(test_year, file = paste0(bts_dir, "/", "year_station_jakub.csv"))
write.csv(test_cruise, file = paste0(bts_dir, "/", "cruise_station_jakub.csv"))
