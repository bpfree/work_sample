###################################################
###################################################
###################################################


######### Part 3 #########
## Combining data
##########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
data_dir <- "data\\a_raw_data"
bts_dir <- "data\\b_bts_data"

# pre-determinants
seasons <- c("Spring|Fall")
months <- c("03|04|05|09|10|11")

###################################################
###################################################
###################################################

#### Load complete raw data (cruise, catch, station)
#### Read RDS files
cruise <- readRDS(paste(data_dir, "bts_cruises.rds", sep = "/")) %>%
  # select only cruises for spring and fall
  dplyr::filter(str_detect(season, seasons))
catch <- readRDS(paste(data_dir, "catch.rds", sep = "/"))
station <- readRDS(paste(bts_dir, "station_north.rds", sep = "/"))
station_representative <- readRDS(paste(bts_dir, "station_representative.rds", sep = "/"))

# length <- readRDS(paste(data_dir, "length.rds", sep = "/"))


###################################################
###################################################
###################################################

### Limiting data to only BTS cruises
bts_catch_season <- inner_join(catch,
                               cruise,
                               by = "cruise") %>%
  filter(str_detect(season, seasons))

# bts_length <- inner_join(length,
#                          cruise,
#                          by = "cruise")

bts_representative <- inner_join(bts_catch_season,
                                 station_representative) %>%
  dplyr::mutate(tow = as.numeric(tow),
                station = as.numeric(station),
                # change the species code from a character code to a numeric code,
                species = as.numeric(species),
                # change sex values (0 - 6) to be numeric
                sex = as.numeric(sex),
                # change the year so it is numeric
                year = as.numeric(year),
                # remove any white space before or after a species name if it exists
                species_name = str_trim(species_name)) %>%
  dplyr::relocate(vessel, .after = season)
  


# inspect number of tows by stations
tow_inspect <- bts_representative %>% 
  group_by(cruise,
           station) %>% 
  dplyr::summarise(count = n_distinct(tow)) %>%
  dplyr::filter(count >= 2)




bts_station_season <- inner_join(station,
                                 cruise,
                                 by = "cruise") %>%
  dplyr::select(-year.y) %>%
  dplyr::rename(year = year.x) %>%
  filter(str_detect(month, months))

bts_all <- inner_join(bts_catch_season,
                      bts_station_season) %>%
  # change key fields to numeric from character
  dplyr::mutate(tow = as.numeric(tow),
                station = as.numeric(station),
                # change the species code from a character code to a numeric code,
                species = as.numeric(species),
                # change sex values (0 - 6) to be numeric
                sex = as.numeric(sex),
                # change the year so it is numeric
                year = as.numeric(year),
                # remove any white space before or after a species name if it exists
                species_name = str_trim(species_name))

###################################################
###################################################
###################################################

## Export data
# Combined data as RDS format for opening in R
saveRDS(bts_catch_season, file = paste0(bts_dir, "/", "bts_catch.rds"))
#saveRDS(bts_length, file = paste0(bts_dir, "/", "bts_length.rds"))
saveRDS(bts_station_season, file = paste0(bts_dir, "/", "bts_station.rds"))

saveRDS(bts_representative, file = paste0(bts_dir, "/", "bts_representative.rds"))
saveRDS(bts_all, file = paste0(bts_dir, "/", "bts_all.rds"))
