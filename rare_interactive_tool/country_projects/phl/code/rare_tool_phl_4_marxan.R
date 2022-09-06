#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

####################
# Marxan Frequency
####################

######################################################
######################################################

### 0. Preparing the environment and packages

# Clean environment
rm(list = ls())

# Preparing packages
if (!require("pacman")) install.packages("pacman")

# Load packages
pacman::p_load(berryFunctions,dplyr,raster,rgdal,sf,sp,stringr)

######################################################
######################################################

### 1. Setting up directories and loading the required data for analysis

## Make sure to copy a country's files are in the appropriate directory (managed access, coral, habitat quality, etc.)
## NOTE: This should be completed before running this code
## It will be preference if to have all the countries's data in a single main directory
## or in separate subdirectories
## Single directory will have lots of files, while separate directories will lead to have
## to setting up many directories in the code

## 1a. Set the directories where the raw planning unit data are currently stored for each 
marxan_dir <- "country_projects\\phl\\data\\a_raw_data\\marxan"

## 1b. setting output directory
analyze_dir <- "country_projects\\phl\\data\\c_analyze_data"

## 1c. inspect the directory
list.files(marxan_dir)

######################################################
######################################################

### 2. load the data

## 2a. load Marxan frequency data per managed access area
burgos_marxan <- st_read(dsn = marxan_dir, layer = "burgos_marxan") %>%
  dplyr::mutate(region = "Surigao del Norte",
                maa = "Burgos") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

city_of_escalante_marxan <- st_read(dsn = marxan_dir, layer = "city_of_escalante_marxan") %>%
  dplyr::mutate(region = "Negros Occidental",
                maa = "City of Escalante") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

general_luna_marxan <- st_read(dsn = marxan_dir, layer = "general_luna_marxan") %>%
  dplyr::mutate(region = "Surigao del Norte",
                maa = "General Luna") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

pandan_marxan <- st_read(dsn = marxan_dir, layer = "pandan_marxan") %>%
  dplyr::mutate(region = "Antique",
                maa = "Pandan") %>%
  dplyr::rename(marxan_frequency = frequency,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

pilar_cebu_marxan <- st_read(dsn = marxan_dir, layer = "pilar_cebu_marxan") %>%
  dplyr::mutate(region = "Cebu",
                maa = "Pilar") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

pilar_sdn_marxan <- st_read(dsn = marxan_dir, layer = "pilar_sdn_marxan") %>%
  dplyr::mutate(region = "Surigao del Norte",
                maa = "Pilar") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

poro_marxan <- st_read(dsn = marxan_dir, layer = "poro_marxan") %>%
  dplyr::mutate(region = "Cebu",
                maa = "Poro") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

san_benito_marxan <- st_read(dsn = marxan_dir, layer = "san_benito_marxan") %>%
  dplyr::mutate(region = "Surigao del Norte",
                maa = "San Benito") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

san_francisco_marxan <- st_read(dsn = marxan_dir, layer = "san_francisco_marxan") %>%
  dplyr::mutate(region = "Cebu",
                maa = "San Francisco") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

san_isidro_marxan <- st_read(dsn = marxan_dir, layer = "san_isidro_marxan") %>%
  dplyr::mutate(region = "Surigao del Norte",
                maa = "San Isidro") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

santa_monica_marxan <- st_read(dsn = marxan_dir, layer = "santa_monica_marxan") %>%
  dplyr::mutate(region = "Surigao del Norte",
                maa = "Santa Monica (Sapoa") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

# sebaste_marxan <- st_read(dsn = marxan_dir, layer = "sebaste_marxan") %>%
#   dplyr::mutate(region = "Antique",
#                 maa = "Sebaste") %>%
#   dplyr::rename(marxan_frequency = number,
#                 puid = PUID) %>%
#   dplyr::select(region, maa, puid, marxan_frequency)

socorro_marxan <- st_read(dsn = marxan_dir, layer = "socorro_marxan") %>%
  dplyr::mutate(region = "Surigao del Norte",
                maa = "Socorro") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

tudela_marxan <- st_read(dsn = marxan_dir, layer = "tudela_marxan") %>%
  dplyr::mutate(region = "Cebu",
                maa = "Tudela") %>%
  dplyr::rename(marxan_frequency = number,
                puid = PUID) %>%
  dplyr::select(region, maa, puid, marxan_frequency)

######################################################
######################################################

phl_marxan <- rbind(burgos_marxan,
                    city_of_escalante_marxan,
                    pandan_marxan,
                    pilar_cebu_marxan,
                    pilar_sdn_marxan,
                    poro_marxan,
                    san_benito_marxan,
                    san_francisco_marxan,
                    san_isidro_marxan,
                    santa_monica_marxan,
                    socorro_marxan,
                    tudela_marxan)

st_geometry(phl_marxan) <- NULL

View(phl_marxan)

write.csv(phl_marxan, file = paste0(analyze_dir, "/", "phl_marxan.csv"))
