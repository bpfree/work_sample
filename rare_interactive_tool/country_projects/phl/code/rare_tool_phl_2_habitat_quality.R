#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

####################
# Habitat quality
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

## 1a. Set the directories where the raw habitat quality data are currently stored for each 
habitat_quality_dir <- "country_projects\\phl\\data\\a_raw_data\\habitat_quality"

## 1b. setting output directory
clean_dir <- "country_projects\\phl\\data\\b_clean_data"

## 1c. inspect the directories
list.files(habitat_quality_dir)


### 2. load the data

## 2a. habitat quality data
antique <- st_read(dsn = habitat_quality_dir, layer = "phl_antique_mantatow")
camotes <- st_read(dsn = habitat_quality_dir, layer = "phl_camotes_mantatow")
escalantecity <- st_read(dsn = habitat_quality_dir, layer = "phl_escalantecity_mantatow")
siargao <- st_read(dsn = habitat_quality_dir, layer = "phl_siargao_mantatow")

pu_mean <- read.csv(paste(habitat_quality_dir, "phl_habitat_quality.csv", sep = "/"), as.is = T)


######################################################
######################################################

### 3. Inspect the data (classes, crs, etc.)

## 3a. Examine the top of the data
head(antique)
head(camotes)
head(escalantecity)
head(siargao)

head(pu_mean)


## 3b. Inspect crs and set crs values if needed for later analyses
crs(antique)
crs(camotes)
crs(escalantecity)
crs(siargao)

######################################################
######################################################

### 4. Cleaning and preparing data

## 4a. Philippines habitat quality data

antique_habitat_quality <- antique %>%
  dplyr::mutate(LC = as.numeric(LC)) %>%
  dplyr::mutate(total_coral = LC) %>%
  dplyr::select(total_coral)

camotes_habitat_quality <- camotes %>%
  dplyr::mutate(Live_coral = as.numeric(Live_coral)) %>%
  dplyr::mutate(Soft_coral = as.numeric(Soft_coral)) %>%
  dplyr::mutate(total_coral = Live_coral+Soft_coral) %>%
  dplyr::select(total_coral)

escalantecity_habitat_quality <- escalantecity %>%
  dplyr::mutate(LHC = as.numeric(LHC)) %>%
  dplyr::mutate(SC = as.numeric(SC)) %>%
  dplyr::mutate(total_coral = LHC + SC) %>%
  dplyr::select(total_coral)

siargao_habitat_quality <- siargao %>%
  dplyr::mutate(total_coral = Hard_coral + Soft_coral) %>%
  dplyr::select(total_coral)

# Seagrass data
seagrass_quality <- siargao %>%
  dplyr::rename(total_seagrass = Other) %>%
  dplyr::mutate(iso3 = "PHL") %>%
  dplyr::select(total_seagrass, iso3)

## 4b. Philippines habitat quality mean data
pu_mean_clean <- pu_mean %>%
  dplyr::mutate(region = str_to_lower(region)) %>%
  dplyr::rename(puid = PUID) %>%
  dplyr::mutate(region = recode(region, "cebu" =  "camotes"))

######################################################
######################################################

### 5. Saving to compiled directory
st_write(obj = antique_habitat_quality, dsn = paste0(clean_dir, "/", "antique_habitat.shp"), append = F)
st_write(obj = camotes_habitat_quality, dsn = paste0(clean_dir, "/", "camotes_habitat.shp"), append = F)
st_write(obj = escalantecity_habitat_quality, dsn = paste0(clean_dir, "/", "escalante_city_habitat.shp"), append = F)
st_write(obj = siargao_habitat_quality, dsn = paste0(clean_dir, "/", "siargao_habitat.shp"), append = F)

st_write(seagrass_quality, dsn = paste0(clean_dir, "/", "habitat_quality_seagrass.shp"), append = F)

write.csv(pu_mean_clean, file = paste0(clean_dir, "/", "pu_mean.csv"))

