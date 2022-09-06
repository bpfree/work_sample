#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

####################
# Habitat area
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

## 1a. Set the directories where the raw habitat area data are currently stored
eez_dir <- "country_projects\\phl\\data\\b_clean_data"

habitat_area_dir <- "country_projects\\phl\\data\\a_raw_data\\habitat_area"

mangrove_dir <- "country_projects\\phl\\data\\a_raw_data\\mangrove"
coral_dir <- "country_projects\\phl\\data\\a_raw_data\\coral_reef"
seagrass_dir <- "country_projects\\phl\\data\\a_raw_data\\seagrass"

## 1b. setting output directories
clean_dir <- "country_projects\\phl\\data\\b_clean_data"
analyze_dir <- "country_projects\\phl\\data\\c_analyze_data"
tool_dir <- "country_projects\\phl\\data\\d_y_data"


## 1c. inspect the directories
list.files(eez_dir)

list.files(habitat_area_dir)

list.files(mangrove_dir)
list.files(coral_dir)
list.files(seagrass_dir)

######################################################
######################################################

### 2. load the data

## 2a. habitat data -- Philippines coral reef and mangrove data
mangrove_gmw <- st_read(dsn = mangrove_dir, layer = "GMW_2016_v2") # Global Mangrove Watch mangrove data
coral_wcmc <- st_read(dsn = coral_dir, layer = "coral_reef_wcmc") # UNEP-WCMC coral reef data

## 2b. habitat area per area of interest
# antique_habitat_area <- read.csv(paste(habitat_area_dir, "antique_habitat_area.csv", sep="/"), as.is = T)
# camotes_habitat_area <- read.csv(paste(habitat_area_dir, "camotes_habitat_area.csv", sep="/"), as.is = T)
# escalante_habitat_area <- read.csv(paste(habitat_area_dir, "escalante_habitat_area.csv", sep="/"), as.is = T)
# siargao_habitat_area <- read.csv(paste(habitat_area_dir, "siargao_habitat_area.csv", sep="/"), as.is = T)

antique_habitat_area <- read.csv(paste(habitat_area_dir, "antique_habitat_area_mean.csv", sep="/"), as.is = T)
camotes_habitat_area <- read.csv(paste(habitat_area_dir, "camotes_habitat_area_mean.csv", sep="/"), as.is = T)
escalante_habitat_area <- read.csv(paste(habitat_area_dir, "escalante_habitat_area_mean.csv", sep="/"), as.is = T)
siargao_habitat_area <- read.csv(paste(habitat_area_dir, "siargao_habitat_area_mean.csv", sep="/"), as.is = T)

## 2c. participatory habitat area
antique_habitat_participatory <- st_read(dsn = habitat_area_dir, layer = "antique_habitat_participatory")
camotes_habitat_participatory <- st_read(dsn = habitat_area_dir, layer = "camotes_habitat_participatory")
escalante_habitat_participatory <- st_read(dsn = habitat_area_dir, layer = "escalante_city_habitat_participatory")
siargao_habitat_participatory <- st_read(dsn = habitat_area_dir, layer = "siargao_habitat_participatory")
tsps_habitat_participatory <- st_read(dsn = habitat_area_dir, layer = "tsps_habitat_participatory")

## 2d. EEZ data for the Philippines
land_eez <- st_read(dsn = eez_dir, layer = "phl_land_eez") # union of EEZ and land (will help for getting mangroves)

######################################################
######################################################

### 3. Inspect the data (classes, crs, etc.)

## 3a. Examine the top of the data
head(mangrove_gmw)
head(coral_wcmc)

head(antique_habitat_area_mean)
head(camotes_habitat_area_mean)
head(escalante_habitat_area_mean)
head(siargao_habitat_area_mean)

head(antique_habitat_participatory)
head(camotes_habitat_participatory)
head(escalante_habitat_participatory)
head(siargao_habitat_participatory)
head(tsps_habitat_participatory)


## 3b. Inspect crs and set crs values if needed for later analyses
crs(mangrove_gmw)
crs(coral_wcmc)

crs(antique_habitat_area_mean)
crs(camotes_habitat_area_mean)
crs(escalante_habitat_area_mean)
crs(siargao_habitat_area_mean)

crs(antique_habitat_participatory)
crs(camotes_habitat_participatory)
crs(escalante_habitat_participatory)
crs(siargao_habitat_participatory)
crs(tsps_habitat_participatory)

######################################################
######################################################

### 4. Cleaning and preparing data

# 4a. Habitat areas combined with planning units
habitat_clean_function <- function(data){
  
  habitat_area_clean <- data %>%
    dplyr::mutate(mangrove_area_ha = 100 * as.numeric(Mangrove_Area),
                  reef_area_ha = 100 * as.numeric(Reef_Area),
                  seagrass_area_ha = 100 * as.numeric(Seagrass_Area),
                  total_area_ha = mangrove_area_ha + reef_area_ha + seagrass_area_ha) %>%
    dplyr::select(region, puid, mangrove_area_ha, reef_area_ha, seagrass_area_ha, total_area_ha, 
                  mean_coral_cover, mean_seagrass_cover, marxan_frequency)
  
  return(habitat_area_clean)
}

# Antique habitat
antique_habitat_area_clean <- habitat_clean_function(antique_habitat_area)

# Camotes habitat
camotes_habitat_area_clean <- habitat_clean_function(camotes_habitat_area)

# Escalante habitat
escalante_habitat_area_clean <- habitat_clean_function(escalante_habitat_area)

# Siargo habitat
siargao_habitat_area_clean <- habitat_clean_function(siargao_habitat_area)

# Combined habitat areas and planning units
# phl_habitat_area <- rbind(antique_habitat_area_clean,
#                           camotes_habitat_area_clean,
#                           escalante_habitat_area_clean,
#                           siargao_habitat_area_clean)



# 4b. Participatory habitat data

# preparing TSPS data
tsps_habitat_participatory <- tsps_habitat_participatory %>%
  dplyr::rename(Habitat = des) %>%
  dplyr::select(-Area_ha)

# preparing Escalante data
escalante_habitat_participatory <- escalante_habitat_participatory %>%
  dplyr::select(Habitat)

# coral
coral_function <- function(data){
  
  coral_area <- data %>%
    dplyr::mutate(iso3 = "PHL") %>%
    dplyr::rename(habitat = Habitat) %>%
    dplyr::filter(habitat == "Coral reef") %>%
    dplyr::select(iso3, habitat)
  
  return(coral_area)
}

antique_coral <- coral_function(antique_habitat_participatory)
camotes_coral <- coral_function(camotes_habitat_participatory)
escalante_coral <- coral_function(escalante_habitat_participatory)
siargao_coral <- coral_function(siargao_habitat_participatory)
tsps_coral <- coral_function(tsps_habitat_participatory)

coral_participatory <- rbind(antique_coral,
                             camotes_coral,
                             escalante_coral,
                             siargao_coral,
                             tsps_coral)


# mangrove
mangrove_function <- function(data){
  
  mangrove_area <- data %>%
    dplyr::mutate(iso3 = "PHL") %>%
    dplyr::rename(habitat = Habitat) %>%
    dplyr::filter(habitat == "Mangroves") %>%
    dplyr::mutate(habitat = recode(habitat, "Mangroves" = "Mangrove")) %>%
    dplyr::select(iso3, habitat)
  
  return(mangrove_area)
}

antique_mangrove <- mangrove_function(antique_habitat_participatory)
camotes_mangrove <- mangrove_function(camotes_habitat_participatory)
escalante_mangrove <- mangrove_function(escalante_habitat_participatory)
siargao_mangrove <- mangrove_function(siargao_habitat_participatory)
tsps_mangrove <- mangrove_function(tsps_habitat_participatory)

mangrove_participatory <- rbind(antique_mangrove,
                                camotes_mangrove,
                                escalante_mangrove,
                                siargao_mangrove,
                                tsps_mangrove)

# seagrass
seagrass_function <- function(data){
  
  seagrass_area <- data %>%
    dplyr::mutate(iso3 = "PHL") %>%
    dplyr::rename(habitat = Habitat) %>%
    dplyr::filter(habitat == "Seagrass") %>%
    dplyr::select(iso3, habitat)
  
  return(seagrass_area)
}

antique_seagrass <- seagrass_function(antique_habitat_participatory)
camotes_seagrass <- seagrass_function(camotes_habitat_participatory)
escalante_seagrass <- seagrass_function(escalante_habitat_participatory)
siargao_seagrass <- seagrass_function(siargao_habitat_participatory)
tsps_seagrass <- seagrass_function(tsps_habitat_participatory)

seagrass_participatory <- rbind(antique_seagrass,
                                camotes_seagrass,
                                escalante_seagrass,
                                siargao_seagrass,
                                tsps_seagrass)

######################################################
######################################################

### 5. Subsetting data to the EEZ and area of interest

## 5a. Mangroves found in the Philippines
phl_mangroves <- st_intersection(st_make_valid(mangrove_gmw), land_eez) %>%
  dplyr::mutate(habitat = "Mangrove") %>%
  dplyr::select(iso3, habitat)
head(phl_mangroves)

phl_coral <- st_intersection(st_make_valid(coral_wcmc), land_eez) %>%
  dplyr::mutate(habitat = "Coral reef", iso3 = "PHL") %>%
  dplyr::select(iso3, habitat)
head(phl_coral)

######################################################
######################################################

### 8. Saving the data to desired directories
st_write(obj = phl_mangroves, dsn = paste0(analyze_dir, "/", "mangrove.shp"), append = F)
st_write(obj = phl_coral, dsn = paste0(analyze_dir, "/", "reef.shp"), append = F)

st_write(obj = coral_participatory, dsn = paste0(analyze_dir, "/", "coral_participatory.shp"), append = F)
st_write(obj = mangrove_participatory, dsn = paste0(analyze_dir, "/", "mangrove_participatory.shp"), append = F)
st_write(obj = seagrass_participatory, dsn = paste0(analyze_dir, "/", "seagrass.shp"), append = F)

write.csv(antique_habitat_area_clean, file = paste0(clean_dir, "/", "antique_habitat_area.csv"))
write.csv(camotes_habitat_area_clean, file = paste0(clean_dir, "/", "camotes_habitat_area.csv"))
write.csv(escalante_habitat_area_clean, file = paste0(clean_dir, "/", "escalante_habitat_area.csv"))
write.csv(siargao_habitat_area_clean, file = paste0(clean_dir, "/", "siargao_habitat_area.csv"))

# write.csv(phl_habitat_area, file = paste0(output_dir, "/", "habitat_area.csv"))
