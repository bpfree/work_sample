#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

####################
# Planning units
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
pu_dir <- "country_projects\\phl\\data\\a_raw_data\\planning_grid"

## 1b. setting output directory
clean_dir <- "country_projects\\phl\\data\\b_clean_data"

## 1c. inspect the directory
list.files(pu_dir)
list.files(marxan_dir)

######################################################
######################################################

### 2. load the data

## 2a. load planning grid units
antique_pu <- st_read(dsn = pu_dir, layer = "antique_pu")
cebu_pu <- st_read(dsn = pu_dir, layer = "cebu_pu")
negros_occidental_pu <- st_read(dsn = pu_dir, layer = "negros_occidental_pu")
surigao_del_norte_pu <- st_read(dsn = pu_dir, layer = "surigao_del_norte_pu")

######################################################
######################################################

### 3. Inspect the data (classes, crs, etc.)

## 3a. Examine the top of the data
head(antique_pu)
head(cebu_pu)
head(negros_occidental_pu)
head(surigao_del_norte_pu)


## 3b. Inspect crs and set crs values if needed for later analyses
crs(antique_pu)
crs(cebu_pu)
crs(negros_occidental_pu)
crs(surigao_del_norte_pu)

######################################################
######################################################

### 4. Cleaning and preparing data

## 4a. Philippines planning unit

## Function to clean the planning units
planning_unit_clean <- function(data){
  pu_clean <- data %>%
    dplyr::mutate(region = "Cebu") %>% ## update region name before running for each Cebu (Camotes), Negros Occidental (Escalante City), Surigao del Norte (Siargao)
    dplyr::mutate(iso3 = "PHL") %>%
    dplyr::rename(puid = PUID) %>%
    dplyr::select(puid,region,iso3)
  return(pu_clean)
}

# Antique planning units
antique_pu_clean <- antique_pu %>%
  dplyr::mutate(region = "Antique") %>%
  dplyr::mutate(iso3 = "PHL") %>%
  dplyr::rename(puid = PUID) %>%
  dplyr::select(-FID_1)

# Camotes planning units
cebu_pu_clean <- planning_unit_clean(cebu_pu)

# Escalante planning units
negros_occidental_pu_clean <- planning_unit_clean(negros_occidental_pu)

# Siargao planning units
surigao_del_norte_pu_clean <- planning_unit_clean(surigao_del_norte_pu)

# combined all planning units
phl_pu <- rbind(antique_pu_clean,
                cebu_pu_clean,
                negros_occidental_pu_clean,
                surigao_del_norte_pu_clean)

x <- as.factor(phl_pu$region)
levels(x)

######################################################
######################################################

### 8. Saving to compiled directory
st_write(obj = phl_pu, dsn = paste0(clean_dir, "/", "phl_pu.shp"), append = F)

# st_write(obj = antique_pu_clean, dsn = paste0(clean_dir, "/", "antique_pu.shp"), append = F)
# st_write(obj = cebu_pu_clean, dsn = paste0(clean_dir, "/", "cebu_pu.shp"), append = F)
# st_write(obj = negros_occidental_pu_clean, dsn = paste0(clean_dir, "/", "negros_occidental_pu.shp"), append = F)
# st_write(obj = surigao_del_norte_pu_clean, dsn = paste0(clean_dir, "/", "surigao_del_norte_pu.shp"), append = F)
