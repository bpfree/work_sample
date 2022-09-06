#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

#############################
# Managed access and reserves
#############################

######################################################
######################################################

### 0. Preparing the environment and packages

# Clean environment
rm(list = ls())

# Preparing packages
if (!require("pacman")) install.packages("pacman")

# Load packages
pacman::p_load(berryFunctions,dplyr,raster,rgdal,sf,sp, stringr)

######################################################
######################################################

### 1. Setting up directories and loading the required data for analysis

## Make sure to copy a country's files are in the appropriate directory (managed access, coral, habitat quality, etc.)
## NOTE: This should be completed before running this code
## It will be preference if to have all the countries's data in a single main directory
## or in separate subdirectories
## Single directory will have lots of files, while separate directories will lead to have
## to setting up many directories in the code

## 1a. Set the directories where the raw managed access and reserve data are currently stored 
managed_access_dir <- "country_projects\\phl\\data\\a_raw_data\\managed_access_areas"
reserve_dir <- "country_projects\\phl\\data\\a_raw_data\\existing_reserve"

## 1b. setting output directories
tool_dir <- "country_projects\\phl\\data\\d_tool_data"


## 1c. inspect the directories
list.files(managed_access_dir)
list.files(reserve_dir)

######################################################
######################################################

### 2. load the data

## 2a. load managed access and reserve data
phl_ma <- st_read(dsn = managed_access_dir, layer = "phl_proposed_ma")
phl_reserves <- st_read(dsn = reserve_dir, layer = "phl_reserves_established")

######################################################
######################################################

### 3. Inspect the data (classes, crs, etc.)

## 3a. Examine the top of the data
head(phl_ma)
head(phl_reserves)

## 3b. Inspect crs and set crs values if needed for later analyses
crs(phl_ma)
crs(phl_reserves)


######################################################
######################################################

### 4. Cleaning and preparing data

## 4a. managed access areas
ma <- phl_ma %>%
  dplyr::mutate(iso3 = "PHL", country = "Philippines") %>%
  dplyr::select(iso3, country, MUNNAME, Area_ha, PROVNAME) %>%
  dplyr::mutate(MUNNAME = str_to_title(MUNNAME), PROVNAME = str_to_title(PROVNAME)) %>%
  dplyr::mutate(MUNNAME = recode(MUNNAME, "City Of Escalante" = "City of Escalante"),
                PROVNAME = recode(PROVNAME, "Surigao Del Norte" = "Surigao del Norte")) %>%
  dplyr::rename(region = PROVNAME, maa = MUNNAME, maa_area = Area_ha)

## 4b. reserves
reserve <- phl_reserves %>%
  dplyr::mutate(iso3 = "PHL") %>%
  dplyr::select(MPA_name, Area_ha, iso3) %>%
  dplyr::rename(reserve_name = MPA_name, area_ha = Area_ha)

######################################################
######################################################

### 8. Saving as a GeoPackage
st_write(obj = ma, dsn = paste0(tool_dir, "/", "managed_access_areas.shp"), append = F)
st_write(obj = reserve, dsn = paste0(tool_dir, "/", "existing_reserves.shp"), append = F)
