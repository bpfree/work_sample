#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

#####################
# Larval migration
#####################

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

## 1a. Set the directories where the larval migration raw data are currently stored for each 
migration_dir <- "country_projects\\phl\\data\\a_raw_data\\larval_migration"

## 1b. setting output directories
analyze_dir <- "country_projects\\phl\\data\\c_analyze_data"


## 1c. inspect the larval migration directory
list.files(migration_dir)

######################################################
######################################################

### 2. load the data

## 2f. larval migration data
# Acanthurus lineatus
aline_migration <- st_read(dsn = migration_dir, layer = "aline_larval_migration")

# Caesio caerulauren
ccaer_migration <- st_read(dsn = migration_dir, layer = "ccaer_larval_migration")

# Chlorurus bleekeri
cblee_migration <- st_read(dsn = migration_dir, layer = "cblee_larval_migration")

# Lutjanus ehrenbergii
lehre_migration <- st_read(dsn = migration_dir, layer = "lehre_larval_migration")

# Naso mino
nmino_migration <- st_read(dsn = migration_dir, layer = "nmino_larval_migration")

# Parupeneus multifasciatus
pmult_migration <- st_read(dsn = migration_dir, layer = "pmult_larval_migration")

# Plectropomus leopardus
pleop_migration <- st_read(dsn = migration_dir, layer = "pleop_larval_migration")

# Pterocaesio pisang
ppisa_migration <- st_read(dsn = migration_dir, layer = "ppisa_larval_migration")

# Scarus globiceps
sglob_migration <- st_read(dsn = migration_dir, layer = "sglob_larval_migration")

# Siganus canaliculatus
scana_migration <- st_read(dsn = migration_dir, layer = "aline_larval_migration")

######################################################
######################################################

### 3. Inspecting the data
head(aline_migration)
head(ccaer_migration)
head(cblee_migration)
head(lehre_migration)
head(nmino_migration)
head(pmult_migration)
head(pleop_migration)
head(ppisa_migration)
head(sglob_migration)
head(scana_migration)



## 3a. Checking the CRS
crs(aline_migration)
crs(ccaer_migration)
crs(cblee_migration)
crs(lehre_migration)
crs(nmino_migration)
crs(pmult_migration)
crs(pleop_migration)
crs(ppisa_migration)
crs(sglob_migration)
crs(scana_migration)

######################################################
######################################################

### 4. Cleaning and preparing data

## 4a. Import data

import_clean_function <- function(data){
  
  import_data_cleaned <- data %>%
    dplyr::select(M, FromID) %>%
    dplyr::rename(import = FromID, migration = M) %>%
    dplyr::mutate(iso3 = "PHL") %>%
    dplyr::relocate(iso3, .after = import)
  
  return(import_data_cleaned)
}

aline_migration_import <- import_clean_function(aline_migration) %>%
  dplyr::mutate(species = "Acanthurus lineatus") %>%
  dplyr::relocate(species, .after = import)
ccaer_migration_import <- import_clean_function(ccaer_migration) %>%
  dplyr::mutate(species = "Caesio caerulaurea") %>%
  dplyr::relocate(species, .after = import)
cblee_migration_import <- import_clean_function(cblee_migration) %>%
  dplyr::mutate(species = "Chlorurus bleekeri") %>%
  dplyr::relocate(species, .after = import)
lehre_migration_import <- import_clean_function(lehre_migration) %>%
  dplyr::mutate(species = "Lutjanus ehrenbergii") %>%
  dplyr::relocate(species, .after = import)
nmino_migration_import <- import_clean_function(nmino_migration) %>%
  dplyr::mutate(species = "Naso minor") %>%
  dplyr::relocate(species, .after = import)
pmult_migration_import <- import_clean_function(pmult_migration) %>%
  dplyr::mutate(species = "Parupeneus multifasciatus") %>%
  dplyr::relocate(species, .after = import)
pleop_migration_import <- import_clean_function(pleop_migration) %>%
  dplyr::mutate(species = "Plectropomus leopardus") %>%
  dplyr::relocate(species, .after = import)
ppisa_migration_import <- import_clean_function(ppisa_migration) %>%
  dplyr::mutate(species = "Pterocaesio pisang") %>%
  dplyr::relocate(species, .after = import)
sglob_migration_import <- import_clean_function(sglob_migration) %>%
  dplyr::mutate(species = "Scarus globiceps") %>%
  dplyr::relocate(species, .after = import)
scana_migration_import <- import_clean_function(scana_migration) %>%
  dplyr::mutate(species = "Siganus canaliculatus") %>%
  dplyr::relocate(species, .after = import)

## 6a. Export data
export_clean_function <- function(data){
  
  export_data_cleaned <- data %>%
    dplyr::select(M, ToID) %>%
    dplyr::rename(export = ToID, migration = M) %>%
    dplyr::mutate(iso3 = "PHL") %>%
    dplyr::relocate(iso3, .after = export)
  
  return(export_data_cleaned)
}

aline_migration_export <- export_clean_function(aline_migration) %>%
  dplyr::mutate(species = "Acanthurus lineatus") %>%
  dplyr::relocate(species, .after = export)
ccaer_migration_export <- export_clean_function(ccaer_migration) %>%
  dplyr::mutate(species = "Caesio caerulaurea") %>%
  dplyr::relocate(species, .after = export)
cblee_migration_export <- export_clean_function(cblee_migration) %>%
  dplyr::mutate(species = "Chlorurus bleekeri") %>%
  dplyr::relocate(species, .after = export)
lehre_migration_export <- export_clean_function(lehre_migration) %>%
  dplyr::mutate(species = "Lutjanus ehrenbergii") %>%
  dplyr::relocate(species, .after = export)
nmino_migration_export <- export_clean_function(nmino_migration) %>%
  dplyr::mutate(species = "Naso minor") %>%
  dplyr::relocate(species, .after = export)
pmult_migration_export <- export_clean_function(pmult_migration) %>%
  dplyr::mutate(species = "Parupeneus multifasciatus") %>%
  dplyr::relocate(species, .after = export)
pleop_migration_export <- export_clean_function(pleop_migration) %>%
  dplyr::mutate(species = "Plectropomus leopardus") %>%
  dplyr::relocate(species, .after = export)
ppisa_migration_export <- export_clean_function(ppisa_migration) %>%
  dplyr::mutate(species = "Pterocaesio pisang") %>%
  dplyr::relocate(species, .after = export)
sglob_migration_export <- export_clean_function(sglob_migration) %>%
  dplyr::mutate(species = "Scarus globiceps") %>%
  dplyr::relocate(species, .after = export)
scana_migration_export <- export_clean_function(scana_migration) %>%
  dplyr::mutate(species = "Siganus canaliculatus") %>%
  dplyr::relocate(species, .after = export)

######################################################
######################################################

### 5. Saving to output directory
st_write(obj = aline_migration_import, dsn = paste0(analyze_dir, "/", "aline_migration_import.shp"), append = F)
st_write(obj = aline_migration_export, dsn = paste0(analyze_dir, "/", "aline_migration_export.shp"), append = F)

st_write(obj = ccaer_migration_import, dsn = paste0(analyze_dir, "/", "ccaer_migration_import.shp"), append = F)
st_write(obj = ccaer_migration_export, dsn = paste0(analyze_dir, "/", "ccaer_migration_export.shp"), append = F)

st_write(obj = cblee_migration_import, dsn = paste0(analyze_dir, "/", "cblee_migration_import.shp"), append = F)
st_write(obj = cblee_migration_export, dsn = paste0(analyze_dir, "/", "cblee_migration_export.shp"), append = F)

st_write(obj = lehre_migration_import, dsn = paste0(analyze_dir, "/", "lehre_migration_import.shp"), append = F)
st_write(obj = lehre_migration_export, dsn = paste0(analyze_dir, "/", "lehre_migration_export.shp"), append = F)

st_write(obj = nmino_migration_import, dsn = paste0(analyze_dir, "/", "nmino_migration_import.shp"), append = F)
st_write(obj = nmino_migration_export, dsn = paste0(analyze_dir, "/", "nmino_migration_export.shp"), append = F)

st_write(obj = pmult_migration_import, dsn = paste0(analyze_dir, "/", "pmult_migration_import.shp"), append = F)
st_write(obj = pmult_migration_export, dsn = paste0(analyze_dir, "/", "pmult_migration_export.shp"), append = F)

st_write(obj = pleop_migration_import, dsn = paste0(analyze_dir, "/", "pleop_migration_import.shp"), append = F)
st_write(obj = pleop_migration_export, dsn = paste0(analyze_dir, "/", "pleop_migration_export.shp"), append = F)

st_write(obj = ppisa_migration_import, dsn = paste0(analyze_dir, "/", "ppisa_migration_import.shp"), append = F)
st_write(obj = ppisa_migration_export, dsn = paste0(analyze_dir, "/", "ppisa_migration_export.shp"), append = F)

st_write(obj = sglob_migration_import, dsn = paste0(analyze_dir, "/", "sglob_migration_import.shp"), append = F)
st_write(obj = sglob_migration_export, dsn = paste0(analyze_dir, "/", "sglob_migration_export.shp"), append = F)

st_write(obj = scana_migration_import, dsn = paste0(analyze_dir, "/", "scana_migration_import.shp"), append = F)
st_write(obj = scana_migration_export, dsn = paste0(analyze_dir, "/", "scana_migration_export.shp"), append = F)