#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

#####################
# Larval connectivity
#####################

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

## 1a. Set the directories where the larval connectivity raw data are currently stored for each 
larval_dir <- "country_projects\\phl\\data\\a_raw_data\\larval_connectivity"

## 1b. setting output directories
analyze_dir <- "country_projects\\phl\\data\\c_analyze_data"


## 1c. inspect the larval connectivity directory
list.files(larval_dir)

######################################################
######################################################

### 2. load the data

## 2f. larval connectivity data
# Acanthurus lineatus
aline_import <- st_read(dsn = larval_dir, layer = "aline_import")
aline_export <- st_read(dsn = larval_dir, layer = "aline_export")

# Caesio caerulauren
ccaer_import <- st_read(dsn = larval_dir, layer = "ccaer_import")
ccaer_export <- st_read(dsn = larval_dir, layer = "ccaer_export")

# Chlorurus bleekeri
cblee_import <- st_read(dsn = larval_dir, layer = "cblee_import")
cblee_export <- st_read(dsn = larval_dir, layer = "cblee_export")

# Lutjanus ehrenbergii
lehre_import <- st_read(dsn = larval_dir, layer = "lehre_import")
lehre_export <- st_read(dsn = larval_dir, layer = "lehre_export")

# Naso mino
nmino_import <- st_read(dsn = larval_dir, layer = "nmino_import")
nmino_export <- st_read(dsn = larval_dir, layer = "nmino_export")

# Parupeneus multifasciatus
pmult_import <- st_read(dsn = larval_dir, layer = "pmult_import")
pmult_export <- st_read(dsn = larval_dir, layer = "pmult_export")

# Plectropomus leopardus
pleop_import <- st_read(dsn = larval_dir, layer = "pleop_import")
pleop_export <- st_read(dsn = larval_dir, layer = "pleop_export")

# Pterocaesio pisang
ppisa_import <- st_read(dsn = larval_dir, layer = "ppisa_import")
ppisa_export <- st_read(dsn = larval_dir, layer = "ppisa_export")

# Scarus globiceps
sglob_import <- st_read(dsn = larval_dir, layer = "sglob_import")
sglob_export <- st_read(dsn = larval_dir, layer = "sglob_export")

# Siganus canaliculatus
scana_import <- st_read(dsn = larval_dir, layer = "scana_import")
scana_export <- st_read(dsn = larval_dir, layer = "scana_export")

######################################################
######################################################

### 3. Inspecting the data
head(aline_import)
head(aline_export)
head(ccaer_import)
head(ccaer_export)
head(cblee_import)
head(cblee_export)
head(lehre_export)
head(lehre_import)
head(nmino_import)
head(nmino_export)
head(pmult_import)
head(pmult_export)
head(pleop_import)
head(pleop_export)
head(ppisa_import)
head(ppisa_export)
head(sglob_import)
head(sglob_export)
head(scana_import)
head(scana_export)


## 3a. Checking the CRS
crs(aline_import)
crs(aline_export)
crs(ccaer_import)
crs(ccaer_export)
crs(cblee_import)
crs(cblee_export)
crs(lehre_export)
crs(lehre_import)
crs(nmino_import)
crs(nmino_export)
crs(pmult_import)
crs(pmult_export)
crs(pleop_import)
crs(pleop_export)
crs(ppisa_import)
crs(ppisa_export)
crs(sglob_import)
crs(sglob_export)
crs(scana_import)
crs(scana_export)

######################################################
######################################################

### 4. Cleaning and preparing data

## 4a. Import data

import_clean_function <- function(data){
  
  import_data_cleaned <- data %>%
    dplyr::select(wInDeg) %>%
    dplyr::rename(import = wInDeg) %>%
    dplyr::mutate(iso3 = "PHL") %>%
    dplyr::relocate(iso3, .after = import)
  
  return(import_data_cleaned)
}

aline_import_clean <- import_clean_function(aline_import) %>%
  dplyr::mutate(species = "Acanthurus lineatus") %>%
  dplyr::relocate(species, .after = import)
ccaer_import_clean <- import_clean_function(ccaer_import) %>%
  dplyr::mutate(species = "Caesio caerulaurea") %>%
  dplyr::relocate(species, .after = import)
cblee_import_clean <- import_clean_function(cblee_import) %>%
  dplyr::mutate(species = "Chlorurus bleekeri") %>%
  dplyr::relocate(species, .after = import)
lehre_import_clean <- import_clean_function(lehre_import) %>%
  dplyr::mutate(species = "Lutjanus ehrenbergii") %>%
  dplyr::relocate(species, .after = import)
nmino_import_clean <- import_clean_function(nmino_import) %>%
  dplyr::mutate(species = "Naso minor") %>%
  dplyr::relocate(species, .after = import)
pmult_import_clean <- import_clean_function(pmult_import) %>%
  dplyr::mutate(species = "Parupeneus multifasciatus") %>%
  dplyr::relocate(species, .after = import)
pleop_import_clean <- import_clean_function(pleop_import) %>%
  dplyr::mutate(species = "Plectropomus leopardus") %>%
  dplyr::relocate(species, .after = import)
ppisa_import_clean <- import_clean_function(ppisa_import) %>%
  dplyr::mutate(species = "Pterocaesio pisang") %>%
  dplyr::relocate(species, .after = import)
sglob_import_clean <- import_clean_function(sglob_import) %>%
  dplyr::mutate(species = "Scarus globiceps") %>%
  dplyr::relocate(species, .after = import)
scana_import_clean <- import_clean_function(scana_import) %>%
  dplyr::mutate(species = "Siganus canaliculatus") %>%
  dplyr::relocate(species, .after = import)

## 6a. Export data
export_clean_function <- function(data){
  
  export_data_cleaned <- data %>%
    dplyr::select(ScrInf) %>%
    dplyr::rename(export = ScrInf) %>%
    dplyr::mutate(iso3 = "PHL") %>%
    dplyr::relocate(iso3, .after = export)
  
  return(export_data_cleaned)
}

aline_export_clean <- export_clean_function(aline_export) %>%
  dplyr::mutate(species = "Acanthurus lineatus") %>%
  dplyr::relocate(species, .after = export)
ccaer_export_clean <- export_clean_function(ccaer_export) %>%
  dplyr::mutate(species = "Caesio caerulaurea") %>%
  dplyr::relocate(species, .after = export)
cblee_export_clean <- export_clean_function(cblee_export) %>%
  dplyr::mutate(species = "Chlorurus bleekeri") %>%
  dplyr::relocate(species, .after = export)
lehre_export_clean <- export_clean_function(lehre_export) %>%
  dplyr::mutate(species = "Lutjanus ehrenbergii") %>%
  dplyr::relocate(species, .after = export)
nmino_export_clean <- export_clean_function(nmino_export) %>%
  dplyr::mutate(species = "Naso minor") %>%
  dplyr::relocate(species, .after = export)
pmult_export_clean <- export_clean_function(pmult_export) %>%
  dplyr::mutate(species = "Parupeneus multifasciatus") %>%
  dplyr::relocate(species, .after = export)
pleop_export_clean <- export_clean_function(pleop_export) %>%
  dplyr::mutate(species = "Plectropomus leopardus") %>%
  dplyr::relocate(species, .after = export)
ppisa_export_clean <- export_clean_function(ppisa_export) %>%
  dplyr::mutate(species = "Pterocaesio pisang") %>%
  dplyr::relocate(species, .after = export)
sglob_export_clean <- export_clean_function(sglob_export) %>%
  dplyr::mutate(species = "Scarus globiceps") %>%
  dplyr::relocate(species, .after = export)
scana_export_clean <- export_clean_function(scana_export) %>%
  dplyr::mutate(species = "Siganus canaliculatus") %>%
  dplyr::relocate(species, .after = export)

######################################################
######################################################

### 5. Saving to output directory
st_write(obj = aline_import_clean, dsn = paste0(analyze_dir, "/", "aline_import.shp"), append = F)
st_write(obj = aline_export_clean, dsn = paste0(analyze_dir, "/", "aline_export.shp"), append = F)

st_write(obj = ccaer_import_clean, dsn = paste0(analyze_dir, "/", "ccaer_import.shp"), append = F)
st_write(obj = ccaer_export_clean, dsn = paste0(analyze_dir, "/", "ccaer_export.shp"), append = F)

st_write(obj = cblee_import_clean, dsn = paste0(analyze_dir, "/", "cblee_import.shp"), append = F)
st_write(obj = cblee_export_clean, dsn = paste0(analyze_dir, "/", "cblee_export.shp"), append = F)

st_write(obj = lehre_import_clean, dsn = paste0(analyze_dir, "/", "lehre_import.shp"), append = F)
st_write(obj = lehre_export_clean, dsn = paste0(analyze_dir, "/", "lehre_export.shp"), append = F)

st_write(obj = nmino_import_clean, dsn = paste0(analyze_dir, "/", "nmino_import.shp"), append = F)
st_write(obj = nmino_export_clean, dsn = paste0(analyze_dir, "/", "nmino_export.shp"), append = F)

st_write(obj = pmult_import_clean, dsn = paste0(analyze_dir, "/", "pmult_import.shp"), append = F)
st_write(obj = pmult_export_clean, dsn = paste0(analyze_dir, "/", "pmult_export.shp"), append = F)

st_write(obj = pleop_import_clean, dsn = paste0(analyze_dir, "/", "pleop_import.shp"), append = F)
st_write(obj = pleop_export_clean, dsn = paste0(analyze_dir, "/", "pleop_export.shp"), append = F)

st_write(obj = ppisa_import_clean, dsn = paste0(analyze_dir, "/", "ppisa_import.shp"), append = F)
st_write(obj = ppisa_export_clean, dsn = paste0(analyze_dir, "/", "ppisa_export.shp"), append = F)

st_write(obj = sglob_import_clean, dsn = paste0(analyze_dir, "/", "sglob_import.shp"), append = F)
st_write(obj = sglob_export_clean, dsn = paste0(analyze_dir, "/", "sglob_export.shp"), append = F)

st_write(obj = scana_import_clean, dsn = paste0(analyze_dir, "/", "scana_import.shp"), append = F)
st_write(obj = scana_export_clean, dsn = paste0(analyze_dir, "/", "scana_export.shp"), append = F)