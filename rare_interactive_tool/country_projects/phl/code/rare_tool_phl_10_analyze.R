#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

#####################
# Analyzing data
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
### Set directories
## Input directory
analyze_dir <- "country_projects\\phl\\data\\c_analyze_data"

## Output directories
tool_dir <- "country_projects\\phl\\data\\d_tool_data"


######################################################
######################################################

### 2. Larval connectivity
## Loading larval connectivity data
aline_connectivity_import <- st_read(dsn = analyze_dir, "aline_import")
aline_connectivity_export <- st_read(dsn = analyze_dir, "aline_export")

ccaer_connectivity_import <- st_read(dsn = analyze_dir, "ccaer_import")
ccaer_connectivity_export <- st_read(dsn = analyze_dir, "ccaer_export")

cblee_connectivity_import <- st_read(dsn = analyze_dir, "cblee_import")
cblee_connectivity_export <- st_read(dsn = analyze_dir, "cblee_export")

lehre_connectivity_import <- st_read(dsn = analyze_dir, "lehre_import")
lehre_connectivity_export <- st_read(dsn = analyze_dir, "lehre_export")

nmino_connectivity_import <- st_read(dsn = analyze_dir, "nmino_import")
nmino_connectivity_export <- st_read(dsn = analyze_dir, "nmino_export")

pmult_connectivity_import <- st_read(dsn = analyze_dir, "pmult_import")
pmult_connectivity_export <- st_read(dsn = analyze_dir, "pmult_export")

pleop_connectivity_import <- st_read(dsn = analyze_dir, "pleop_import")
pleop_connectivity_export <- st_read(dsn = analyze_dir, "pleop_export")

ppisa_connectivity_import <- st_read(dsn = analyze_dir, "ppisa_import")
ppisa_connectivity_export <- st_read(dsn = analyze_dir, "ppisa_export")

sglob_connectivity_import <- st_read(dsn = analyze_dir, "sglob_import")
sglob_connectivity_export <- st_read(dsn = analyze_dir, "sglob_export")

scana_connectivity_import <- st_read(dsn = analyze_dir, "scana_import")
scana_connectivity_export <- st_read(dsn = analyze_dir, "scana_export")


## Functions that generate and bind the quantile classifications of import / export values
# import functions
import_quantile_function <- function(data){
  
  import_data_quantiled <- data %>%
    classify(data,
             method = "quantile",
             breaks = 5)
  
  return(import_data_quantiled)
}

import_classify_function <- function(data,data2){
  
  import_data_classified <- data %>%
    cbind(data2$index) %>%
    dplyr::rename(quantile = data2.index) %>%
    dplyr::select(import, quantile, species, iso3)
  
  return(import_data_classified)
}

## export functions
export_quantile_function <- function(data){
  
  export_data_quantiled <- data %>%
    classify(data,
             method = "quantile",
             breaks = 5)
  
  return(export_data_quantiled)
}

export_classify_function <- function(data,data2){
  
  export_data_classified <- data %>%
    cbind(data2$index) %>%
    dplyr::rename(quantile = data2.index) %>%
    dplyr::select(export, quantile, species, iso3)
  
  return(export_data_classified)
}

# Larval import connectivity by species
aline_import_quantile <- import_quantile_function(aline_connectivity_import$import)
aline_import_classify <- import_classify_function(aline_connectivity_import,aline_import_quantile)

ccaer_import_quantile <- import_quantile_function(ccaer_connectivity_import$import)
ccaer_import_classify <- import_classify_function(ccaer_connectivity_import,ccaer_import_quantile)

cblee_import_quantile <- import_quantile_function(cblee_connectivity_import$import)
cblee_import_classify <- import_classify_function(cblee_connectivity_import,cblee_import_quantile)

lehre_import_quantile <- import_quantile_function(lehre_connectivity_import$import)
lehre_import_classify <- import_classify_function(lehre_connectivity_import,lehre_import_quantile)

nmino_import_quantile <- import_quantile_function(nmino_connectivity_import$import)
nmino_import_classify <- import_classify_function(nmino_connectivity_import,nmino_import_quantile)

pmult_import_quantile <- import_quantile_function(pmult_connectivity_import$import)
pmult_import_classify <- import_classify_function(pmult_connectivity_import,pmult_import_quantile)

pleop_import_quantile <- import_quantile_function(pleop_connectivity_import$import)
pleop_import_classify <- import_classify_function(pleop_connectivity_import,pleop_import_quantile)

ppisa_import_quantile <- import_quantile_function(ppisa_connectivity_import$import)
ppisa_import_classify <- import_classify_function(ppisa_connectivity_import,ppisa_import_quantile)

sglob_import_quantile <- import_quantile_function(sglob_connectivity_import$import)
sglob_import_classify <- import_classify_function(sglob_connectivity_import,sglob_import_quantile)

scana_import_quantile <- import_quantile_function(scana_connectivity_import$import)
scana_import_classify <- import_classify_function(scana_connectivity_import,scana_import_quantile)

larval_connectivity_import <- rbind(aline_import_classify,
                                    ccaer_import_classify,
                                    cblee_import_classify,
                                    lehre_import_classify,
                                    nmino_import_classify,
                                    pmult_import_classify,
                                    pleop_import_classify,
                                    ppisa_import_classify,
                                    sglob_import_classify,
                                    scana_import_classify)

# Larval export connectivity by species
aline_export_quantile <- export_quantile_function(aline_connectivity_export$export)
aline_export_classify <- export_classify_function(aline_connectivity_export,aline_export_quantile)

ccaer_export_quantile <- export_quantile_function(ccaer_connectivity_export$export)
ccaer_export_classify <- export_classify_function(ccaer_connectivity_export,ccaer_export_quantile)

cblee_export_quantile <- export_quantile_function(cblee_connectivity_export$export)
cblee_export_classify <- export_classify_function(cblee_connectivity_export,cblee_export_quantile)

lehre_export_quantile <- export_quantile_function(lehre_connectivity_export$export)
lehre_export_classify <- export_classify_function(lehre_connectivity_export,lehre_export_quantile)

nmino_export_quantile <- export_quantile_function(nmino_connectivity_export$export)
nmino_export_classify <- export_classify_function(nmino_connectivity_export,nmino_export_quantile)

pmult_export_quantile <- export_quantile_function(pmult_connectivity_export$export)
pmult_export_classify <- export_classify_function(pmult_connectivity_export,pmult_export_quantile)

pleop_export_quantile <- export_quantile_function(pleop_connectivity_export$export)
pleop_export_classify <- export_classify_function(pleop_connectivity_export,pleop_export_quantile)

ppisa_export_quantile <- export_quantile_function(ppisa_connectivity_export$export)
ppisa_export_classify <- export_classify_function(ppisa_connectivity_export,ppisa_export_quantile)

sglob_export_quantile <- export_quantile_function(sglob_connectivity_export$export)
sglob_export_classify <- export_classify_function(sglob_connectivity_export,sglob_export_quantile)

scana_export_quantile <- export_quantile_function(scana_connectivity_export$export)
scana_export_classify <- export_classify_function(scana_connectivity_export,scana_export_quantile)

larval_connectivity_export <- rbind(aline_export_classify,
                                    ccaer_export_classify,
                                    cblee_export_classify,
                                    lehre_export_classify,
                                    nmino_export_classify,
                                    pmult_export_classify,
                                    pleop_export_classify,
                                    ppisa_export_classify,
                                    sglob_export_classify,
                                    scana_export_classify)

######################################################
######################################################

### 3. Larval migration
## Loading larval migration data
aline_migration_import <- st_read(dsn = analyze_dir, "aline_migration_import")
aline_migration_export <- st_read(dsn = analyze_dir, "aline_migration_export")

ccaer_migration_import <- st_read(dsn = analyze_dir, "ccaer_migration_import")
ccaer_migration_export <- st_read(dsn = analyze_dir, "ccaer_migration_export")

cblee_migration_import <- st_read(dsn = analyze_dir, "cblee_migration_import")
cblee_migration_export <- st_read(dsn = analyze_dir, "cblee_migration_export")

lehre_migration_import <- st_read(dsn = analyze_dir, "lehre_migration_import")
lehre_migration_export <- st_read(dsn = analyze_dir, "lehre_migration_export")

nmino_migration_import <- st_read(dsn = analyze_dir, "nmino_migration_import")
nmino_migration_export <- st_read(dsn = analyze_dir, "nmino_migration_export")

pmult_migration_import <- st_read(dsn = analyze_dir, "pmult_migration_import")
pmult_migration_export <- st_read(dsn = analyze_dir, "pmult_migration_export")

pleop_migration_import <- st_read(dsn = analyze_dir, "pleop_migration_import")
pleop_migration_export <- st_read(dsn = analyze_dir, "pleop_migration_export")

ppisa_migration_import <- st_read(dsn = analyze_dir, "ppisa_migration_import")
ppisa_migration_export <- st_read(dsn = analyze_dir, "ppisa_migration_export")

sglob_migration_import <- st_read(dsn = analyze_dir, "sglob_migration_import")
sglob_migration_export <- st_read(dsn = analyze_dir, "sglob_migration_export")

scana_migration_import <- st_read(dsn = analyze_dir, "scana_migration_import")
scana_migration_export <- st_read(dsn = analyze_dir, "scana_migration_export")


## Functions that generate and bind the quantile classifications of import / export values
# import functions
import_quantile_migration <- function(data){
  
  import_migration_quantiled <- data %>%
    classify(data,
             method = "quantile",
             breaks = 5)
  
  return(import_migration_quantiled)
}

import_classify_migration <- function(data,data2){
  
  import_migration_classified <- data %>%
    cbind(data2$index) %>%
    dplyr::rename(quantile = data2.index) %>%
    dplyr::select(migration, import, quantile, species, iso3)
  
  return(import_migration_classified)
}

## export functions
export_quantile_migration <- function(data){
  
  export_migration_quantiled <- data %>%
    classify(data,
             method = "quantile",
             breaks = 5)
  
  return(export_migration_quantiled)
}

export_classify_migration <- function(data,data2){
  
  export_migration_classified <- data %>%
    cbind(data2$index) %>%
    dplyr::rename(quantile = data2.index) %>%
    dplyr::select(migration, export, quantile, species, iso3)
  
  return(export_migration_classified)
}

# Larval import migration by species
aline_migration_import_quantile <- import_quantile_migration(aline_migration_import$migration)
aline_migration_import_classify <- import_classify_migration(aline_migration_import,aline_migration_import_quantile)

ccaer_migration_import_quantile <- import_quantile_migration(ccaer_migration_import$migration)
ccaer_migration_import_classify <- import_classify_migration(ccaer_migration_import,ccaer_migration_import_quantile)

cblee_migration_import_quantile <- import_quantile_migration(cblee_migration_import$migration)
cblee_migration_import_classify <- import_classify_migration(cblee_migration_import,cblee_migration_import_quantile)

lehre_migration_import_quantile <- import_quantile_migration(lehre_migration_import$migration)
lehre_migration_import_classify <- import_classify_migration(lehre_migration_import,lehre_migration_import_quantile)

nmino_migration_import_quantile <- import_quantile_migration(nmino_migration_import$migration)
nmino_migration_import_classify <- import_classify_migration(nmino_migration_import,nmino_migration_import_quantile)

pmult_migration_import_quantile <- import_quantile_migration(pmult_migration_import$migration)
pmult_migration_import_classify <- import_classify_migration(pmult_migration_import,pmult_migration_import_quantile)

pleop_migration_import_quantile <- import_quantile_migration(pleop_migration_import$migration)
pleop_migration_import_classify <- import_classify_migration(pleop_migration_import,pleop_migration_import_quantile)

ppisa_migration_import_quantile <- import_quantile_migration(ppisa_migration_import$migration)
ppisa_migration_import_classify <- import_classify_migration(ppisa_migration_import,ppisa_migration_import_quantile)

sglob_migration_import_quantile <- import_quantile_migration(sglob_migration_import$migration)
sglob_migration_import_classify <- import_classify_migration(sglob_migration_import,sglob_migration_import_quantile)

scana_migration_import_quantile <- import_quantile_migration(scana_migration_import$migration)
scana_migration_import_classify <- import_classify_migration(scana_migration_import,scana_migration_import_quantile)

larval_migration_import <- rbind(aline_migration_import_classify,
                                 ccaer_migration_import_classify,
                                 cblee_migration_import_classify,
                                 lehre_migration_import_classify,
                                 nmino_migration_import_classify,
                                 pmult_migration_import_classify,
                                 pleop_migration_import_classify,
                                 ppisa_migration_import_classify,
                                 sglob_migration_import_classify,
                                 scana_migration_import_classify)

# Larval export migration by species
aline_migration_export_quantile <- export_quantile_migration(aline_migration_export$migration)
aline_migration_export_classify <- export_classify_migration(aline_migration_export,aline_migration_export_quantile)

ccaer_migration_export_quantile <- export_quantile_migration(ccaer_migration_export$migration)
ccaer_migration_export_classify <- export_classify_migration(ccaer_migration_export,ccaer_migration_export_quantile)

cblee_migration_export_quantile <- export_quantile_migration(cblee_migration_export$migration)
cblee_migration_export_classify <- export_classify_migration(cblee_migration_export,cblee_migration_export_quantile)

lehre_migration_export_quantile <- export_quantile_migration(lehre_migration_export$migration)
lehre_migration_export_classify <- export_classify_migration(lehre_migration_export,lehre_migration_export_quantile)

nmino_migration_export_quantile <- export_quantile_migration(nmino_migration_export$migration)
nmino_migration_export_classify <- export_classify_migration(nmino_migration_export,nmino_migration_export_quantile)

pmult_migration_export_quantile <- export_quantile_migration(pmult_migration_export$migration)
pmult_migration_export_classify <- export_classify_migration(pmult_migration_export,pmult_migration_export_quantile)

pleop_migration_export_quantile <- export_quantile_migration(pleop_migration_export$migration)
pleop_migration_export_classify <- export_classify_migration(pleop_migration_export,pleop_migration_export_quantile)

ppisa_migration_export_quantile <- export_quantile_migration(ppisa_migration_export$migration)
ppisa_migration_export_classify <- export_classify_migration(ppisa_migration_export,ppisa_migration_export_quantile)

sglob_migration_export_quantile <- export_quantile_migration(sglob_migration_export$migration)
sglob_migration_export_classify <- export_classify_migration(sglob_migration_export,sglob_migration_export_quantile)

scana_migration_export_quantile <- export_quantile_migration(scana_migration_export$migration)
scana_migration_export_classify <- export_classify_migration(scana_migration_export,scana_migration_export_quantile)

larval_migration_export <- rbind(aline_migration_export_classify,
                                 ccaer_migration_export_classify,
                                 cblee_migration_export_classify,
                                 lehre_migration_export_classify,
                                 nmino_migration_export_classify,
                                 pmult_migration_export_classify,
                                 pleop_migration_export_classify,
                                 ppisa_migration_export_classify,
                                 sglob_migration_export_classify,
                                 scana_migration_export_classify)

######################################################
######################################################

### 4. Habitat area extents

## load data
mangrove <- st_read(dsn = analyze_dir, "mangrove")
coral <- st_read(dsn = analyze_dir, "reef")
seagrass <- st_read(dsn = analyze_dir, "seagrass")

coral_participatory <- st_read(dsn = analyze_dir, "coral_participatory")
mangrove_participatory <- st_read(dsn = analyze_dir, "mangrove_participatory")


## combining habitat area
coral_reef_combine <- rbind(coral,
                            coral_participatory)
coral_reef_simplified <- coral_reef_combine %>%
  dplyr::mutate(area = st_area(coral_reef_combine)) %>%
  summarise(area = sum(area)) %>%
  dplyr::mutate(iso3 = "PHL",
                habitat = "Coral reef") %>%
  dplyr::select(iso3, habitat)


mangrove_combine <- rbind(mangrove,
                          mangrove_participatory) 
mangrove_simplified <- mangrove_combine %>%
  dplyr::mutate(area = st_area(mangrove_combine)) %>%
  summarise(area = sum(area)) %>%
  dplyr::mutate(iso3 = "PHL",
                habitat = "Mangrove") %>%
  dplyr::select(iso3, habitat)
View(mangrove_simplified)

seagrass_simplified <- seagrass %>%
  dplyr::mutate(area = st_area(seagrass)) %>%
  summarise(area = sum(area)) %>%
  dplyr::mutate(iso3 = "PHL",
                habitat = "Seagrass") %>%
  dplyr::select(iso3, habitat)
View(seagrass_simplified)

coral_minus_mangroves <- st_difference(st_make_valid(coral_reef_simplified),
                                       st_make_valid(mangrove_simplified))



# reef <- st_difference(st_make_valid(coral_minus_mangroves), # process takes a long time
#                       st_make_valid(seagrass_simplified))
# 
# reef <- reef %>%
#   dplyr::select(iso3,habitat)
# 
# mangrove_tool <- st_make_valid(mangrove_simplified)
# coral_reef <- st_make_valid(reef) # process takes a long time
# seagrass_tool <- st_make_valid(seagrass_simplified)


######################################################
######################################################
### 5. Planning grid managed access area percentages

## load the data
# Marxan frequency data across all interested managed access areas
phl_maa_marxan <- read.csv(paste(analyze_dir, "phl_marxan.csv", sep= "/"), as.is = T)
View(phl_maa_marxan)

# original planning grid data
planning_units <- st_read(dsn = analyze_dir, "planning_grid") %>%
  dplyr::select(-mrxn_fr) # remove old marxan frequencies to be able to add the new data later
View(planning_units)

# key for planning unit ID and managed access area names
maa_names <- read.csv(paste(analyze_dir, "planning_grid_ma_region.csv", sep= "/"), as.is = T)
View(maa_names)

levels(as.factor(maa_names$maa))

# See how each managed access area matches with the region of interest
grouping <- maa_names %>%
  group_by(maa) %>%
  summarise(region = first(region))

## combine the data based on puid, region, and maa names
# first need to combine the original planning grid data (which has puid and region data but not maa nor areas) with table that has puid, region, and maa names
pu_maa_names <- merge(planning_units, maa_names, by.x = c("region", "puid"), by.y = c("region", "puid"))
View(pu_maa_names) # sf now has maa name

pu_marxan <- merge(pu_maa_names, phl_maa_marxan, by.x = c("region", "puid"), by.y = c("region", "puid")) %>%
  dplyr::rename(maa = maa.x) %>%
  dplyr::select(-X, -maa.y)

# summed habitat area per managed access area data
maa_habitat <- pu_marxan %>%
  group_by(maa,region) %>%
  summarise(coral_maa = sum(ref_r_h),
            mangrove_maa = sum(mngrv__),
            seagrass_maa = sum(sgrss__),
            total_maa = sum(ttl_r_h))
View(maa_habitat)

st_geometry(maa_habitat) <- NULL # need to make data frame for allowing to be joined in a later step
class(maa_habitat)

# now can join original managed access data with has area values with the planning grid data using the maa names
pu_ma_area <- left_join(pu_marxan,maa_habitat) %>%
  dplyr::rename(coral_puid_ha = ref_r_h,
                mangrove_puid_ha = mngrv__,
                seagrass_puid_ha = sgrss__,
                total_puid_ha = ttl_r_h,
                mean_coral_cover = mn_crl_,
                mean_seagrass_cover = mn_sgr_) %>%
  dplyr::mutate(coral_maa_pct = (coral_puid_ha / coral_maa) * 100,
                mangrove_maa_pct = (mangrove_puid_ha / mangrove_maa) * 100,
                seagrass_maa_pct = (seagrass_puid_ha / seagrass_maa) * 100,
                total_maa_pct = (total_puid_ha / total_maa) * 100) %>%
  dplyr::select(iso3, puid, region, maa, coral_maa, mangrove_maa, seagrass_maa, total_maa,
                coral_puid_ha, mangrove_puid_ha, seagrass_puid_ha, total_puid_ha,
                coral_maa_pct, mangrove_maa_pct, seagrass_maa_pct, total_maa_pct,
                mean_coral_cover, mean_seagrass_cover, marxan_frequency)
View(pu_ma_area)



######################################################
######################################################


### 6. Export data for tool integration
st_write(obj = larval_connectivity_import, dsn = paste0(tool_dir, "/", "connectivity_nodes_import.shp"), append = F)
st_write(obj = larval_connectivity_export, dsn = paste0(tool_dir, "/", "connectivity_nodes_export.shp"), append = F)

st_write(obj = larval_migration_import, dsn = paste0(tool_dir, "/", "larval_migration_import.shp"), append = F)
st_write(obj = larval_migration_export, dsn = paste0(tool_dir, "/", "larval_migration_export.shp"), append = F)

# st_write(obj = coral_reef, dsn = paste0(tool_dir, "/", "reef.shp"), append = F)
st_write(obj = mangrove_tool, dsn = paste0(tool_dir, "/", "mangrove.shp"), append = F)
st_write(obj = seagrass_tool, dsn = paste0(tool_dir, "/", "seagrass.shp"), append = F)

# st_write(obj = reef, dsn = paste0(tool_dir, "/", "reef1.shp"), append = F)
# st_write(obj = coral_reef_simplified, dsn = paste0(tool_dir, "/", "reef_simplified.shp"), append = F)
st_write(obj = coral_minus_mangroves, dsn = paste0(tool_dir, "/", "coral_minus_mangrove.shp"), append = F)

st_write(obj = pu_ma_area, dsn = paste0(tool_dir, "/", "planning_grid.shp"), append = F)
