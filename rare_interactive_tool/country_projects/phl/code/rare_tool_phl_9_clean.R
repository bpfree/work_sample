#######################
# Rare Interactive Tool
#######################

#############
# Philippines
#############

#####################
# Cleaning data
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
clean_dir <- "country_projects\\phl\\data\\b_clean_data"

## Output directories
analyze_dir <- "country_projects\\phl\\data\\c_analyze_data"
tool_dir <- "country_projects\\phl\\data\\d_tool_data"

######################################################
######################################################

### Coral habitat quality
## Load data
antique_hq <- st_read(dsn = clean_dir, "antique_habitat")
camotes_hq <- st_read(dsn = clean_dir, "camotes_habitat")
escalante_hq <- st_read(dsn = clean_dir, "escalante_city_habitat")
siargao_hq <- st_read(dsn = clean_dir, "siargao_habitat")


## Clean and prepare data
habitat_quality_coral <- rbind(antique_hq,
                               camotes_hq,
                               escalante_hq,
                               siargao_hq) %>%
  dplyr::mutate(iso3 = "PHL") %>%
  dplyr::relocate(iso3, .after = ttl_crl)


## Export data for analysis or later integration in tool
st_write(obj = habitat_quality_coral, dsn = paste0(tool_dir, "/", "habitat_quality_coral.shp"), append = F)

### Seagrass habitat quality
## Load seagrass data
habitat_quality_seagrass <- st_read(dsn = clean_dir, "habitat_quality_seagrass")
View(habitat_quality_seagrass)

## Export data for tool
st_write(obj = habitat_quality_seagrass, dsn = paste0(tool_dir, "/", "habitat_quality_seagrass.shp"), append = F)





### Planning grid and habitat area
## Load planning unit data
phl_pu <- st_read(dsn = clean_dir, "phl_pu")

## Load habitat area data
antique_habitat_area <- read.csv(paste(clean_dir, "antique_habitat_area.csv", sep= "/"), as.is = T)
cebu_habitat_area <- read.csv(paste(clean_dir, "cebu_habitat_area.csv", sep= "/"), as.is = T)
negros_occidental_habitat_area <- read.csv(paste(clean_dir, "negros_occidental_habitat_area.csv", sep= "/"), as.is = T)
surigao_del_norte_habitat_area <- read.csv(paste(clean_dir, "surigao_del_norte_habitat_area.csv", sep= "/"), as.is = T)

colnames(antique_habitat_area)
colnames(cebu_habitat_area)
colnames(negros_occidental_habitat_area)
colnames(surigao_del_norte_habitat_area)

phl_habitat <- rbind(antique_habitat_area,
                     cebu_habitat_area,
                     negros_occidental_habitat_area,
                     surigao_del_norte_habitat_area)

# Load mean habitat and Marxan data
pu_mean <- read.csv(paste(clean_dir, "pu_mean.csv", sep = "/"), as.is = T)
pu_mean <- pu_mean %>%
  dplyr::mutate(region = recode(region, "antique" = "Antique",
                                        "camotes" = "Cebu",
                                        "escalante" = "Negros Occidental",
                                        "siargao" = "Surigao del Norte"))

View(pu_mean)
x <- as.factor(pu_mean$region)
levels(x)

## Clean and prepare data
# Function to link habitat with planning unit
pu_habitat_function <- function(data,data2){
  pu_habitat <- data %>%
    cbind(data2) %>%
    dplyr::select(-X, - region.1, -puid.1)
  
  return(pu_habitat)
}

phl_habitat_clean <- pu_habitat_function(phl_pu,
                                         phl_habitat)

planning_grid <- merge(phl_habitat_clean, pu_mean, by.x=c("region", "puid"), by.y=c("region", "puid")) %>%
  dplyr::select(-X,-ISO3) %>%
  dplyr::relocate(iso3,
                  puid,
                  region,
                  reef_area_ha,
                  seagrass_area_ha,
                  mangrove_area_ha,
                  total_area_ha,
                  mean_coral_cover,
                  mean_seagrass_cover,
                  marxan_frequency)

View(planning_grid)


## Export data for analysis or later integration
st_write(planning_grid, dsn = paste0(analyze_dir, "/", "planning_grid.shp"), append = F)
