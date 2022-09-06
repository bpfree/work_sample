###################################################
###################################################
###################################################


######### Part 10 #########
## Wind farms
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
name_dir <- "data\\e_wind_data"
wind_dir <- "data\\c_spatial_data\\owf_areas"

wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

###################################################
###################################################
###################################################

### Load the wind farm areas (note: these were from the March 29 2021 update by BOEM)
### https://www.boem.gov/renewable-energy/mapping-and-data/renewable-energy-gis-data
## Lease areas
lease_areas <- st_read(dsn = wind_dir, layer = "atlantic_lease_areas_08032021") %>%
  # calculate the area, which will be in square meters, for each unique stratum using the Strata_Hist object
  dplyr::mutate(area = as.numeric(st_area(.)),
                # convert area to square nautical miles [1 sq nm = 3,429,904 sq meters]
                sq_nm = area / 3429904)


## Planning areas
planning_areas <- st_read(dsn = wind_dir, layer = "atlantic_planning_areas_07272021") %>%
  # calculate the area, which will be in square meters, for each unique stratum using the Strata_Hist object
  dplyr::mutate(area = as.numeric(st_area(.)),
                # convert area to square nautical miles [1 sq nm = 3,429,904 sq meters]
                sq_nm = area / 3429904)

levels(as.factor(lease_areas$Lease_Numb)) # to see the names of the lease areas
levels(as.factor(planning_areas$INFO)) # to see the unique planning area names

###################################################

# conventional name tables
lease_name <- read.csv(paste(name_dir, "owf_lease_name_table.csv", sep = "/"))
planning_name <- read.csv(paste(name_dir, "owf_planning_name_table.csv", sep = "/"))

###################################################
###################################################
###################################################

lease_areas <- merge(lease_areas, lease_name) %>%
  dplyr::rename(state = State) %>%
  dplyr::group_by(name,
                  state) %>%
  dplyr::select(name,
                state,
                sq_nm) %>%
  dplyr::summarise(sq_nm = sum(sq_nm))

planning_areas <- merge(planning_areas, planning_name) %>%
  dplyr::mutate(state = "New York") %>%
  dplyr::group_by(name,
                  state) %>%
  dplyr::select(name,
                state,
                sq_nm) %>%
  dplyr::summarise(sq_nm = sum(sq_nm))

owf_areas <- rbind(lease_areas,
                   planning_areas)

owf_areas_multi <- st_make_valid(st_cast(owf_areas,
                           to = "MULTIPOLYGON"))

###################################################
###################################################
###################################################

### Export offshore wind farm data
st_write(obj = lease_areas, dsn = wind_geopackage, layer = "lease_wind_farms", append = F)
st_write(obj = planning_areas, dsn = wind_geopackage, layer = "planning_wind_farms", append = F)
st_write(obj = owf_areas, dsn = wind_geopackage, layer = "owf_areas", append = T)

st_write(obj = owf_areas, paste0(wind_dir, setp = "/", "owf_areas.shp"), append = T)
st_write(obj = owf_areas_multi, paste0(wind_dir, setp = "/", "owf_areas_multi.shp"), append = T)
