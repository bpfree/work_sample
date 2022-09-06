###################################################
###################################################
###################################################


######### Part 5 #########
## Creating geographic data
##########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
bts_dir <- "data\\b_bts_data"
spatial_dir <- "data\\c_spatial_data"
geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
list.files(bts_dir)

# data parameters
crs <- 4269

###################################################
###################################################
###################################################

#### Load catch (abundance), weight (biomass), and station data for bottom trawl survey
bts_all <- readRDS(paste(bts_dir, "bts_all.rds", sep = "/"))
bts_representative <- readRDS(paste(bts_dir, "bts_representative.rds", sep = "/"))

###################################################
###################################################
###################################################

### Remove any data that do not have starting longitude / latitudes
# produce all rows where the starting latitude does not have any NA values
bts_all_non_na <- bts_all[!is.na(bts_all$dd_start_lat),]
bts_representative_non_na <- bts_representative[!is.na(bts_representative$dd_start_lat),]

### Convert data to be a simple feature
## This will allow R and other spatial data to know
## where in the world the data are located on a
## a coordinate plane
bts_all_sf <- st_as_sf(bts_all_non_na,
                       # using the longitude and latitude starting points as the x- and y-values)
                       coords = c(x = "dd_start_long",
                                  y = "dd_start_lat"),
                       # place the data into a coordinate reference system of 4269 (NAD83)
                       crs = crs)

bts_representative_sf <- st_as_sf(bts_representative_non_na,
                                  # using the longitude and latitude starting points as the x- and y-values)
                                  coords = c(x = "dd_start_long",
                                             y = "dd_start_lat"),
                                  # place the data into a coordinate reference system of 4269 (NAD83)
                                  crs = crs)

###################################################
###################################################
###################################################

### Export the data
## as a RDS file (to open in R)
saveRDS(bts_all_sf, file = paste0(spatial_dir, "/", "bts_all_sf.rds"))
saveRDS(bts_representative_sf, file = paste0(spatial_dir, "/", "bts_representative_sf.rds"))

saveRDS(bts_all_non_na, file = paste0(spatial_dir, "/", "bts_all_non_na.rds"))
saveRDS(bts_representative_non_na, file = paste0(spatial_dir, "/", "bts_representative_non_na.rds"))

## within a geopackage (to open up in other spatial software)
st_write(obj = bts_all_sf, dsn = geopackage, layer = "bts_all_sf", append = F)
st_write(obj = bts_representative_sf, dsn = geopackage, layer = "bts_representative_sf", append = F)
