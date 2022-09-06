############################
# Great Sea Reef Data Set-up
############################

# Clean environment
rm(list = ls())

# Setup
###########################

# Preparing packages
if (!require("pacman")) install.packages("pacman")

# get the most updated version of ggrepel
# devtools::install_github("slowkow/ggrepel") # add library(ggrepel) if version is not 0.9.0 [can check using sessionInfo()]

# Load packages
pacman::p_load(dplyr,fasterize,ggplot2,ggrepel,ggsn,ggspatial,metR,raster,RColorBrewer,rgdal,rgeos,scales,sf,sp,tidyr) # may have to use library(metR) to load that package

# Setting data directory
gis_dir <- setwd("C:\\Users\\free\\Dropbox (MPAMystery)\\RICOTTA_GIS\\oceans_program\\dom\\fiji_report\\gis_r")
gis_dir # check that correct directory has been set

# Verify that all associated files are in directory
list.files(gis_dir)

# Setting output map directories
gsr_map_dir <- "C:\\Users\\free\\Dropbox (MPAMystery)\\RICOTTA_GIS\\oceans_program\\dom\\fiji_report\\maps\\gsr"
province_map_dir <-  "C:\\Users\\free\\Dropbox (MPAMystery)\\RICOTTA_GIS\\oceans_program\\dom\\fiji_report\\maps\\province"
qoliqoli_map_dir <- "C:\\Users\\free\\Dropbox (MPAMystery)\\RICOTTA_GIS\\oceans_program\\dom\\fiji_report\\maps\\qoliqoli"

# Read data
###########################
# Loading the required data
# Administrative boundary data
fiji <- st_read(dsn = gis_dir, layer = "fiji") # Fiji land administrative boundary
fji_eez <- st_read(dsn = gis_dir, layer = "fiji_eez") # Fijian exclusive economic zone
qoliqoli <- st_read(dsn = gis_dir, layer = "qoliqoli") # Qoliqoli (district) boundaries where survey sites occurred
provinces <- st_read(dsn = gis_dir, layer = "province") # Fiji provinces where survey sites occurred
gsr <- st_read(dsn = gis_dir, layer = "gsr") # Great Sea Reef boundary

# Ecological data
fji_coral <- st_read(dsn = gis_dir, layer = "fiji_coral") # Coral data extent in Great Sea Reef
fji_seagrass <- st_read(dsn = gis_dir, layer = "fiji_seagrass") %>% # Seagrass extent in Great Sea Reef
  st_buffer(100) %>% # add a 100-meter buffer around the data
  st_cast("MULTIPOLYGON") # make as multipolygon again
fji_mangrove96 <- st_read(dsn = gis_dir, layer = "fji_mang96") # Mangrove data in Great Sea Reef for 1996
fji_mangrove16 <- st_read(dsn = gis_dir, layer = "fji_mang16") # Mangrove data in Great Sea Reef for 2016
fji_mang_gain <- st_read(dsn = gis_dir, layer = "fji_mang_gain") %>% # Mangrove extent gained in Great Sea Reef between 1996 and 2016
  st_buffer(100) %>% # add a 50-meter buffer around the data
  st_cast("MULTIPOLYGON") # make as multipolygon again
fji_mang_loss <- st_read(dsn = gis_dir, layer = "fji_mang_loss") %>% # Mangrove extent lost in Great Sea Reef between 1996 and 2016
  st_buffer(100) %>% # add a 50-meter buffer around the data
  st_cast("MULTIPOLYGON") # make as multipolygon again

# Geomorphic data
fji_geo <- st_read(dsn = gis_dir, layer = "fiji_geo") # loads the geomorphic data
levels(fji_geo$Geo_Zone) # get list of unique geomorphic zones

object.size(fji_geo) # 0.16 GB

# Separate geomorphic zones
fji_irf <- filter(fji_geo,Geo_Zone == "Inner Reef Flat") # inner reef flat
fji_orf <- filter(fji_geo,Geo_Zone == "Outer Reef Flat") # outer reef flat
fji_plat <- filter(fji_geo,Geo_Zone == "Plateau") # plateau
fji_rc <- filter(fji_geo,Geo_Zone == "Reef Crest") # reef crest
fji_rs <- filter(fji_geo, Geo_Zone == "Reef Slope") # reef slope
fji_sl <- filter(fji_geo,Geo_Zone == "Shallow Lagoon") # shallow lagoon
fji_srs <- filter(fji_geo,Geo_Zone == "Sheltered Reef Slope") # sheltered reef slope
fji_trf <- filter(fji_geo,Geo_Zone == "Terrestrial Reef Flat") # terrestrial reef flat
fji_unk <- filter(fji_geo, Geo_Zone == "Unknown") # unknown

# Rasterize geomorphic data
# All data
geo_temp <- raster(extent(fji_geo), res = 25, crs = fji_geo) # create a template raster with the Fiji geomorphic extent
geo_rast <- fasterize(fji_geo, geo_temp) # rasterize the geomorphic data
geo_map <- raster::as.data.frame(geo_rast, xy=T) %>%
  dplyr::filter(!is.na(layer))

object.size(geo_map) # check how large data are --> 0.18 GB

# Inner Reef Flat
irf_temp <- raster(extent(fji_irf), res = 25, crs = fji_irf) # create a template raster with the inner reef flat extent
irf_rast <- fasterize(fji_irf, irf_temp) # rasterize the inner reef flat data
irf_map <- raster::as.data.frame(irf_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "irf")) %>%
  mutate(irf = "Inner Reef Flat")
plot(irf_rast)

# Outer Reef Flat
orf_temp <- raster(extent(fji_orf), res = 25, crs = fji_orf) # create a template raster with the inner reef flat extent
orf_rast <- fasterize(fji_orf, orf_temp) # rasterize the inner reef flat data
orf_map <- raster::as.data.frame(orf_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "orf")) %>%
  mutate(orf = "Outer Reef Flat")
plot(orf_rast)

# Plateau
plat_temp <- raster(extent(fji_plat), res = 25, crs = fji_plat) # create a template raster with the inner reef flat extent
plat_rast <- fasterize(fji_plat, plat_temp) # rasterize the inner reef flat data
plat_map <- raster::as.data.frame(plat_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "plat")) %>%
  mutate(plat = "Plateau")
plot(plat_rast)

# Reef Crest
rc_temp <- raster(extent(fji_rc), res = 25, crs = fji_rc) # create a template raster with the inner reef flat extent
rc_rast <- fasterize(fji_rc, rc_temp) # rasterize the inner reef flat data
rc_map <- raster::as.data.frame(rc_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "rc")) %>%
  mutate(rc = "Reef Crest")
plot(rc_rast)

# Reef Slope
rs_temp <- raster(extent(fji_rs), res = 25, crs = fji_rs) # create a template raster with the inner reef flat extent
rs_rast <- fasterize(fji_rs, rs_temp) # rasterize the inner reef flat data
rs_map <- raster::as.data.frame(rs_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "rs")) %>%
  mutate(rs = "Reef Slope")
plot(rs_rast)

# Shallow Lagoon
sl_temp <- raster(extent(fji_sl), res = 25, crs = fji_sl) # create a template raster with the inner reef flat extent
sl_rast <- fasterize(fji_sl, sl_temp) # rasterize the inner reef flat data
sl_map <- raster::as.data.frame(sl_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "sl")) %>%
  mutate(sl = "Shallow Lagoon")
plot(sl_rast)

# Sheltered Reef Slope
srs_temp <- raster(extent(fji_srs), res = 25, crs = fji_srs) # create a template raster with the inner reef flat extent
srs_rast <- fasterize(fji_srs, srs_temp) # rasterize the inner reef flat data
srs_map <- raster::as.data.frame(srs_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "srs")) %>%
  mutate(srs = "Sheltered Reef Slope")
plot(srs_rast)

# Terrestrial Reef Flat
trf_temp <- raster(extent(fji_trf), res = 25, crs = fji_trf) # create a template raster with the inner reef flat extent
trf_rast <- fasterize(fji_trf, trf_temp) # rasterize the inner reef flat data
trf_map <- raster::as.data.frame(trf_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "trf")) %>%
  mutate(trf = "Terrestrial Reef Flat")
plot(trf_rast)

# Unknown
unk_temp <- raster(extent(fji_unk), res = 25, crs = fji_unk) # create a template raster with the inner reef flat extent
unk_rast <- fasterize(fji_unk, unk_temp) # rasterize the inner reef flat data
unk_map <- raster::as.data.frame(unk_rast, xy=T) %>%
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "unk")) %>%
  mutate(unk = "Unknown")
plot(unk_rast)



# Rasterize ecological data
# Coral data
coral_temp <- raster(extent(fji_coral),res = 25, crs = fji_coral) # create a template raster with the coral reef extent
coral_rast <- fasterize(fji_coral,coral_temp) # rasterizing the coral data
coral_map <- raster::as.data.frame(coral_rast, xy=T) %>% # Convert to dataframe to have mapped later
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "coral")) %>%
  mutate(coral = "Coral")

plot(coral_rast) # check to make sure that the data appears correctly

object.size (coral_map) # size = 0.03 GB

# Mangrove data
# Mangrove 2016
mangrove16_temp <- raster(extent(fji_mangrove16), res = 25, crs = fji_mangrove16) # create a template raster with the Fiji mangrove 2016 extent
mangrove16_rast <- fasterize(fji_mangrove16, mangrove16_temp) # rasterizing the mangrove 2016 data
mangrove16_map <- raster::as.data.frame(mangrove16_rast,xy=T) %>% # convert to datafram to have mapped later
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "mangrove")) %>%
  mutate(mangrove = "Mangrove")
plot(mangrove16_rast)

# Mangrove 1996
mangrove96_temp <- raster(extent(fji_mangrove96), res = 25, crs = fji_mangrove96) # create a template raster with the Fiji mangrove 1996 extent
mangrove96_rast <- fasterize(fji_mangrove96, mangrove96_temp) # rasterizing the mangrove 1996 data
mangrove96_map <- raster::as.data.frame(mangrove96_rast,xy=T) %>% # convert to datafram to have mapped later
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "mangrove")) %>%
  mutate(mangrove = "Mangrove")
plot(mangrove96_rast)

# Mangrove gain -- 100 meter buffer gain
mangrove_gain_temp <- raster(extent(fji_mang_gain), res = 25, crs = fji_mang_gain) # create a template raster with the Fiji mangrove gain extent
mangrove_gain_rast <- fasterize(fji_mang_gain, mangrove_gain_temp) # rasterizing the mangrove gain data
mangrove_gain_map <- raster::as.data.frame(mangrove_gain_rast,xy=T) %>% # convert to datafram to have mapped later
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "gain")) %>%
  mutate(gain = "Gain")
plot(mangrove_gain_rast)

# Mangrove loss -- 100 meter buffer gain
mangrove_loss_temp <- raster(extent(fji_mang_loss), res = 25, crs = fji_mang_loss) # create a template raster with the Fiji mangrove loss extent
mangrove_loss_rast <- fasterize(fji_mang_loss, mangrove_loss_temp) # rasterizing the mangrove loss data
mangrove_loss_map <- raster::as.data.frame(mangrove_loss_rast,xy=T) %>% # convert to datafram to have mapped later
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "loss")) %>%
  mutate(loss = "Loss") #%>%
#sf::st_crs("+proj=utm +zone=60 +south +datum=WGS84 +units=m +no_defs")
plot(mangrove_loss_rast)

# Seagrass data -- 100 meter buffer
seagrass_temp <- raster(extent(fji_seagrass),res = 25, crs = fji_seagrass) # create a template raster with the seagrass extent
seagrass_rast <- fasterize(fji_seagrass,seagrass_temp) # rasterizing the seagrass data
seagrass_map <- raster::as.data.frame(seagrass_rast, xy=T) %>% # Convert to dataframe to have mapped later
  dplyr::filter(!is.na(layer)) %>%
  setNames(c("longitude", "latitude", "seagrass")) %>%
  mutate(seagrass = "Seagrass")
plot(seagrass_rast) # check to make sure that the data appears correctly


# Geophysical
# Bathymetry
fji_bath <- raster("fji_bath.tif")

object.size(fji_bath)

# To make size smaller, we can focus on an area slightly larger than the extent of the Great Barrier Reef
extent(gsr)

# Create mask on bathymetry data
gsr_aoi <- data.frame(list(rbind(c(470000,7910000), # southwest corner
                                 c(855000,7910000), # southeast corner
                                 c(855000,8255000), # northeast corner
                                 c(470000,8255000)))) %>% #northwest corner
  # Rename columns
  rename(Long = X1,
         Lat = X2) %>%
  # Change to be a simple feature
  st_as_sf(coords = c("Long", "Lat"),crs=32760) %>% # match coordinate reference system as all the other data
  # Change to be a multipoint
  summarise(geomtery = st_combine(geometry)) %>%
  # Change to be a polygon
  st_cast("POLYGON")

class(gsr_aoi) # verify that bath_df is a simple feature / data frame

# Extract the bathymetry data for the smaller extent
bath_aoi <- mask(x = fji_bath, mask = gsr_aoi)
bath_map <- raster::as.data.frame(bath_aoi, xy=T)
bath_map <- setNames(bath_map, c("longitude", "latitude", "depth"))
bath_map <- dplyr::filter(bath_map, depth >= 1)

# alternative way when memory issues are not a thing....
# bath_map <- raster::as.data.frame(bath_aoi, xy=T) %>%
#   setNames(c("longitude", "latitude", "depth")) %>%
#   dplyr::filter(depth >= 1)

plot(bath_aoi)

# Turbidity
fji_sed <- raster("Fiji_turbidity.tif")
sediment_map <- raster::as.data.frame(fji_sed, xy=T) %>%
  dplyr::filter(Fiji_turbidity > 1, Fiji_turbidity < 11) %>%
  setNames(c("longitude", "latitude", "relative"))
plot(fji_sed)

# Historic survey site data
surv_site <- st_read(dsn = gis_dir, layer = "gsr_survey_sites") %>% # all survey sites
  # Rearrange columns
  dplyr::select(Site,District,Historic_f,Qoliqoli_I,Place,Villages,
                Province,Latitude,Longitude,
                Province_1,Sub_group,Historic_b,
                Past_data_,geometry) %>%
  # Remove place, villages, province, sub-group, historic_b, past data
  dplyr::select(-Place,
                -Villages,
                -Qoliqoli_I,
                -Province,
                -Sub_group,
                -Historic_b,
                -Past_data_) %>%
  # Rename columns
  rename(site=Site,
         district=District,
         surveyor=Historic_f,
         latitude=Latitude,
         longitude=Longitude,
         province=Province_1)


levels(surv_site$surveyor) # get order of the surveyors
levels(surv_site$province) # survey sites were not conducted in Ra Province

# Add Ra province to the provinces field to assist with the map for loops
levels(surv_site$province) <- c(levels(surv_site$province), "Ra")
levels(surv_site$province) # see that Ra now appears for the field

# subset out the survey sites by surveyor
wwf_site <- surv_site %>%
  dplyr::filter(surveyor == "WWF") # only historic WWF sites
eia_site <- surv_site %>%
  dplyr::filter(surveyor == "Ba EIA") # only historic EIA sites in Ba province
rfc_site <- surv_site %>%
  dplyr::filter(surveyor == "Reef Check") # only hisoric Reef Check sites
new_site <- surv_site %>%
  dplyr::filter(surveyor == "New Site") # only new site locations


# Quick map of the area
map1 <- ggplot() +
  geom_sf(data = fji_eez, fill = NA, size = 0.05) +
  geom_sf(data = fiji, fill = "red") +
  geom_sf(data = gsr) +
  geom_sf(data = qoliqoli) +
  geom_sf(data = provinces, fill = "green") +
  geom_sf(data = surv_site) +
  scale_x_longitude(breaks = seq(-178,180,2)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Quick map") +
  theme_bw()
map1