#################################
# Fiji Great Sea Reef Report Maps
#################################

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
gis_dir <- setwd("C:\\Users\\Administrator\\Dropbox\\fiji_report\\gis_r")
gis_dir # check that correct directory has been set

# Verify that all associated files are in directory
list.files(gis_dir)

# Setting output map directories
gsr_map_dir <- "C:\\Users\\free\\Dropbox (MPAMystery)\\RICOTTA_GIS\\oceans_program\\dom\\fiji_report\\maps\\gsr"
province_map_dir <-  "C:\\Users\\free\\Dropbox (MPAMystery)\\RICOTTA_GIS\\oceans_program\\dom\\fiji_report\\maps\\province"
qoliqoli_map_dir <- "C:\\Users\\Administrator\\Dropbox\\fiji_report\\maps\\qoliqoli"

# Read data
###########################
# Loading the required data
# Administrative boundary data
fiji <- st_read(dsn = gis_dir, layer = "fiji") # Fiji land administrative boundary
fji_eez <- st_read(dsn = gis_dir, layer = "fiji_eez") # Fijian exclusive economic zone
qoliqoli <- st_read(dsn = gis_dir, layer = "qoliqoli_update_utm") # Qoliqoli (district) boundaries [16, previously 13] where survey sites occurred
qoliqoli_old <- st_read(dsn = gis_dir, layer = "qoliqoli") # old Qoliqoli (district) boundaries [13]
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
bath_map <- raster::as.data.frame(bath_aoi, xy=T) %>%
  setNames(c("longitude", "latitude", "depth")) %>%
  dplyr::filter(depth >= 1)

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
  dplyr::filter(surveyor == "Reef Check") # only historic Reef Check sites
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


















# 
# # Map setup
# ###########################
# ## Defining map elements
# # Colors
# # administrative
# land_col <- "#878787"
# water_col <- "#a6cee3"
# 
# # ecological
# coral_col <- "#e31a1c" # red
# seagrass_col <- "#86bb56" # light green
# mangrove96_col <- "#408536"
# mangrove16_col <- "#33a02c" # green
# mangrove_gain <- "#a481b6" # purple
# mangrove_loss <- "#fdbf6f" # orange
# 
# # geomorphical
# plat_col <- "#befbff" # dark orange
# rs_col <- "#92739d"   # light yellow
# srs_col <- "#ffba15"  # tan
# rc_col <- "#cd6812"   # deep purple
# orf_col <- "#614272"  # dark purple
# irf_col <- "#288471"  # mild purple
# trf_col <- "#77d0fc"  # light purple
# sl_col <- "#e69113"   # tile blue
# unk_col <- "#B2B2B2"  # grey
# 
# # geophysical
# bath_col <- bath_color <- (RColorBrewer::brewer.pal(9,"Blues"))
# sed_col <- (RColorBrewer::brewer.pal(9,"YlOrRd"))
# 
# # create bathymetry breaks
# bath_break <- c(seq(from = 0, to = 200, by = 20),2000) # 0,20,40,60,80,100,120,140,160,180,200,2000
# 
# # survey sites
# site_shape <- c(21,22,23,24)
# site_color <- c("#ffffb3","#fb8072","#bebada","#8dd3c7")
# 
# # unique site colors
# eia_col <- "#ffffb3" # light yellow
# new_col <- "#fb8072" # light pink
# rfc_col <- "#bebada" # light purple
# wwf_col <- "#8dd3c7" # light green
# 
# # survey site shapes
# eia_shape <- 21 # circle
# new_shape <- 22 # square
# rfc_shape <- 23 # diamond
# wwf_shape <- 24 # triangle up
# 
# # Map elements
# # Provincial maps
# scalebar_prov <- annotation_scale(width_hint = 0.2, # percent of the plot occupied (20%)
#                                   pad_x = unit(0.25, "in"), # how much padded from the x=0 position
#                                   pad_y = unit(2.55, "in")) # how much padded from the y=0 position
# narrow_prov <- annotation_north_arrow(height = unit(0.25, "in"), 
#                                       width = unit(0.20, "in"),
#                                       pad_x = unit(0.05, "in"),
#                                       pad_y = unit(2.55, "in"),
#                                       style = north_arrow_orienteering(
#                                         line_width = 1,
#                                         line_col = "black",
#                                         fill = c("white", "black"),
#                                         text_col = "black",
#                                         text_family = "",
#                                         text_face = NULL,
#                                         text_size = 5,
#                                         text_angle = 0))
# 
# # District maps
# scalebar_dist <- annotation_scale(width_hint = 0.2, # percent of the plot occupied (20%)
#                                   pad_x = unit(1.5, "in"), # how much padded from the x=0 position
#                                   pad_y = unit(0.15, "in")) # how much padded from the y=0 position
# narrow_dist <- annotation_north_arrow(height = unit(0.25, "in"), 
#                                       width = unit(0.20, "in"),
#                                       pad_x = unit(1.75, "in"),
#                                       pad_y = unit(0.35, "in"),
#                                       style = north_arrow_orienteering(
#                                         line_width = 1,
#                                         line_col = "black",
#                                         fill = c("white", "black"),
#                                         text_col = "black",
#                                         text_family = "",
#                                         text_face = NULL,
#                                         text_size = 5,
#                                         text_angle = 0))
# 
# # Great Sea Reef maps
# gsr_scale <- annotation_scale(width_hint = 0.2, # percent of the plot occupied (20%)
#                               pad_x = unit(3.90, "in"), # how much padded from the x=0 position
#                               pad_y = unit(0.05, "in")) # how much padded from the y=0 position
# gsr_narrow <- annotation_north_arrow(height = unit(0.25, "in"), 
#                                      width = unit(0.20, "in"),
#                                      pad_x = unit(5.00, "in"),
#                                      pad_y = unit(0.20, "in"),
#                                      style = north_arrow_orienteering(
#                                        line_width = 1,
#                                        line_col = "black",
#                                        fill = c("white", "black"),
#                                        text_col = "black",
#                                        text_family = "",
#                                        text_face = NULL,
#                                        text_size = 5,
#                                        text_angle = 0))
# 
# # Great Sea Reef turbidity maps
# gsr_scale_sed <- annotation_scale(width_hint = 0.2, # percent of the plot occupied (20%)
#                                   pad_x = unit(3.3, "in"), # how much padded from the x=0 position
#                                   pad_y = unit(0.1, "in")) # how much padded from the y=0 position
# gsr_narrow_sed <- annotation_north_arrow(height = unit(0.25, "in"), 
#                                          width = unit(0.20, "in"),
#                                          pad_x = unit(4.28, "in"),
#                                          pad_y = unit(0.25, "in"),
#                                          style = north_arrow_orienteering(
#                                            line_width = 1,
#                                            line_col = "black",
#                                            fill = c("white", "black"),
#                                            text_col = "black",
#                                            text_family = "",
#                                            text_face = NULL,
#                                            text_size = 5,
#                                            text_angle = 0))
# 
# 
# # Theme
# # Province
# map_theme_province <- theme(axis.text=element_text(size=8), # size of the longitude/latitude numbers
#                             axis.title=element_text(size=10),
#                             plot.title=element_text(size=12),
#                             panel.grid.major = element_line(color = "transparent"), 
#                             panel.grid.minor = element_line(color = "transparent"),
#                             panel.background = element_rect(fill = water_col),
#                             axis.text.y = element_text(angle = 90, hjust = 0.5),
#                             legend.position =  c(0.87,0.05), # alternative = bottom
#                             legend.title = element_blank(), # remove the legend title
#                             legend.text = element_text(size=6), # text size of the descriptor
#                             legend.background = element_rect(fill = "transparent"), # remove white background around fill
#                             legend.key.size = unit(0.1, "in"), # size of the color box
#                             legend.key = element_rect(fill = "transparent"), # remove the white background behind legend
#                             legend.margin = margin(-0.15, 0.0, 0, 0, "in"), # reduce spacing between legend elements
#                             axis.line = element_line(colour = "black"))
# 
# prov_theme_title <- theme(axis.text=element_text(size=8),
#                           axis.title=element_text(size=10),
#                           plot.title=element_text(size=12),
#                           panel.grid.major = element_line(color = "transparent"), 
#                           panel.grid.minor = element_line(color = "transparent"),
#                           panel.background = element_rect(fill = water_col),
#                           axis.text.y = element_text(angle = 90, hjust = 0.5),
#                           legend.position =  c(0.80,0.10), # alternative = bottom
#                           legend.title = element_text(size = 8), # text size of legend title
#                           legend.text = element_text(size=6), # text size of the descriptor
#                           legend.background = element_rect(fill = "transparent"), # remove white background around fill
#                           legend.key.size = unit(0.1, "in"), # size of the color box
#                           legend.key = element_rect(fill = "transparent"), # remove the white background behind legend
#                           legend.margin = margin(-0.15, 0.0, 0, 0, "in"), # reduce spacing between legend elements
#                           axis.line = element_line(colour = "black"))
# 
# 
# # District
# map_theme_district <- theme(axis.text=element_text(size=8),
#                             axis.title=element_text(size=10),
#                             plot.title=element_text(size=12),
#                             panel.grid.major = element_line(color = "transparent"), 
#                             panel.grid.minor = element_line(color = "transparent"),
#                             panel.background = element_rect(fill = water_col),
#                             axis.text.y = element_text(angle = 90, hjust = 0.5),
#                             legend.position =  c(0.85,0.05), # alternative = bottom
#                             legend.title = element_blank(), # remove the legend title
#                             legend.text = element_text(size=6), # text size of the descriptor
#                             legend.background = element_rect(fill = "transparent"), # remove white background around fill
#                             legend.key.size = unit(0.1, "in"), # size of the color box
#                             legend.key = element_rect(fill = "transparent"), # remove the white background behind legend
#                             axis.line = element_line(colour = "black"))
# 
# dist_theme_title <- theme(axis.text=element_text(size=8),
#                           axis.title=element_text(size=10),
#                           plot.title=element_text(size=12),
#                           panel.grid.major = element_line(color = "transparent"), 
#                           panel.grid.minor = element_line(color = "transparent"),
#                           panel.background = element_rect(fill = water_col),
#                           axis.text.y = element_text(angle = 90, hjust = 0.5),
#                           legend.position =  c(0.80,0.10), # alternative = bottom
#                           legend.title = element_text(size = 8), # text size of legend title
#                           legend.text = element_text(size=6), # text size of the descriptor
#                           legend.background = element_rect(fill = "transparent"), # remove white background around fill
#                           legend.key.size = unit(0.1, "in"), # size of the color box
#                           legend.key = element_rect(fill = "transparent"), # remove the white background behind legend
#                           axis.line = element_line(colour = "black"))
# 
# # Great Sea Reef
# map_theme_gsr <- theme(axis.text=element_text(size=8),
#                        axis.title=element_text(size=10),
#                        plot.title=element_text(size=12),
#                        panel.grid.major = element_line(color = "transparent"), 
#                        panel.grid.minor = element_line(color = "transparent"),
#                        panel.background = element_rect(fill = water_col),
#                        axis.text.y = element_text(angle = 90, hjust = 0.5),
#                        legend.position =  c(0.20,0.90), # alternative = bottom
#                        legend.title = element_blank(), # remove the legend title
#                        legend.text = element_text(size=6), # text size of the descriptor
#                        legend.background = element_rect(fill = "transparent"), # remove white background around fill
#                        legend.key.size = unit(0.1, "in"), # size of the color box
#                        legend.key = element_rect(fill = "transparent"), # remove the white background behind legend
#                        axis.line = element_line(colour = "black"))
# 
# # Great Sea Reef sedimentation theme
# gsr_sed_theme <- theme(axis.text=element_text(size=8),
#                        axis.title=element_text(size=10),
#                        plot.title=element_text(size=12),
#                        panel.grid.major = element_line(color = "transparent"), 
#                        panel.grid.minor = element_line(color = "transparent"),
#                        panel.background = element_rect(fill = water_col),
#                        axis.text.y = element_text(angle = 90, hjust = 0.5),
#                        legend.position =  c(0.20,0.90), # alternative = "bottom"
#                        legend.title = element_text(size = 8), # text size of legend title
#                        legend.text = element_text(size=6), # text size of the descriptor
#                        legend.background = element_rect(fill = "transparent"), # remove white background around fill
#                        legend.direction = "horizontal", # make the scale horizontal instead of vertical
#                        legend.key.size = unit(0.1, "in"), # size of the color box
#                        legend.key = element_rect(fill = "transparent"), # remove the white background behind legend
#                        axis.line = element_line(colour = "black"))
# 
# # Great Sea Reef bathymetry theme
# gsr_bath_theme <- theme(axis.text=element_text(size=8), # size of the longitude/latitude numbers
#                         axis.title=element_text(size=10), # size of xy-axis title
#                         plot.title=element_text(size=12), # size of plot title
#                         panel.grid.major = element_line(color = "transparent"), # removes grid lines
#                         panel.grid.minor = element_line(color = "transparent"), # removes grid lines
#                         panel.background = element_rect(fill = water_col),
#                         axis.text.y = element_text(angle = 90, hjust = 0.5),
#                         legend.position =  c(0.20,0.90), # alternative = "bottom"
#                         legend.title = element_text(size = 8), # text size of legend title
#                         legend.text = element_text(size=6), # text size of the descriptor
#                         legend.background = element_rect(fill = "transparent"), # remove white background around fill
#                         legend.key.size = unit(0.1, "in"), # size of the color box
#                         legend.key = element_rect(fill = "transparent"), # remove the white background behind legend
#                         axis.line = element_line(colour = "black"))
# 
# # Maps
# ###########################
# # Create the loop for the provincial coral maps
# i <- 2 # use to test the loop to produce a single province coral data, otherwise start with for loop
# 
# for (i in 1:nrow(provinces)){
#   # get province
#   province_do <- provinces[i,]
#   # get the limits
#   xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
#   ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
#   
#   # extract province name
#   prov_name <- province_do$Name
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(177,178,0.25)}
#   if(i==2){xbreaks <- seq(177,180,0.25)}
#   if(i==3){xbreaks <- seq(175,180,0.25)}
#   if(i==4){xbreaks <- seq(177,178,0.25)}
#   if(i==5){xbreaks <- seq(177,178.5,0.25)}
#   
#   # create plot
#   province_coral <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load coral data
#     geom_tile(data = coral_map, aes(x=longitude, y=latitude, fill=coral)) + 
#     # load province
#     geom_sf(data = province_do, fill = NA, aes(color = Name), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_prov, 
#              ylim = ylim_prov) + 
#     # legend
#     scale_fill_manual(values=coral_col) + 
#     scale_color_manual(values = "grey30") +
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) + 
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_prov +
#     narrow_prov +
#     # theme
#     theme_bw() + 
#     map_theme_province
#   
#   # Plot map
#   province_coral
#   
#   
#   # Export plot
#   out_file <- paste0(prov_name,"_coral.tiff")
#   ggsave(province_coral, filename=file.path(province_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# 
# # Create the loop for the provincial seagrass maps
# for (i in 1:nrow(provinces)){
#   # get province
#   province_do <- provinces[i,]
#   # get the limits
#   xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
#   ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
#   
#   # extract province name
#   prov_name <- province_do$Name
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(177,178,0.25)}
#   if(i==2){xbreaks <- seq(177,180,0.25)}
#   if(i==3){xbreaks <- seq(175,180,0.25)}
#   if(i==4){xbreaks <- seq(177,178,0.25)}
#   if(i==5){xbreaks <- seq(177,178.5,0.25)}
#   
#   # create plot
#   province_seagrass <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load seagrass data
#     geom_tile(data = seagrass_map, aes(x=longitude, y=latitude, fill=seagrass)) + 
#     # load province
#     geom_sf(data = province_do, fill = NA, aes(color = Name), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_prov, 
#              ylim = ylim_prov) + 
#     # legend
#     scale_fill_manual(values=seagrass_col) + 
#     scale_color_manual(values = "grey30") +
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_prov +
#     narrow_prov +
#     # theme
#     theme_bw() + 
#     map_theme_province
#   
#   # Plot map
#   province_seagrass
#   
#   # Export plot
#   out_file <- paste0(prov_name,"_seagrass.tiff")
#   ggsave(province_seagrass, filename=file.path(province_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the provincial mangrove maps
# for (i in 1:nrow(provinces)){
#   # get province
#   province_do <- provinces[i,]
#   # get the limits
#   xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
#   ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
#   
#   # extract province name
#   prov_name <- province_do$Name
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(177,178,0.25)}
#   if(i==2){xbreaks <- seq(177,180,0.25)}
#   if(i==3){xbreaks <- seq(175,180,0.25)}
#   if(i==4){xbreaks <- seq(177,178,0.25)}
#   if(i==5){xbreaks <- seq(177,178.5,0.25)}
#   
#   # create plot
#   province_mangrove <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load mangrove data
#     geom_tile(data = mangrove16_map, aes(x=longitude, y=latitude, fill=mangrove)) + 
#     # load province
#     geom_sf(data = province_do, fill = NA, aes(color = Name), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_prov, 
#              ylim = ylim_prov) + 
#     # legend
#     scale_fill_manual(values=mangrove16_col) + 
#     scale_color_manual(values="grey30") +
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_prov +
#     narrow_prov +
#     # theme
#     theme_bw() + 
#     map_theme_province
#   
#   # Plot map
#   province_mangrove
#   
#   # Export plot
#   out_file <- paste0(prov_name,"_mangrove.tiff")
#   ggsave(province_mangrove, filename=file.path(province_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the provincial mangrove change maps
# for (i in 1:nrow(provinces)){
#   # get province
#   province_do <- provinces[i,]
#   # get the limits
#   xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
#   ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
#   
#   # extract province name
#   prov_name <- province_do$Name
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(177,178,0.25)}
#   if(i==2){xbreaks <- seq(177,180,0.25)}
#   if(i==3){xbreaks <- seq(175,180,0.25)}
#   if(i==4){xbreaks <- seq(177,178,0.25)}
#   if(i==5){xbreaks <- seq(177,178.5,0.25)}
#   
#   # create plot
#   province_mangrove_change <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load mangrove change data
#     geom_tile(data = mangrove_gain_map, aes(x=longitude, y=latitude, fill=gain)) + 
#     geom_tile(data = mangrove_loss_map, aes(x=longitude, y=latitude, fill=loss)) + 
#     # load province
#     geom_sf(data = province_do, fill = NA, aes(color = Name), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_prov, 
#              ylim = ylim_prov) + 
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) + 
#     # legend
#     scale_fill_manual(labels = c("Mangrove gain",
#                                  "Mangrove loss"),
#                       values=c(mangrove_gain,
#                                mangrove_loss)) + 
#     scale_color_manual(values="grey30") +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_prov +
#     narrow_prov +
#     # theme
#     theme_bw() + 
#     map_theme_province
#   
#   # Plot map
#   province_mangrove_change
#   
#   # Export plot
#   out_file <- paste0(prov_name,"_mangrove_change.tiff")
#   ggsave(province_mangrove_change, filename=file.path(province_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the provincial geomorphic maps
# for (i in 1:nrow(provinces)){
#   # get province
#   province_do <- provinces[i,]
#   # get the limits
#   xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
#   ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
#   
#   # extract province name
#   prov_name <- province_do$Name
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(177,178,0.25)}
#   if(i==2){xbreaks <- seq(177,180,0.25)}
#   if(i==3){xbreaks <- seq(175,180,0.25)}
#   if(i==4){xbreaks <- seq(177,178,0.25)}
#   if(i==5){xbreaks <- seq(177,178.5,0.25)}
#   
#   # create plot
#   province_geomorphic <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load geomorphic data
#     geom_tile(data = irf_map, aes(x=longitude, y=latitude, fill=irf)) + # inner reef flat
#     geom_tile(data = orf_map, aes(x=longitude, y=latitude, fill=orf)) + # outer reef flat
#     geom_tile(data = plat_map, aes(x=longitude, y=latitude, fill=plat)) + # plateau
#     geom_tile(data = rc_map, aes(x=longitude, y=latitude, fill=rc)) + # reef crest
#     geom_tile(data = rs_map, aes(x=longitude, y=latitude, fill=rs)) + # reef slope
#     geom_tile(data = sl_map, aes(x=longitude, y=latitude, fill=sl)) + # shallow lagoon
#     geom_tile(data = srs_map, aes(x=longitude, y=latitude, fill=srs)) + # sheltered reef slope
#     geom_tile(data = trf_map, aes(x=longitude, y=latitude, fill=trf)) + # terrestrial reef flat
#     geom_tile(data = unk_map, aes(x=longitude, y=latitude, fill=unk)) + # unknown
#     # load province
#     geom_sf(data = province_do, fill = NA, aes(color = Name), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_prov, 
#              ylim = ylim_prov) + 
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) + 
#     # legend
#     scale_fill_manual(name = "Geomorphic zone",
#                       guide = guide_legend(ncol = 2),
#                       values=c(irf_col,
#                                orf_col,
#                                plat_col,
#                                rc_col,
#                                rs_col,
#                                sl_col,
#                                srs_col,
#                                trf_col,
#                                unk_col)) + 
#     scale_color_manual(name = "Province",
#                        values = "grey30") +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_prov +
#     narrow_prov +
#     # theme
#     theme_bw() + 
#     prov_theme_title
#   
#   # Plot map
#   province_geomorphic
#   
#   # Export plot
#   out_file <- paste0(prov_name,"_geomorphic.tiff")
#   ggsave(province_geomorphic, filename=file.path(province_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the provincial sedimentation maps
# for (i in 1:nrow(provinces)){
#   # get province
#   province_do <- provinces[i,]
#   # get the limits
#   xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
#   ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
#   
#   # extract province name
#   prov_name <- province_do$Name
#   
#   # sediment_map_sample <- sample_frac(sediment_map,0.01)
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(177,178,0.25)}
#   if(i==2){xbreaks <- seq(177,180,0.25)}
#   if(i==3){xbreaks <- seq(175,180,0.25)}
#   if(i==4){xbreaks <- seq(177,178,0.25)}
#   if(i==5){xbreaks <- seq(177,178.5,0.25)}
#   
#   # Create plot
#   province_sediment <- ggplot() + 
#     # load sediment data
#     geom_tile(data = sediment_map, aes(x=longitude, y=latitude, fill = relative)) +
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load province
#     geom_sf(data = province_do, fill = NA, aes(color = Name), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_prov, 
#              ylim = ylim_prov) + 
#     # color sedimentation
#     scale_fill_gradientn(name = "Turbidity \n(relative)",
#                          colors = sed_col,
#                          breaks = seq(0,10,2.5),
#                          na.value=NA) +
#     guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#     # province legend
#     scale_color_manual(name = "Province",
#                        values = "grey30") + 
#     # labels + title
#     labs(x="",y="",title="") + 
#     # change x-axis breaks
#     scale_x_longitude(breaks = xbreaks) +
#     # map elements
#     scalebar_prov +
#     narrow_prov +
#     # theme
#     theme_bw() + 
#     prov_theme_title
#   
#   # Plot map
#   province_sediment
#   
#   # Export plot
#   out_file <- paste0(prov_name,"_sedimentation.tiff")
#   ggsave(province_sediment, filename=file.path(province_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the provincial bathymetry maps
# for (i in 1:nrow(provinces)){
#   # get province
#   province_do <- provinces[i,]
#   # get the limits
#   xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
#   ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
#   
#   # extract province name
#   prov_name <- province_do$Name
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(177,178,0.25)}
#   if(i==2){xbreaks <- seq(177,180,0.25)}
#   if(i==3){xbreaks <- seq(175,180,0.25)}
#   if(i==4){xbreaks <- seq(177,178,0.25)}
#   if(i==5){xbreaks <- seq(177,178.5,0.25)}
#   
#   # create plot
#   province_bath <- ggplot() + 
#     # load bathmetry data
#     geom_tile(data = bath_map, aes(x=longitude, y=latitude, fill = depth)) +
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load province
#     geom_sf(data = province_do, fill = NA, aes(color = Name), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_prov,
#              ylim = ylim_prov) + 
#     # color bathymetry
#     scale_fill_gradientn(name = "Depth (m)",
#                          colors = bath_color,
#                          na.value=NA) +
#     guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) + 
#     # province legend
#     scale_color_manual(name = "Province",
#                        vvalues = "grey30") + 
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) + 
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_prov +
#     narrow_prov +
#     # theme
#     theme_bw() + 
#     prov_theme_title
#   
#   # Plot map
#   province_bath
#   
#   # Export plot
#   out_file <- paste0(prov_name,"_bathymetry.tiff")
#   ggsave(province_bath, filename=file.path(province_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the provincial survery site maps
# for (i in 1:nrow(provinces)){
#   # get province
#   province_do <- provinces[i,]
#   # get the limits
#   xlim_prov <- c(xmin = st_bbox(province_do)$xmin, xmax = st_bbox(province_do)$xmax)
#   ylim_prov <- c(xmin = st_bbox(province_do)$ymin, xmax = st_bbox(province_do)$ymax)
#   
#   # extract province name
#   prov_name <- province_do$Name
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(177,178,0.25)}
#   if(i==2){xbreaks <- seq(177,180,0.25)}
#   if(i==3){xbreaks <- seq(175,180,0.25)}
#   if(i==4){xbreaks <- seq(177,178,0.25)}
#   if(i==5){xbreaks <- seq(177,178.5,0.25)}
#   
#   # create plot
#   province_survey <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load province
#     geom_sf(data = province_do, fill = NA, aes(color = Name), size = 0.5) +
#     # load coral data
#     geom_tile(data = coral_map, aes(x=longitude,y=latitude), fill=coral_col) +
#     # load suvery site data
#     geom_sf(data = surv_site, aes(fill=surveyor, shape=surveyor),show.legend = "point") +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_prov, 
#              ylim = ylim_prov) + 
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) +
#     # surveyor shape
#     scale_shape_manual(name = "Surveyor",
#                        values = c(21,
#                                   22,
#                                   23,
#                                   24)) + 
#     # surveyor fill
#     scale_fill_manual(values = c(eia_col,
#                                  new_col,
#                                  rfc_col,
#                                  wwf_col))+
#     # province legend
#     scale_color_manual(name = "Province",
#                        values = "grey30",
#                        guide = guide_legend(override.aes = list(linetype = 1, shape = NA))) + 
#     # keep only surveyor and province
#     guides(fill = FALSE) + 
#     # repel text of sites in area of interest
#     ggrepel::geom_text_repel(data = filter(surv_site, province == prov_name),
#                              mapping = aes(x = longitude,
#                                            y = latitude,
#                                            label = site,
#                                            geometry = geometry),
#                              stat = "sf_coordinates",
#                              size = 1,
#                              fontface = "bold",
#                              nudge_x = 20,
#                              nudge_y = 30,
#                              max.iter = 1500) + 
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_prov +
#     narrow_prov +
#     # theme
#     theme_bw() + 
#     prov_theme_title
#   
#   # Plot map
#   province_survey
#   
#   
#   # Export plot
#   out_file <- paste0(prov_name,"_survey.tiff")
#   ggsave(province_survey, filename=file.path(province_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# 
# 
# # Create the loop for the qoliqoli coral maps
# i <- 2 # use to test the loop to produce a single qoliqoli coral data, otherwise start with for loop
# 
# for (i in 1:nrow(qoliqoli)){
#   # get qoliqoli
#   qoliqoli_do <- qoliqoli[i,]
#   # get the limits
#   xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin, xmax = st_bbox(qoliqoli_do)$xmax)
#   ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
#   
#   # extract qoliqoli name
#   qoli_name <- qoliqoli_do$District
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(178.2,178.8,0.10)} # Lekutu and Navakasiga
#   if(i==2){xbreaks <- seq(178.5,179.5,0.20)} # Macuata, Seaqaqa, Dreketi, Sasa and Mali
#   if(i==3){xbreaks <- seq(176.75,177.25,0.25)} # Malolo
#   if(i==4){xbreaks <- seq(177.2,177.4,0.05)} # Nacula
#   if(i==5){xbreaks <- seq(179.4,179.6,0.05)} # Nadogo
#   if(i==6){xbreaks <- seq(177.3,177.7,0.10)} # Nailaga
#   if(i==7){xbreaks <- seq(177.4,177.7,0.05)} # Nailaga & Bulu
#   if(i==8){xbreaks <- seq(179.5,180.0,0.05)} # Namuka & Nadogo
#   if(i==9){xbreaks <- seq(-1.0,180.0,0.15)} # Udu
#   if(i==10){xbreaks <- seq(177.25,177.75,0.10)} # Vitogo
#   if(i==11){xbreaks <- seq(177.0,177.5,0.10)} # Vuda & Waya
#   if(i==12){xbreaks <- seq(177.8,178.8,0.20)} # Vuya & Bua
#   if(i==13){xbreaks <- seq(177.25,178.0,0.25)} # Yasawa & Nacula
#   
#   # create plot
#   qoliqoli_coral <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load coral data
#     geom_tile(data = coral_map, aes(x=longitude, y=latitude, fill=coral)) +
#     # load qoliqoli
#     geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_qoli, 
#              ylim = ylim_qoli) + 
#     # legend
#     scale_fill_manual(values=coral_col) + 
#     scale_color_manual(values = "grey30") +
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_dist +
#     narrow_dist +
#     # theme
#     theme_bw() + 
#     map_theme_district
#   
#   # Plot map
#   qoliqoli_coral
#   
#   
#   # Export plot
#   out_file <- paste0(qoli_name,"_coral.tiff")
#   ggsave(qoliqoli_coral, filename=file.path(qoliqoli_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the qoliqoli seagrass maps
# for (i in 1:nrow(qoliqoli)){
#   # get qoliqoli
#   qoliqoli_do <- qoliqoli[i,]
#   # get the limits
#   xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin, xmax = st_bbox(qoliqoli_do)$xmax)
#   ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
#   
#   # extract qoliqoli name
#   qoli_name <- qoliqoli_do$District
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(178.2,178.8,0.10)} # Lekutu and Navakasiga
#   if(i==2){xbreaks <- seq(178.5,179.5,0.20)} # Macuata, Seaqaqa, Dreketi, Sasa and Mali
#   if(i==3){xbreaks <- seq(176.75,177.25,0.25)} # Malolo
#   if(i==4){xbreaks <- seq(177.2,177.4,0.05)} # Nacula
#   if(i==5){xbreaks <- seq(179.4,179.6,0.05)} # Nadogo
#   if(i==6){xbreaks <- seq(177.3,177.7,0.10)} # Nailaga
#   if(i==7){xbreaks <- seq(177.4,177.7,0.05)} # Nailaga & Bulu
#   if(i==8){xbreaks <- seq(179.5,180.0,0.05)} # Namuka & Nadogo
#   if(i==9){xbreaks <- seq(-1.0,180.0,0.15)} # Udu
#   if(i==10){xbreaks <- seq(177.25,177.75,0.10)} # Vitogo
#   if(i==11){xbreaks <- seq(177.0,177.5,0.10)} # Vuda & Waya
#   if(i==12){xbreaks <- seq(177.8,178.8,0.20)} # Vuya & Bua
#   if(i==13){xbreaks <- seq(177.25,178.0,0.25)} # Yasawa & Nacula
#   
#   # create plot
#   qoliqoli_seagrass <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load seagrass data
#     geom_tile(data = seagrass_map, aes(x=longitude, y=latitude, fill=seagrass)) +
#     # load qoliqoli
#     geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_qoli, 
#              ylim = ylim_qoli) + 
#     # legend
#     scale_fill_manual(values = seagrass_col) + 
#     scale_color_manual(values = "grey30") +
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_dist +
#     narrow_dist +
#     # theme
#     theme_bw() + 
#     map_theme_district
#   
#   # Plot map
#   qoliqoli_seagrass
#   
#   # Export plot
#   out_file <- paste0(qoli_name,"_seagrass.tiff")
#   ggsave(qoliqoli_seagrass, filename=file.path(qoliqoli_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the qoliqoli mangrove maps
# for (i in 1:nrow(qoliqoli)){
#   # get qoliqoli
#   qoliqoli_do <- qoliqoli[i,]
#   # get the limits
#   xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin, xmax = st_bbox(qoliqoli_do)$xmax)
#   ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
#   
#   # extract qoliqoli name
#   qoli_name <- qoliqoli_do$District
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(178.2,178.8,0.10)} # Lekutu and Navakasiga
#   if(i==2){xbreaks <- seq(178.5,179.5,0.20)} # Macuata, Seaqaqa, Dreketi, Sasa and Mali
#   if(i==3){xbreaks <- seq(176.75,177.25,0.25)} # Malolo
#   if(i==4){xbreaks <- seq(177.2,177.4,0.05)} # Nacula
#   if(i==5){xbreaks <- seq(179.4,179.6,0.05)} # Nadogo
#   if(i==6){xbreaks <- seq(177.3,177.7,0.10)} # Nailaga
#   if(i==7){xbreaks <- seq(177.4,177.7,0.05)} # Nailaga & Bulu
#   if(i==8){xbreaks <- seq(179.5,180.0,0.05)} # Namuka & Nadogo
#   if(i==9){xbreaks <- seq(-1.0,180.0,0.15)} # Udu
#   if(i==10){xbreaks <- seq(177.25,177.75,0.10)} # Vitogo
#   if(i==11){xbreaks <- seq(177.0,177.5,0.10)} # Vuda & Waya
#   if(i==12){xbreaks <- seq(177.8,178.8,0.20)} # Vuya & Bua
#   if(i==13){xbreaks <- seq(177.25,178.0,0.25)} # Yasawa & Nacula
#   
#   # create plot
#   qoliqoli_mangrove <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load mangrove data
#     geom_tile(data = mangrove16_map, aes(x=longitude, y=latitude, fill=mangrove)) + 
#     # load qoliqoli
#     geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_qoli, 
#              ylim = ylim_qoli) + 
#     # legend
#     scale_fill_manual(values=mangrove16_col) + 
#     scale_color_manual(values = "grey30") +
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_dist +
#     narrow_dist +
#     # theme
#     theme_bw() + 
#     map_theme_district
#   
#     
#   # Plot map
#   qoliqoli_mangrove
#   
#   # Export plot
#   out_file <- paste0(qoli_name,"_mangrove.tiff")
#   ggsave(qoliqoli_mangrove, filename=file.path(qoliqoli_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the qoliqoli mangrove change maps
# for (i in 1:nrow(qoliqoli)){
#   # get qoliqoli
#   qoliqoli_do <- qoliqoli[i,]
#   # get the limits
#   xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin, xmax = st_bbox(qoliqoli_do)$xmax)
#   ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
#   
#   # extract qoliqoli name
#   qoli_name <- qoliqoli_do$District
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(178.2,178.8,0.10)} # Lekutu and Navakasiga
#   if(i==2){xbreaks <- seq(178.5,179.5,0.20)} # Macuata, Seaqaqa, Dreketi, Sasa and Mali
#   if(i==3){xbreaks <- seq(176.75,177.25,0.25)} # Malolo
#   if(i==4){xbreaks <- seq(177.2,177.4,0.05)} # Nacula
#   if(i==5){xbreaks <- seq(179.4,179.6,0.05)} # Nadogo
#   if(i==6){xbreaks <- seq(177.3,177.7,0.10)} # Nailaga
#   if(i==7){xbreaks <- seq(177.4,177.7,0.05)} # Nailaga & Bulu
#   if(i==8){xbreaks <- seq(179.5,180.0,0.05)} # Namuka & Nadogo
#   if(i==9){xbreaks <- seq(-1.0,180.0,0.15)} # Udu
#   if(i==10){xbreaks <- seq(177.25,177.75,0.10)} # Vitogo
#   if(i==11){xbreaks <- seq(177.0,177.5,0.10)} # Vuda & Waya
#   if(i==12){xbreaks <- seq(177.8,178.8,0.20)} # Vuya & Bua
#   if(i==13){xbreaks <- seq(177.25,178.0,0.25)} # Yasawa & Nacula
#   
#   # create plot
#   qoliqoli_mangrove_change <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load mangrove data
#     #geom_tile(data = mangrove16_map, aes(x=x, y=y, fill=layer), col = mangrove16_col) + 
#     # load mangrove change data
#     geom_tile(data = mangrove_gain_map, aes(x=longitude, y=latitude, fill=gain)) + 
#     geom_tile(data = mangrove_loss_map, aes(x=longitude, y=latitude, fill=loss)) + 
#     # load qoliqoli
#     geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_qoli, 
#              ylim = ylim_qoli) + 
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) + 
#     # legend
#     scale_fill_manual(labels = c("Mangrove gain",
#                                  "Mangrove loss"),
#                       values=c(mangrove_gain,
#                                mangrove_loss)) + 
#     scale_color_manual(values = "grey30") + 
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     scalebar_dist +
#     narrow_dist +
#     # theme
#     theme_bw() +
#     map_theme_district
# 
#     
#   # Plot map
#   qoliqoli_mangrove_change
#   
#   # Export plot
#   out_file <- paste0(qoli_name,"_mangrove_change.tiff")
#   ggsave(qoliqoli_mangrove_change, filename=file.path(qoliqoli_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the qoliqoli geomorphic maps
# for (i in 1:nrow(qoliqoli)){
#   # get province
#   qoliqoli_do <- qoliqoli[i,]
#   # get the limits
#   xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin, xmax = st_bbox(qoliqoli_do)$xmax)
#   ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
#   
#   # extract province name
#   qoli_name <- qoliqoli_do$District
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(178.2,178.8,0.10)} # Lekutu and Navakasiga
#   if(i==2){xbreaks <- seq(178.5,179.5,0.20)} # Macuata, Seaqaqa, Dreketi, Sasa and Mali
#   if(i==3){xbreaks <- seq(176.75,177.25,0.25)} # Malolo
#   if(i==4){xbreaks <- seq(177.2,177.4,0.05)} # Nacula
#   if(i==5){xbreaks <- seq(179.4,179.6,0.05)} # Nadogo
#   if(i==6){xbreaks <- seq(177.3,177.7,0.10)} # Nailaga
#   if(i==7){xbreaks <- seq(177.4,177.7,0.05)} # Nailaga & Bulu
#   if(i==8){xbreaks <- seq(179.5,180.0,0.05)} # Namuka & Nadogo
#   if(i==9){xbreaks <- seq(-1.0,180.0,0.15)} # Udu
#   if(i==10){xbreaks <- seq(177.25,177.75,0.10)} # Vitogo
#   if(i==11){xbreaks <- seq(177.0,177.5,0.10)} # Vuda & Waya
#   if(i==12){xbreaks <- seq(177.8,178.8,0.20)} # Vuya & Bua
#   if(i==13){xbreaks <- seq(177.25,178.0,0.25)} # Yasawa & Nacula
#   
#   # create plot
#   qoliqoli_geomorphic <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load geomorphic data
#     geom_tile(data = irf_map, aes(x=longitude, y=latitude, fill=irf)) + # inner reef flat
#     geom_tile(data = orf_map, aes(x=longitude, y=latitude, fill=orf)) + # outer reef flat
#     geom_tile(data = plat_map, aes(x=longitude, y=latitude, fill=plat)) + # plateau
#     geom_tile(data = rc_map, aes(x=longitude, y=latitude, fill=rc)) + # reef crest
#     geom_tile(data = rs_map, aes(x=longitude, y=latitude, fill=rs)) + # reef slope
#     geom_tile(data = sl_map, aes(x=longitude, y=latitude, fill=sl)) + # shallow lagoon
#     geom_tile(data = srs_map, aes(x=longitude, y=latitude, fill=srs)) + # sheltered reef slope
#     geom_tile(data = trf_map, aes(x=longitude, y=latitude, fill=trf)) + # terrestrial reef flat
#     geom_tile(data = unk_map, aes(x=longitude, y=latitude, fill=unk)) + # unknown
#     # load qoliqoli
#     geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_qoli, 
#              ylim = ylim_qoli) + 
#     # legend
#     scale_fill_manual(name = "Geomorphic zone",
#                       guide = guide_legend(ncol = 2),
#                       values=c(irf_col,
#                                orf_col,
#                                plat_col,
#                                rc_col,
#                                rs_col,
#                                sl_col,
#                                srs_col,
#                                trf_col,
#                                unk_col)) + 
#     scale_color_manual(name = "District",
#                        values = "grey30") +
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) + 
#     # labels + title
#     labs(x="",y="", title="") + 
#     # map elements
#     scalebar_dist +
#     narrow_dist +
#     # theme
#     theme_bw() + 
#     dist_theme_title
#   
#   # Plot map
#   qoliqoli_geomorphic
#   
#   # Export plot
#   out_file <- paste0(qoli_name,"_geomorphic.tiff")
#   ggsave(qoliqoli_geomorphic, filename=file.path(qoliqoli_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the qoliqoli sedimentation maps
# for (i in 1:nrow(qoliqoli)){
#   # get qoliqoli
#   qoliqoli_do <- qoliqoli[i,]
#   # get the limits
#   xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin, xmax = st_bbox(qoliqoli_do)$xmax)
#   ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
#   
#   # extract qoliqoli name
#   qoli_name <- qoliqoli_do$District
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(178.2,178.8,0.10)} # Lekutu and Navakasiga
#   if(i==2){xbreaks <- seq(178.5,179.5,0.20)} # Macuata, Seaqaqa, Dreketi, Sasa and Mali
#   if(i==3){xbreaks <- seq(176.75,177.25,0.25)} # Malolo
#   if(i==4){xbreaks <- seq(177.2,177.4,0.05)} # Nacula
#   if(i==5){xbreaks <- seq(179.4,179.6,0.05)} # Nadogo
#   if(i==6){xbreaks <- seq(177.3,177.7,0.10)} # Nailaga
#   if(i==7){xbreaks <- seq(177.4,177.7,0.05)} # Nailaga & Bulu
#   if(i==8){xbreaks <- seq(179.5,180.0,0.05)} # Namuka & Nadogo
#   if(i==9){xbreaks <- seq(-1.0,180.0,0.15)} # Udu
#   if(i==10){xbreaks <- seq(177.25,177.75,0.10)} # Vitogo
#   if(i==11){xbreaks <- seq(177.0,177.5,0.10)} # Vuda & Waya
#   if(i==12){xbreaks <- seq(177.8,178.8,0.20)} # Vuya & Bua
#   if(i==13){xbreaks <- seq(177.25,178.0,0.25)} # Yasawa & Nacula
#   
#   # Create plot
#   qoliqoli_sediment <- ggplot() + 
#     # load sediment data
#     geom_tile(data = sediment_map, aes(x=longitude, y=latitude, fill = relative)) +
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load qoliqoli
#     geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_qoli, 
#              ylim = ylim_qoli) + 
#     # color sedimentation
#     scale_fill_gradientn(name = "Turbidity \n(relative)",
#                          colors = sed_col,
#                          breaks = seq(0,10,2.5),
#                          na.value=NA) +
#     guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#     # province legend
#     scale_color_manual(name = "District",
#                        values = "grey30") + 
#     # labels + title
#     labs(x="",y="", title="") + 
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) + 
#     # map elements
#     scalebar_dist +
#     narrow_dist +
#     # theme
#     theme_bw() + 
#     dist_theme_title
#   
#   # Plot map
#   qoliqoli_sediment
#   
#   # Export plot
#   out_file <- paste0(qoli_name,"_sedimentation.tiff")
#   ggsave(qoliqoli_sediment, filename=file.path(qoliqoli_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the qoliqoli bathymetry maps
# for (i in 1:nrow(qoliqoli)){
#   # get qoliqoli
#   qoliqoli_do <- qoliqoli[i,]
#   # get the limits
#   xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin, xmax = st_bbox(qoliqoli_do)$xmax)
#   ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
#   
#   # extract qoliqoli name
#   qoli_name <- qoliqoli_do$District
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(178.2,178.8,0.10)} # Lekutu and Navakasiga
#   if(i==2){xbreaks <- seq(178.5,179.5,0.20)} # Macuata, Seaqaqa, Dreketi, Sasa and Mali
#   if(i==3){xbreaks <- seq(176.75,177.25,0.25)} # Malolo
#   if(i==4){xbreaks <- seq(177.2,177.4,0.05)} # Nacula
#   if(i==5){xbreaks <- seq(179.4,179.6,0.05)} # Nadogo
#   if(i==6){xbreaks <- seq(177.3,177.7,0.10)} # Nailaga
#   if(i==7){xbreaks <- seq(177.4,177.7,0.05)} # Nailaga & Bulu
#   if(i==8){xbreaks <- seq(179.5,180.0,0.05)} # Namuka & Nadogo
#   if(i==9){xbreaks <- seq(-1.0,180.0,0.15)} # Udu
#   if(i==10){xbreaks <- seq(177.25,177.75,0.10)} # Vitogo
#   if(i==11){xbreaks <- seq(177.0,177.5,0.10)} # Vuda & Waya
#   if(i==12){xbreaks <- seq(177.8,178.8,0.20)} # Vuya & Bua
#   if(i==13){xbreaks <- seq(177.25,178.0,0.25)} # Yasawa & Nacula
#   
#   # create plot
#   qoliqoli_bath <- ggplot() + 
#     # load bathmetry data
#     geom_tile(data = bath_map, aes(x=longitude, y=latitude, fill = depth)) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load qoliqoli
#     geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_qoli, 
#              ylim = ylim_qoli) + 
#     # color bathymetry
#     scale_fill_gradientn(name = "Depth (m)",
#                          colors = bath_color,
#                          na.value=NA) +
#     guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) + 
#     # province legend
#     scale_color_manual(name = "District",
#                        values = "grey30") + 
#     # labels + title
#     labs(x="",y="", title="") + 
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) + 
#     # map elements
#     scalebar_dist +
#     narrow_dist +
#     # theme
#     theme_bw() + 
#     dist_theme_title
#   
#   # Plot map
#   qoliqoli_bath
#   
#   # Export plot
#   out_file <- paste0(qoli_name,"_bathymetry.tiff")
#   ggsave(qoliqoli_bath, filename=file.path(qoliqoli_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# 
# # Create the loop for the qoliqoli survery site maps
# for (i in 1:nrow(qoliqoli)){
#   # get qoliqoli
#   qoliqoli_do <- qoliqoli[i,]
#   # get the limits
#   xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin, xmax = st_bbox(qoliqoli_do)$xmax)
#   ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
#   
#   # extract province name
#   qoli_name <- qoliqoli_do$District
#   
#   # x-axis limits
#   if(i==1){xbreaks <- seq(178.2,178.8,0.10)} # Lekutu and Navakasiga
#   if(i==2){xbreaks <- seq(178.5,179.5,0.20)} # Macuata, Seaqaqa, Dreketi, Sasa and Mali
#   if(i==3){xbreaks <- seq(176.75,177.25,0.25)} # Malolo
#   if(i==4){xbreaks <- seq(177.2,177.4,0.05)} # Nacula
#   if(i==5){xbreaks <- seq(179.4,179.6,0.05)} # Nadogo
#   if(i==6){xbreaks <- seq(177.3,177.7,0.10)} # Nailaga
#   if(i==7){xbreaks <- seq(177.4,177.7,0.05)} # Nailaga & Bulu
#   if(i==8){xbreaks <- seq(179.5,180.0,0.05)} # Namuka & Nadogo
#   if(i==9){xbreaks <- seq(-1.0,180.0,0.15)} # Udu
#   if(i==10){xbreaks <- seq(177.25,177.75,0.10)} # Vitogo
#   if(i==11){xbreaks <- seq(177.0,177.5,0.10)} # Vuda & Waya
#   if(i==12){xbreaks <- seq(177.8,178.8,0.20)} # Vuya & Bua
#   if(i==13){xbreaks <- seq(177.25,178.0,0.25)} # Yasawa & Nacula
#   
#   # create plot
#   qoliqoli_survey <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, linetype = "3313", color = "grey50", size = 0.5) +
#     # load qoliqoli
#     geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
#     # load coral data
#     geom_tile(data = coral_map, aes(x=longitude,y=latitude), fill=coral_col) +
#     # load suvery site data
#     geom_sf(data = surv_site, aes(fill=surveyor, shape=surveyor),show.legend = "point") +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_qoli, 
#              ylim = ylim_qoli) + 
#     # x-axis breaks
#     scale_x_longitude(breaks = xbreaks) +
#     # surveyor shape
#     scale_shape_manual(name = "Surveyor",
#                        values = c(21,
#                                   22,
#                                   23,
#                                   24)) + 
#     # surveyor fill
#     scale_fill_manual(values = c(eia_col,
#                                  new_col,
#                                  rfc_col,
#                                  wwf_col))+
#     # province legend
#     scale_color_manual(name = "District",
#                        values = "grey30",
#                        guide = guide_legend(override.aes = list(linetype = 1, shape = NA))) + 
#     # keep only surveyor and district
#     guides(fill = FALSE) + 
#     # repel text of sites in area of interest
#     ggrepel::geom_text_repel(data = filter(surv_site, district == qoli_name),
#                              mapping = aes(x = longitude,
#                                            y = latitude,
#                                            label = site,
#                                            geometry = geometry),
#                              stat = "sf_coordinates",
#                              size = 1,
#                              fontface = "bold",
#                              nudge_x = 20,
#                              nudge_y = 30,
#                              max.iter = 1500) +
#     # labels + title
#     labs(x="",y="", title="") + 
#     # map elements
#     scalebar_dist +
#     narrow_dist +
#     # theme
#     theme_bw() + 
#     dist_theme_title
#   
#   # Plot map
#   qoliqoli_survey
#   
#   
#   # Export plot
#   out_file <- paste0(qoli_name,"_survey.tiff")
#   ggsave(qoliqoli_survey, filename=file.path(qoliqoli_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# 
# 
# 
# # Create the loop for the Great Sea Reef coral maps
# 
# for (i in 1:nrow(gsr)){
#   # get Great Sea Reef
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract gsr name
#   gsr_name <- "GSR"
#   
#   # create plot
#   gsr_coral <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load coral data
#     geom_tile(data = coral_map, aes(x=longitude, y=latitude, fill=coral)) + 
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # legend
#     scale_fill_manual(values=coral_col) + 
#     scale_color_manual(values = "grey30") + 
#     # x-axis edits
#     scale_x_longitude(breaks = seq(-178,180,0.5)) +
#     # labels + title
#     labs(x="",y="", title="") + 
#     # map elements
#     gsr_scale +
#     gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#   
#   # Plot map
#   gsr_coral
#   
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_coral.tiff")
#   ggsave(gsr_coral, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the Great Sea Reef seagrass maps
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
#   # create plot
#   gsr_seagrass <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load seagrass data
#     geom_tile(data = seagrass_map, aes(x=longitude, y=latitude, fill=seagrass)) + 
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # legend
#     scale_fill_manual(values=seagrass_col) + 
#     scale_color_manual(values="grey30") + 
#     # x-axis edits
#     scale_x_longitude(breaks = seq(-178,180,0.5)) +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     gsr_scale +
#     gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#   
#   # Plot map
#   gsr_seagrass
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_seagrass.tiff")
#   ggsave(gsr_seagrass, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the Great Sea Reef mangrove maps
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
#   # create plot
#   gsr_mangrove <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load mangrove data
#     geom_tile(data = mangrove16_map, aes(x=longitude, y=latitude, fill=mangrove)) + 
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # legend
#     scale_fill_manual(values=mangrove16_col) + 
#     scale_color_manual(values="grey30") +
#     # labels + title
#     labs(x="",y="", title="") + 
#     # x-axis edits
#     scale_x_longitude(breaks = seq(-178,180,0.5)) +
#     # map elements
#     gsr_scale +
#     gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#   
#   # Plot map
#   gsr_mangrove
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_mangrove.tiff")
#   ggsave(gsr_mangrove, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the Great Sea Reef mangrove change maps
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
#   # create plot
#   gsr_mangrove_change <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load mangrove data
#     #geom_tile(data = mangrove16_map, aes(x=x, y=y, fill=layer), col = mangrove16_col) + 
#     # load mangrove change data
#     geom_tile(data = mangrove_gain_map, aes(x=longitude, y=latitude, fill=gain)) + 
#     geom_tile(data = mangrove_loss_map, aes(x=longitude, y=latitude, fill=loss)) + 
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # legend
#     scale_fill_manual(labels = c("Mangrove gain",
#                                  "Mangrove loss"),
#                       values=c(mangrove_gain,
#                                mangrove_loss)) + 
#     scale_color_manual(values="grey30") +
#     # x-axis edits
#     scale_x_longitude(breaks = seq(-178,180,0.5)) +
#     # labels + title
#     labs(x="",y="", title="") + 
#     # map elements
#      gsr_scale +
#      gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#   
#   # Plot map
#   gsr_mangrove_change
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_mangrove_change.tiff")
#   ggsave(gsr_mangrove_change, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the Great Sea Reef geomorphic maps
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
#   # create plot
#   gsr_geomorphic <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load geomorphic data
#     geom_tile(data = irf_map, aes(x=longitude, y=latitude, fill=irf)) + # inner reef flat
#     geom_tile(data = orf_map, aes(x=longitude, y=latitude, fill=orf)) + # outer reef flat
#     geom_tile(data = plat_map, aes(x=longitude, y=latitude, fill=plat)) + # plateau
#     geom_tile(data = rc_map, aes(x=longitude, y=latitude, fill=rc)) + # reef crest
#     geom_tile(data = rs_map, aes(x=longitude, y=latitude, fill=rs)) + # reef slope
#     geom_tile(data = sl_map, aes(x=longitude, y=latitude, fill=sl)) + # shallow lagoon
#     geom_tile(data = srs_map, aes(x=longitude, y=latitude, fill=srs)) + # sheltered reef slope
#     geom_tile(data = trf_map, aes(x=longitude, y=latitude, fill=trf)) + # terrestrial reef flat
#     geom_tile(data = unk_map, aes(x=longitude, y=latitude, fill=unk)) + # unknown
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = ""), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # legend
#     scale_fill_manual(name = "Geomorphic zone",
#                       guide = guide_legend(ncol = 2),
#                       values=c(irf_col,
#                                orf_col,
#                                plat_col,
#                                rc_col,
#                                rs_col,
#                                sl_col,
#                                srs_col,
#                                trf_col,
#                                unk_col)) + 
#     scale_color_manual(name = "Great Sea Reef",
#                        values = "grey30") +
#     # x-axis breaks
#     scale_x_longitude(breaks = seq(-178,180,0.5)) + 
#     # labels + title
#     labs(x="",y="", title="") + 
#     # map elements
#     gsr_scale +
#     gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#   
#   # Plot map
#   gsr_geomorphic
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_geomorphic.tiff")
#   ggsave(gsr_geomorphic, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Create the loop for the Great Sea Reef sedimentation maps
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
#   # Create plot
#   gsr_sediment <- ggplot() + 
#     # load bathmetry data
#     geom_tile(data = sediment_map, aes(x=longitude, y=latitude, fill = relative)) +
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = ""), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # color sedimentation
#     scale_fill_gradientn(name = "Turbidity \n(relative)",
#                          colors = sed_col,
#                          breaks = seq(0,10,2.5),
#                          na.value=NA) +
#     guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#     scale_color_manual(name = "Great Sea Reef",
#                        values = "grey30") +
#     # labels + title
#     labs(x="",y="", title="") + 
#     # x-axis breaks
#     scale_x_longitude(breaks = seq(-178,180,0.5)) + 
#     # map elements
#     gsr_scale +
#     gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#   
#   # Plot map
#   gsr_sediment
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_sedimentation.tiff")
#   ggsave(gsr_sediment, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# 
# # Create the loop for the Great Sea Reef sedimentation maps focused on Vanua Levu
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = 650000, xmax = 820000)
#   ylim_gsr <- c(ymin = 8100000, ymax = 8220000)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
#   # Create plot
#   gsr_sediment_vl <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = ""), size = 0.5) +
#     # load bathmetry data
#     geom_tile(data = sediment_map, aes(x=longitude, y=latitude, fill = relative)) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # add ticks and longitude
#     scale_x_longitude(breaks = seq(178,180,0.5)) +
#     # color sedimentation
#     scale_fill_gradientn(name = "Turbidity \n(relative)",
#                          colors = sed_col,
#                          breaks = seq(0,10,2.5),
#                          na.value=NA) +
#     guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#     scale_color_manual(name = "Great Sea Reef",
#                        values = "grey30") +
#     # labels + title
#     labs(x="",y="",title="") + 
#     # map elements
#     gsr_scale_sed +
#     gsr_narrow_sed +
#     # theme
#     theme_bw() + 
#     gsr_sed_theme
#   
#   # Plot map
#   gsr_sediment_vl
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_sedimentation_VanuaLevu.tiff")
#   ggsave(gsr_sediment_vl, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# 
# # Create the loop for the Great Sea Reef bathymetry maps
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
#   # create plot
#   gsr_bath <- ggplot() + 
#     # load bathmetry data
#     geom_tile(data = bath_map, aes(x=longitude, y=latitude, fill = depth)) +
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = ""), size = 0.5) +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # color bathymetry
#     scale_fill_gradientn(name = "Depth (m)",
#                          colors = bath_color,
#                          na.value=NA) +
#     guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
#     scale_color_manual(name = "Great Sea Reef",
#                        values = "grey30") +
#     # x-axis edits
#     scale_x_longitude(breaks = seq(-178,180,0.5)) +
#     # labels + title
#     labs(x="",y="", title="") + 
#     # map elements
#     gsr_scale +
#     gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#   
#   # Plot map
#   gsr_bath
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_bathymetry.tiff")
#   ggsave(gsr_bath, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# 
# # Create the loop for the Great Sea Reef survery site maps
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract province name
#   gsr_name <- "GSR"
#   
#   
#   # create plot
#   gsr_survey <- ggplot() +
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
#     # load coral data
#     geom_tile(data = coral_map, aes(x=longitude,y=latitude), fill=coral_col) +
#     # load suvery site data
#     geom_sf(data = surv_site, aes(fill=surveyor, shape=surveyor),show.legend = "point") +
#     # focus on the area of interest
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) + 
#     # surveyor shape
#     scale_shape_manual(name = "Surveyor",
#                        values = c(21,
#                                   22,
#                                   23,
#                                   24)) + 
#     # surveyor fill
#     scale_fill_manual(values = c(eia_col,
#                                  new_col,
#                                  rfc_col,
#                                  wwf_col))+
#     # province legend
#     scale_color_manual(name = "District",
#                        values = "grey30",
#                        guide = guide_legend(override.aes = list(linetype = 1, shape = NA))) + 
#     # keep only surveyor and district
#     guides(fill = FALSE) + 
#     # labeling survey sites
#     ggrepel::geom_text_repel(data = surv_site,
#                              mapping = aes(x = longitude,
#                                            y = latitude,
#                                            label = site,
#                                            geometry = geometry),
#                              stat = "sf_coordinates",
#                              size = 1,
#                              fontface = "bold",
#                              nudge_x = 20,
#                              nudge_y = 30,
#                              max.iter = 1500) +
#     # labels + title
#     labs(x="",y="", title="") + 
#     # map elements
#     gsr_scale +
#     gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#    
#   # Plot map
#   gsr_survey
#   
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_survey.tiff")
#   ggsave(gsr_survey, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Great Sea Reef map with provinces
# # get the limits
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
#   # plot
#   gsr_provinces <- ggplot() + 
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
#     # load province
#     geom_sf(data = provinces, fill = NA, aes(color = "Provinces"), size = 0.5) +
#     # legend
#     scale_color_manual(values = c("grey30",
#                                   "grey50")) +
#     # focus on Great Sea Reef
#     coord_sf(xlim = xlim_gsr, 
#              ylim = ylim_gsr) +
#     # x-axis edits
#     scale_x_longitude(breaks = seq(-178,180,0.5)) +
#     # labels + title
#     labs(x="",y="", title="") + 
#     # map elements
#     gsr_scale +
#     gsr_narrow +
#     # theme
#     theme_bw() + 
#     map_theme_gsr
#   
#   # Plot map
#   gsr_provinces
# 
# 
# # Export plot
# out_file <- paste0(gsr_name,"_provinces.tiff")
# ggsave(gsr_provinces, filename=file.path(gsr_map_dir, out_file), width=6.5,
#        height=4.5, units="in", dpi=600, compression = "lzw")
# }
# 
# # Great Sea Reef map with provinces
# # get the limits
# for (i in 1:nrow(gsr)){
#   # get gsr
#   gsr_do <- gsr[i,]
#   # get the limits
#   xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin, xmax = st_bbox(gsr_do)$xmax)
#   ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
#   
#   # extract Great Sea Reef name
#   gsr_name <- "GSR"
#   
# 
#   # Great Sea Reef with districts
#   gsr_districts <- ggplot() +
#     # load Fiji land
#     geom_sf(data = fiji, fill = land_col, color = NA) +
#     # load Great Sea Reef
#     geom_sf(data = gsr,fill = NA,aes(color = "Great Sea Reef"),size = 0.5) +
#     # load districts
#     geom_sf(data = qoliqoli, fill = NA, aes(color = "Districts"),size = 0.5) +
#     # legend
#     scale_color_manual(values = c("grey50",
#                                   "grey30")) +
#     # focus on Great Sea Reef
#     coord_sf(xlim = xlim_gsr,
#              ylim = ylim_gsr) +
#     # x-axis edits
#     scale_x_longitude(breaks = seq(-178, 180, 0.5)) +
#     # labels + title
#     labs(x = "", y = "", title = "") +
#     # map elements
#     scalebar_gsr +
#     narrow_gsr +
#     # theme
#     theme_bw() +
#     map_theme_gsr
# 
#   # Plot map
#   gsr_districts
#   
#   
#   # Export plot
#   out_file <- paste0(gsr_name,"_districts.tiff")
#   ggsave(gsr_districts, filename=file.path(gsr_map_dir, out_file), width=6.5,
#          height=4.5, units="in", dpi=600, compression = "lzw")
# }