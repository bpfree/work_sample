###################################################
###################################################
###################################################


######### Part 17 #########
## Mapping wind farm specific
## Representative surveys
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
geopackage <- "data\\geopackage\\bts_spatial_data.gpkg"
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"
state_dir <- "data\\c_spatial_data\\us_states"

figure_dir <- "figures\\maps"

###################################################
###################################################
###################################################

# load data
strata <- st_read(dsn = geopackage, layer = "strata")
state <- st_read(dsn = state_dir, layer = "us_states_east_coast")
wind_farm <- st_read(dsn = wind_geopackage, layer = "owf_areas")

###################################################

# load wind farm borders and all survey points
# Atlantic
wind_area_survey <- st_read(dsn = wind_geopackage, layer = "atlantic_owf_tows")

###################################################
###################################################
###################################################

# setting specific Offshore Wind Farm(s) of interest
owf_names <- c("South|Vineyard|North")

# subsetting survey data for area(s) of interest
owf_survey <- wind_area_survey %>%
  # select the wind farms of interest
  dplyr::filter(str_detect(name, owf_names))

unique(owf_survey$name)

# subsetting wind farm area(s) of interest
owf_areas <- wind_farm %>%
  dplyr::filter(str_detect(name, owf_names))

unique(owf_areas$name)

###################################################
###################################################
###################################################

## This function will subset out only strata that the survey data for a particular wind area overlap
## wass = wind area survey strata
wass_function <- function(strata, owf_survey){
  wind_area_survey_strata <- strata %>%
    # join strata data to survey strata data
    merge(as.data.frame(owf_survey) %>%
            # group data by stratum
            dplyr::group_by(stratum) %>%
            dplyr::summarise() %>%
            # select only the stratum data
            dplyr::select(stratum),
          # match strata with wind farm strata by field stratum
          by = "stratum") %>%
    # select only fields 
    dplyr::select(stratum)
  return(wind_area_survey_strata)
}

###################################################
###################################################
###################################################

### mapping the wind area survey data overtop of the wind areas, affected strata and overall strata
zoomed_mapping_function <- function(owf_wass, owf_survey){
  owf_all_strata_map <- ggplot() +
    # plot all the strata
    geom_sf(data = strata, fill = "grey87", alpha = 0.76) +
    # add the US east coast state boundary data
    geom_sf(data = state, fill = "grey96", color = "grey80", linetype = "84") +
    # add Atlantic offshore wind farm
    geom_sf(data = wind_farm, fill = "ghostwhite") +
    # add Atlantic offshore wind farms of interest
    geom_sf(data = owf_areas, fill = "NA", color = "red", size = 2, linetype = "64") +
    # plot strata impacted by wind farm
    geom_sf(data = owf_wass, fill = "steelblue2", alpha = 0.4) +
    # plot points where they are in the wind farm strata
    geom_sf(data = owf_survey, color = "#FBFE76", alpha = 0.6, shape = 3) +
    # focus on the stratum
    coord_sf(xlim = c(xmin = st_bbox(owf_wass)$xmin-2, xmax = st_bbox(owf_wass)$xmax)+1,
             ylim = c(ymin = st_bbox(owf_wass)$ymin-0.5, ymax = st_bbox(owf_wass)$ymax+0.5)) +
    # label the stratum
    geom_sf_label(data = owf_wass,
                  aes(label = stratum)) +
    # add theme
    theme_bw() +
    # review axis titles
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank())
  
  return(owf_all_strata_map)
}

# Specific wind farm(s)
owf_wass <- wass_function(strata, owf_survey)
owf_zoomed_map <- zoomed_mapping_function(owf_wass, owf_survey)
owf_zoomed_map

###################################################
###################################################
###################################################

# save maps
# Atlantic
ggsave(owf_zoomed_map, filename = file.path(figure_dir, "owf_zoomed_map.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")
