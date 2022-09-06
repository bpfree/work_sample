###################################################
###################################################
###################################################


######### Part 16 #########
## Mapping wind farm strata
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

## This function will subset out only strata that the survey data for a particular wind area overlap
## wass = wind area survey strata
wass_function <- function(strata, wind_area_survey){
  wind_area_survey_strata <- strata %>%
    # join strata data to survey strata data
    merge(as.data.frame(wind_area_survey) %>%
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
mapping_function <- function(wass, wind_area_survey){
  owf_all_strata_map <- ggplot() +
    # plot all the strata
    geom_sf(data = strata, fill = "grey96", alpha = 0.76) +
    # add the US east coast state boundary data
    geom_sf(data = state, fill = "bisque1", color = "black", linetype = "84") +
    # add Atlantic offshore wind farm
    geom_sf(data = wind_farm, fill = "ghostwhite") +
    # plot strata impacted by wind farm
    geom_sf(data = wass, fill = "steelblue2", alpha = 0.4) +
    # plot points where they are in the wind farm strata
    geom_sf(data = wind_area_survey, color = "#FBFE76", alpha = 0.6, shape = 3) +
    # focus on the stratum
    coord_sf(xlim = c(xmin = st_bbox(wass)$xmin-2, xmax = st_bbox(wass)$xmax)+1,
             ylim = c(ymin = st_bbox(wass)$ymin-0.5, ymax = st_bbox(wass)$ymax+0.5)) +
    # label the stratum
    geom_sf_label(data = wass,
                  aes(label = stratum)) +
    # add theme
    theme_bw() +
    # review axis titles
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank())
  
  return(owf_all_strata_map)
}

###################################################
###################################################
###################################################

# Atlantic
owf_survey_wass <- wass_function(strata, wind_area_survey)
owf_strata_map <- mapping_function(owf_survey_wass, wind_area_survey)
owf_strata_map

###################################################
###################################################
###################################################

# export data
st_write(obj = owf_survey_wass, dsn = wind_geopackage, layer = "owf_survey_wass", append = F)

###################################################
###################################################
###################################################

# save maps
# Atlantic
ggsave(owf_strata_map, filename = file.path(figure_dir, "owf_strata_map.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")
