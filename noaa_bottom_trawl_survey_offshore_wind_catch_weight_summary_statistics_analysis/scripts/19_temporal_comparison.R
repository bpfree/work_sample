###################################################
###################################################
###################################################


######### Part 19 #########
## Comparing BTS and OWF data
## Weight and count
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
# input directories
geopackage <- "data\\geopackage\\bts_spatial_data.gpkg"
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

state_dir <- "data\\c_spatial_data\\us_states"

# export directories
anim_dir <- "figures\\animations"
csv_dir <- "data\\f_csv_tables"

###################################################
###################################################
###################################################

map_theme <- theme(panel.grid.major = element_blank(),
                   axis.title.x = element_text(size = 10),
                   axis.title.y = element_text(size = 10),
                   plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(size = 10,
                                                face = "bold"),
                   plot.caption = element_text(size = 6,
                                               lineheight = 0.5))

###################################################
###################################################
###################################################

strata <- st_read(dsn = geopackage, layer = "strata")
state <- st_read(dsn = state_dir, layer = "us_states_east_coast")
wind_farm <- st_read(dsn = wind_geopackage, layer = "owf_areas")

###################################################
###################################################
###################################################

bts_temporal <- st_read(dsn = geopackage, layer = "temporal_normalized_point") %>%
  dplyr::rename(bts_count = count,
                bts_diversity = diversity,
                bts_weight = weight,
                bts_tow = tows,
                bts_count_per_tow = count_per_tow,
                bts_weight_per_tow = weight_per_tow,
                bts_geom = geom)

owf_temporal <- st_read(dsn = wind_geopackage, layer = "owf_temporal") %>%
  dplyr::rename(owf_count = count,
                owf_diversity = diversity,
                owf_weight = weight,
                owf_tow = tows,
                owf_count_per_tow = count_per_tow,
                owf_weight_per_tow = weight_per_tow,
                owf_strata_sqnm = sq_nm,
                owf_count_per_sqnm = count_per_sqnm,
                owf_diversity_per_sqnm = diversity_per_sqnm,
                owf_weight_per_sqnm = weight_per_sqnm,
                owf_geom = geom)

###################################################
###################################################
###################################################

temporal_comparison <- owf_temporal %>%
  # join the representative tow data with the tow data within the offshore wind farms
  dplyr::full_join(as.data.frame(bts_temporal),
                   by = c("stratum",
                          "year",
                          "season")) %>%
  # reduce joined data down to unique strata, year, and season combinations for calculations
  dplyr::group_by(stratum,
                  year,
                  season) %>%
  # calculate the means for the counts and weights across the stratas by season
  dplyr::mutate(
    # combined offshore wind farm averages by strata, year, and season
    owf_cpt_mean = round(mean(owf_count_per_tow), 2),
    owf_wpt_mean = round(mean(owf_weight_per_tow), 2),
    # comparing all offshore wind farm tows to the BTS tows
    year_count_comp = round(100 * (owf_cpt_mean / bts_count_per_tow), 2),
    year_weight_comp = round(100 * (owf_wpt_mean / bts_weight_per_tow), 2),
    # comparing individual offshore wind farms to the BTS tows
    owf_count_comp = round(100 * (owf_count_per_tow / bts_count_per_tow), 2),
    owf_weight_comp = round(100 * (owf_weight_per_tow / bts_weight_per_tow), 2)) %>%
  dplyr::select(name, state, stratum, year, season,
                bts_count, bts_diversity, bts_weight,
                bts_tow, bts_count_per_tow, bts_weight_per_tow,
                owf_count, owf_diversity, owf_weight,
                owf_tow, owf_count_per_tow, owf_weight_per_tow,
                owf_cpt_mean, owf_wpt_mean,
                owf_strata_sqnm, owf_count_per_sqnm, owf_diversity_per_sqnm, owf_weight_per_sqnm,
                year_count_comp, year_weight_comp,
                owf_count_comp, owf_weight_comp,
                owf_geom, bts_geom)

temporal_comparison_csv <- as.data.frame(temporal_comparison) %>%
  dplyr::select(-owf_geom, -bts_geom)

###################################################
###################################################
###################################################

temp_comp <- temporal_comparison %>%
  dplyr::filter(year_count_comp > 0 | year_weight_comp > 0) %>%
  dplyr::select(stratum, year, season,
                year_count_comp, year_weight_comp,
                owf_geom)

c(max(temp_comp$year_count_comp), min(temp_comp$year_count_comp))
c(max(temp_comp$year_weight_comp), min(temp_comp$year_weight_comp))

year_comparison_csv <- as.data.frame(temp_comp) %>%
  dplyr::select(-owf_geom)

###################################################

nframes <- max(temp_comp$year) - min(temp_comp$year) # alternative is range(strata_comp$year)[[2]] - range(strata_comp$year)[[1]]
breaks_count <- c(0, 50, 100, 250, 500, 600)

###################################################

year_count_animation <- ggplot() +
  # plot all the strata
  geom_sf(data = strata, fill = "grey99", alpha = 0.8) +
  # plot the wind farm strata
  geom_sf(data = temp_comp, aes(color = year_count_comp), alpha = 0.6) +
  # add the US east coast state boundary data
  geom_sf(data = state, fill = "bisque1", color = "black", linetype = "84") +
  # add Atlantic offshore wind farm
  geom_sf(data = wind_farm, fill = NA, color = "#F6F627", linetype = "62") +
  # redefine the 
  scale_color_gradientn(name = "Count \n(normalized)",
                       # Define the colors
                       colors = c("#d7191c",
                                  "#fdae61",
                                  "#2c7bb6",
                                  "#b2abd2",
                                  "#5e3c99"),
                       breaks = breaks_count) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(temp_comp)$xmin-2, xmax = st_bbox(temp_comp)$xmax)+1,
           ylim = c(ymin = st_bbox(temp_comp)$ymin-0.5, ymax = st_bbox(temp_comp)$ymax+0.5)) +
  # add theme
  theme_bw() +
  # review axis titles
  map_theme +
  # add animation aspects
  labs(x = "Longtitude",
       y = "Latitude",
       title = "Mean count per tow",
       subtitle = "Year: {current_frame}",
       caption = "These were calculated by calculating the mean count by tow for all offshore wind \n
       farms, which was then divided by bottom trawl survey count by tow for the target \n
       strata.") +
  transition_manual(year) +
  ease_aes('linear')
year_count_animation

###################################################

breaks_weight <- c(0, 50, 100, 250, 500, 1000, 1500, 2000)

###################################################

year_weight_animation <- ggplot() +
  # plot all the strata
  geom_sf(data = strata, fill = "grey99", alpha = 0.8) +
  # plot the wind farm strata
  geom_sf(data = temp_comp, aes(color = year_weight_comp), alpha = 0.6) +
  # add the US east coast state boundary data
  geom_sf(data = state, fill = "bisque1", color = "black", linetype = "84") +
  # add Atlantic offshore wind farm
  geom_sf(data = wind_farm, fill = NA, color = "#F6F627", linetype = "62") +
  # redefine the 
  scale_color_gradientn(name = "Weight (kg) \n(normalized)",
                        # Define the colors
                        colors = c("#d7191c",
                                   "#fdae61",
                                   "#2c7bb6",
                                   "#b2abd2",
                                   "#5e3c99"),
                        breaks = breaks_weight) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(temp_comp)$xmin-2, xmax = st_bbox(temp_comp)$xmax)+1,
           ylim = c(ymin = st_bbox(temp_comp)$ymin-0.5, ymax = st_bbox(temp_comp)$ymax+0.5)) +
  # add theme
  theme_bw() +
  # review axis titles
  map_theme +
  # add animation aspects
  labs(x = "Longtitude",
       y = "Latitude",
       title = "Mean weight per tow",
       subtitle = "Year: {current_frame}",
       caption = "These were calculated by calculating the mean weight by tow for all offshore wind \n
       farms, which was then divided by bottom trawl survey weight by tow for the target \n
       strata.") +
  transition_manual(year) +
  ease_aes('linear')
year_weight_animation

###################################################
###################################################
###################################################

owf_temp_comp <- temporal_comparison %>%
  dplyr::filter(owf_count_comp > 0 | owf_weight_comp > 0) %>%
  dplyr::select(stratum, year, season,
                owf_count_comp, owf_weight_comp,
                owf_geom)

c(max(owf_temp_comp$owf_count_comp), min(owf_temp_comp$owf_count_comp))
c(max(owf_temp_comp$owf_weight_comp), min(owf_temp_comp$owf_weight_comp))

owf_temporal_comparison_csv <- as.data.frame(owf_temp_comp) %>%
  dplyr::select(-owf_geom)

###################################################

nframes <- max(owf_temp_comp$year) - min(owf_temp_comp$year) # alternative is range(strata_comp$year)[[2]] - range(strata_comp$year)[[1]]
owf_breaks_count <- c(0, 50, 100, 250, 500, 750, 1000, 1250, 1500)

###################################################

owf_year_count_animation <- ggplot() +
  # plot all the strata
  geom_sf(data = strata, fill = "grey99", alpha = 0.8) +
  # plot the wind farm strata
  geom_sf(data = owf_temp_comp, aes(color = owf_count_comp), alpha = 0.6) +
  # add the US east coast state boundary data
  geom_sf(data = state, fill = "bisque1", color = "black", linetype = "84") +
  # add Atlantic offshore wind farm
  geom_sf(data = wind_farm, fill = NA, color = "#F6F627", linetype = "62") +
  # redefine the 
  scale_color_gradientn(name = "Count \n(normalized)",
                        # Define the colors
                        colors = c("#d7191c",
                                   "#fdae61",
                                   "#2c7bb6",
                                   "#b2abd2",
                                   "#5e3c99"),
                        breaks = owf_breaks_count) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(owf_temp_comp)$xmin-2, xmax = st_bbox(owf_temp_comp)$xmax)+1,
           ylim = c(ymin = st_bbox(owf_temp_comp)$ymin-0.5, ymax = st_bbox(owf_temp_comp)$ymax+0.5)) +
  # add theme
  theme_bw() +
  # review axis titles
  map_theme +
  # add animation aspects
  labs(x = "Longtitude",
       y = "Latitude",
       title = "Mean count per tow",
       subtitle = "Year: {current_frame}",
       caption = "These were calculated by calculating the mean count by tow for individual offshore wind \n
       farms, which was then divided by bottom trawl survey count by tow for the target \n
       strata and year.") +
  transition_manual(year) +
  ease_aes('linear')
owf_year_count_animation

###################################################

owf_breaks_weight <- c(0, 50, 100, 250, 500, 1000, 1500, 2000, 3000, 4000, 5000, 5500)

###################################################

owf_year_weight_animation <- ggplot() +
  # plot all the strata
  geom_sf(data = strata, fill = "grey99", alpha = 0.8) +
  # plot the wind farm strata
  geom_sf(data = owf_temp_comp, aes(color = owf_weight_comp), alpha = 0.6) +
  # add the US east coast state boundary data
  geom_sf(data = state, fill = "bisque1", color = "black", linetype = "84") +
  # add Atlantic offshore wind farm
  geom_sf(data = wind_farm, fill = NA, color = "#F6F627", linetype = "62") +
  # redefine the 
  scale_color_gradientn(name = "Weight \n(normalized)",
                        # Define the colors
                        colors = c("#d7191c",
                                   "#fdae61",
                                   "#2c7bb6",
                                   "#b2abd2",
                                   "#5e3c99"),
                        breaks = owf_breaks_weight) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(owf_temp_comp)$xmin-2, xmax = st_bbox(owf_temp_comp)$xmax)+1,
           ylim = c(ymin = st_bbox(owf_temp_comp)$ymin-0.5, ymax = st_bbox(owf_temp_comp)$ymax+0.5)) +
  # add theme
  theme_bw() +
  # review axis titles
  map_theme +
  # add animation aspects
  labs(x = "Longtitude",
       y = "Latitude",
       title = "Mean weight per tow",
       subtitle = "Year: {current_frame}",
       caption = "These were calculated by calculating the mean weight by tow for individual offshore wind \n
       farms, which was then divided by bottom trawl survey weight by tow for the target \n
       strata.") +
  transition_manual(year) +
  ease_aes('linear')
owf_year_weight_animation

###################################################
###################################################
###################################################
# export data
write.csv(temporal_comparison_csv, file = paste0(csv_dir, "/", "bts_owf_temporal_comparison_data.csv"))
write.csv(year_comparison_csv, file = paste0(csv_dir, "/", "bts_owf_year_comparison_data.csv"))
write.csv(owf_temporal_comparison_csv, file = paste0(csv_dir, "/", "bts_owf_individual_temporal_comparison_data.csv"))

# export animations
anim_save(year_count_animation, filename=file.path(anim_dir, "year_count_animation.gif"), duration=30,
          width=4.5, height=6.5, units="in", res=600)

anim_save(year_weight_animation, filename=file.path(anim_dir, "year_weight_animation.gif"), duration=30,
          width=4.5, height=6.5, units="in", res=600)

anim_save(owf_year_count_animation, filename=file.path(anim_dir, "owf_year_count_animation.gif"), duration=30,
          width=4.5, height=6.5, units="in", res=600)

anim_save(owf_year_weight_animation, filename=file.path(anim_dir, "owf_year_weight_animation.gif"), duration=30,
          width=4.5, height=6.5, units="in", res=600)