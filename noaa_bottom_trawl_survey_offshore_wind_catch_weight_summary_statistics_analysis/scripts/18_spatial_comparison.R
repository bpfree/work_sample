###################################################
###################################################
###################################################


######### Part 18 #########
## Comparing BTS and OWF data
## Weight and count
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
csv_dir <- "data\\f_csv_tables"

###################################################
###################################################
###################################################

map_theme <- theme(panel.grid.major = element_blank(),
                   axis.title.x = element_text(size = 10),
                   axis.title.y = element_text(size = 10),
                   plot.subtitle = element_text(size = 8),
                   plot.caption = element_text(size = 6,
                                               lineheight = 0.5))

###################################################
###################################################
###################################################

strata <- st_read(dsn = geopackage, layer = "strata")
state <- st_read(dsn = state_dir, layer = "us_states_east_coast")
wind_farm <- st_read(dsn = wind_geopackage, layer = "owf_areas")

###################################################

bts_spatial <- st_read(dsn = geopackage, layer = "spatial_normalized_tow_point") %>%
  dplyr::rename(bts_count = count,
                bts_diversity = diversity,
                bts_weight = weight,
                bts_tow = tows,
                bts_count_per_tow = count_per_tow,
                bts_weight_per_tow = weight_per_tow,
                bts_strata_sqnm = sq_nm,
                bts_count_per_sqnm = count_per_sqnm,
                bts_diversity_per_sqnm = diversity_per_sqnm,
                bts_weight_per_sqnm = weight_per_sqnm,
                bts_geom = geom)

owf_spatial <- st_read(dsn = wind_geopackage, layer = "owf_spatial") %>%
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

bts_spatial_polygon <- st_read(dsn = geopackage, layer = "spatial_normalized_strata_polygon") %>%
  dplyr::rename(bts_count = count,
                bts_diversity = diversity,
                bts_weight = weight,
                bts_tow = tows,
                bts_count_per_tow = count_per_tow,
                bts_weight_per_tow = weight_per_tow,
                bts_strata_sqnm = sq_nm,
                bts_count_per_sqnm = count_per_sqnm,
                bts_diversity_per_sqnm = diversity_per_sqnm,
                bts_weight_per_sqnm = weight_per_sqnm,
                bts_geom = geom)

###################################################
###################################################
###################################################

spatial_comparison <- owf_spatial %>%
  # join the representative tow data with the tow data within the offshore wind farms
  dplyr::full_join(as.data.frame(bts_spatial),
                   by = c("stratum",
                          "season")) %>%
  # reduce joined data down to unique strata and season combinations
  dplyr::group_by(stratum,
                  season) %>%
  # calculate the means for the counts and weights across the stratas by season
  dplyr::mutate(
    # offshore wind farm averages
    owf_cpt_mean = round(mean(owf_count_per_tow), 2),
    owf_wpt_mean = round(mean(owf_weight_per_tow), 2),
    # comparing all offshore wind farm tows to the BTS tows
    strata_count_comp = round(100 * (owf_cpt_mean / bts_count_per_tow), 2),
    strata_weight_comp = round(100 * (owf_wpt_mean / bts_weight_per_tow), 2),
    # comparing individual offshore wind farms to the BTS tows
    owf_count_comp = round(100 * (owf_count_per_tow / bts_count_per_tow), 2),
    owf_weight_comp = round(100 * (owf_weight_per_tow / bts_weight_per_tow), 2)) %>%
  dplyr::select(stratum, season,
                bts_count, bts_diversity, bts_weight,
                bts_tow, bts_count_per_tow, bts_weight_per_tow,
                bts_strata_sqnm, bts_count_per_sqnm, bts_diversity_per_sqnm, bts_weight_per_sqnm,
                name, state,
                owf_count, owf_diversity, owf_weight,
                owf_tow, owf_count_per_tow, owf_weight_per_tow,
                owf_cpt_mean, owf_wpt_mean,
                owf_strata_sqnm, owf_count_per_sqnm, owf_diversity_per_sqnm, owf_weight_per_sqnm,
                strata_count_comp, strata_weight_comp,
                owf_count_comp, owf_weight_comp,
                owf_geom, bts_geom)

spatial_comparison_polygon <- bts_spatial_polygon %>%
  # join the representative tow data with the tow data within the offshore wind farms
  dplyr::full_join(as.data.frame(owf_spatial),
                   by = c("stratum",
                          "season")) %>%
  # reduce joined data down to unique strata and season combinations
  dplyr::group_by(stratum,
                  season) %>%
  # calculate the means for the counts and weights across the stratas by season
  dplyr::mutate(
    # offshore wind farm averages
    owf_cpt_mean = round(mean(owf_count_per_tow), 2),
    owf_wpt_mean = round(mean(owf_weight_per_tow), 2),
    # comparing all offshore wind farm tows to the BTS tows
    strata_count_comp = round(100 * (owf_cpt_mean / bts_count_per_tow), 2),
    strata_weight_comp = round(100 * (owf_wpt_mean / bts_weight_per_tow), 2),
    # comparing individual offshore wind farms to the BTS tows
    owf_count_comp = round(100 * (owf_count_per_tow / bts_count_per_tow), 2),
    owf_weight_comp = round(100 * (owf_weight_per_tow / bts_weight_per_tow), 2)) %>%
  dplyr::select(stratum, season,
                bts_count, bts_diversity, bts_weight,
                bts_tow, bts_count_per_tow, bts_weight_per_tow,
                bts_strata_sqnm, bts_count_per_sqnm, bts_diversity_per_sqnm, bts_weight_per_sqnm,
                name, state,
                owf_count, owf_diversity, owf_weight,
                owf_tow, owf_count_per_tow, owf_weight_per_tow,
                owf_cpt_mean, owf_wpt_mean,
                owf_strata_sqnm, owf_count_per_sqnm, owf_diversity_per_sqnm, owf_weight_per_sqnm,
                strata_count_comp, strata_weight_comp,
                owf_count_comp, owf_weight_comp,
                polygon_geom, -point_geom)

spatial_comparison_csv <- as.data.frame(spatial_comparison) %>%
  dplyr::select(-owf_geom, -bts_geom)

###################################################
###################################################
###################################################

strata_comp <- spatial_comparison %>%
  dplyr::filter(strata_count_comp > 0 | strata_weight_comp > 0) %>%
  dplyr::select(stratum, season,
                strata_count_comp, strata_weight_comp,
                owf_geom)

c(max(strata_comp$strata_count_comp), min(strata_comp$strata_count_comp))
c(max(strata_comp$strata_weight_comp), min(strata_comp$strata_weight_comp))

strata_comparison_csv <- as.data.frame(strata_comp) %>%
  dplyr::select(-owf_geom)

###################################################
###################################################
###################################################

### Combined OWF data comparison
bts_breaks_weight <- c(0, 25, 50, 100, 200, 300, 400, 500)

strata_weight_comparison <- ggplot() +
  # plot all the strata
  geom_sf(data = strata, fill = "grey99", alpha = 0.8) +
  # plot the wind farm strata
  geom_sf(data = strata_comp, aes(color = strata_weight_comp), alpha = 0.6) +
  # add the US east coast state boundary data
  geom_sf(data = state, fill = "bisque1", color = "black", linetype = "84") +
  # add Atlantic offshore wind farm
  geom_sf(data = wind_farm, fill = NA, color = "#F6F627", linetype = "62") +
  # redefine the 
  scale_color_gradientn(name = "Weight (kg)\n(normalized)",
                        # Define the colors
                        colors = c("#d7191c",
                                   "#fdae61",
                                   "#2c7bb6",
                                   "#b2abd2",
                                   "#5e3c99"),
                        breaks = bts_breaks_weight) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(strata_comp)$xmin-2, xmax = st_bbox(strata_comp)$xmax)+1,
           ylim = c(ymin = st_bbox(strata_comp)$ymin-0.5, ymax = st_bbox(strata_comp)$ymax+0.5)) +
  # add theme
  theme_bw() +
  map_theme + 
  # add labels
  labs(x = "Longitude",
       y = "Latitude",
       title = "Comparing normalized weight data",
       subtitle = "Percent of normalized Offshore Wind Farm \ntows as of the Bottom Trawl Survey tows",
       caption = "These were calculated by calculating the mean weight by tow for all offshore wind \n
       farms, which was then divided by bottom trawl survey weight by tow for the target \n
       strata.")
strata_weight_comparison

###################################################

bts_breaks_count <- c(0, 25, 50, 100, 200, 300)

strata_count_comparison <- ggplot() +
  # plot all the strata
  geom_sf(data = strata, fill = "grey99", alpha = 0.8) +
  # plot the wind farm strata
  geom_sf(data = strata_comp, aes(color = strata_count_comp), alpha = 0.6) +
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
                        breaks = bts_breaks_count) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(strata_comp)$xmin-2, xmax = st_bbox(strata_comp)$xmax)+1,
           ylim = c(ymin = st_bbox(strata_comp)$ymin-0.5, ymax = st_bbox(strata_comp)$ymax+0.5)) +
  # add theme
  theme_bw() +
  map_theme +
  # add labels
  labs(x = "Longitude",
       y = "Latitude",
       title = "Comparing normalized count data",
       subtitle = "Percent of normalized Offshore Wind Farm \ntows as of the Bottom Trawl Survey tows",
       caption = "These were calculated by calculating the mean count by tow for all offshore wind \n
       farms, which was then divided by bottom trawl survey count by tow for the target \n
       strata.")
strata_count_comparison

###################################################
###################################################
###################################################

owf_comp <- spatial_comparison %>%
  dplyr::filter(owf_count_comp >0 | owf_weight_comp > 0) %>%
  dplyr::select(stratum, season,
                name, state,
                owf_count_comp, owf_weight_comp)

c(max(owf_comp$owf_count_comp), min(owf_comp$owf_count_comp))
c(max(owf_comp$owf_weight_comp), min(owf_comp$owf_weight_comp))

owf_comparison_csv <- as.data.frame(owf_comp) %>%
  dplyr::select(-owf_geom)

###################################################
###################################################
###################################################

### Individual specific OWF data comparison
owf_breaks_weight <- c(0, 50, 100, 250, 500, 750, 1000, 1250, 1500)

owf_weight_comparison <- ggplot() +
  # plot all the strata
  geom_sf(data = strata, fill = "grey99", alpha = 0.8) +
  # plot the wind farm strata
  geom_sf(data = owf_comp, aes(color = owf_weight_comp), alpha = 0.6) +
  # add the US east coast state boundary data
  geom_sf(data = state, fill = "bisque1", color = "black", linetype = "84") +
  # add Atlantic offshore wind farm
  geom_sf(data = wind_farm, fill = NA, color = "#F6F627", linetype = "62") +
  # redefine the 
  scale_color_gradientn(name = "Weight (kg)\n(normalized)",
                        # Define the colors
                        colors = c("#d7191c",
                                   "#fdae61",
                                   "#2c7bb6",
                                   "#b2abd2",
                                   "#5e3c99"),
                        breaks = owf_breaks_weight) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(owf_comp)$xmin-2, xmax = st_bbox(owf_comp)$xmax)+1,
           ylim = c(ymin = st_bbox(owf_comp)$ymin-0.5, ymax = st_bbox(owf_comp)$ymax+0.5)) +
  # add theme
  theme_bw() +
  map_theme +
  # add labels
  labs(x = "Longitude",
       y = "Latitude",
       title = "Comparing normalized weight data",
       subtitle = "Percent of normalized individual Offshore Wind Farm \ntows as of the Bottom Trawl Survey tows by strata",
       caption = "These were calculated by calculating the mean weight by tow for individual offshore \n 
                  wind farms, which was then divided by bottom trawl survey weight by tow for the \n
                  target strata.")
owf_weight_comparison

###################################################

owf_breaks_count <- c(0, 25, 50, 75, 100, 250, 500, 750, 900)

owf_count_comparison <- ggplot() +
  # plot all the strata
  geom_sf(data = strata, fill = "grey99", alpha = 0.8) +
  # plot the wind farm strata
  geom_sf(data = owf_comp, aes(color = owf_count_comp), alpha = 0.6) +
  # add the US east coast state boundary data
  geom_sf(data = state, fill = "bisque1", color = "black", linetype = "84") +
  # add Atlantic offshore wind farm
  geom_sf(data = wind_farm, fill = NA, color = "#F6F627", linetype = "62") +
  # redefine the 
  scale_color_gradientn(name = "Count\n(normalized)",
                        # Define the colors
                        colors = c("#d7191c",
                                   "#fdae61",
                                   "#2c7bb6",
                                   "#b2abd2",
                                   "#5e3c99"),
                        breaks = owf_breaks_count) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(owf_comp)$xmin-2, xmax = st_bbox(owf_comp)$xmax)+1,
           ylim = c(ymin = st_bbox(owf_comp)$ymin-0.5, ymax = st_bbox(owf_comp)$ymax+0.5)) +
  # add theme
  theme_bw() +
  map_theme +
  # add labels
  labs(x = "Longitude",
       y = "Latitude",
       title = "Comparing normalized count data",
       subtitle = "Percent of normalized individual Offshore Wind Farm \ntows as of the Bottom Trawl Survey tows by strata",
       caption = "These were calculated by calculating the mean count by tow for individual offshore \n 
                  wind farms, which was then divided by bottom trawl survey count by tow for the \n
                  target strata.")
owf_count_comparison

###################################################
###################################################
###################################################

# export data
write.csv(spatial_comparison_csv, file = paste0(csv_dir, "/", "bts_owf_spatial_comparison_data.csv"))
write.csv(strata_comparison_csv, file = paste0(csv_dir, "/", "bts_owf_strata_comparison_data.csv"))
write.csv(owf_comparison_csv, file = paste0(csv_dir, "/", "bts_owf_individual_spatial_comparison_data.csv"))

# export figures
ggsave(strata_count_comparison, filename = file.path(figure_dir, "strata_count_comparison.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")
ggsave(strata_weight_comparison, filename = file.path(figure_dir, "strata_weight_comparison.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

ggsave(owf_count_comparison, filename = file.path(figure_dir, "owf_count_comparison.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")
ggsave(owf_weight_comparison, filename = file.path(figure_dir, "owf_weight_comparison.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")
