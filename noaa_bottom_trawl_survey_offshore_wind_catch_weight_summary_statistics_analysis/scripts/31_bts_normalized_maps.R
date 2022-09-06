###################################################
###################################################
###################################################


######### Part 31 #########
## Mapping BTS
## Spatial, Temporal, Species
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
bts_geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
bts_non_owf_geopackage <- "data\\geopackage\\bts_non_owf_geopackage.gpkg"
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"
state_dir <- "data\\c_spatial_data\\us_states"

figure_dir <- "figures\\maps"
csv_dir <- "data\\f_csv_tables\\normalized_summary_tables"

map_theme <- theme(panel.grid.major = element_blank(),
                   axis.title.x = element_text(size = 10),
                   axis.title.y = element_text(size = 10),
                   plot.subtitle = element_text(size = 10),
                   plot.caption = element_text(size = 6,
                                               lineheight = 0.8),
                   legend.title = element_text(size = 10,
                                               hjust = 0.5),
                   legend.text = element_text(size = 8),
                   axis.text.y = element_text(angle = 90,
                                              hjust = 0.5))

begin_year <- 1963
end_year <- 2019

###################################################
###################################################
###################################################

# load data
strata <- st_read(dsn = bts_geopackage, layer = "strata")
state <- st_read(dsn = state_dir, layer = "us_states_east_coast")
wind_farm <- st_read(dsn = wind_geopackage, layer = "owf_areas")

###################################################

# load BTS data
bts_spatial <- st_read(dsn = bts_geopackage, layer = "bts_spatial")
bts_temporal <- st_read(dsn = bts_geopackage, layer = "bts_temporal")
bts_species <- st_read(dsn = bts_geopackage, layer = "bts_species")

###################################################
###################################################
###################################################

### calculate normalized data
## by stratum
bts_spatial_strata <- bts_spatial %>%
  group_by(stratum) %>%
  # calculate total count of caught species per stratum
  dplyr::summarise(count = sum(count, na.rm = TRUE),
                   weight = sum(weight, na.rm = TRUE),
                   tows = sum(tows, na.rm = TRUE)) %>%
  # create a new field that divides (normalizes) the species weight by number of tows and area
  dplyr::mutate(count_per_tow = count / tows,
                weight_per_tow = weight / tows) %>%
  dplyr::select(stratum,
                count, weight,
                tows, count_per_tow, weight_per_tow)

bts_spatial_strata_csv <- bts_spatial_strata %>%
  as.data.frame() %>%
  dplyr::select(-geom)

###################################################

## by year
bts_temporal_year <- bts_temporal %>%
  group_by(stratum,
           year) %>%
  # calculate total count of caught species per stratum
  dplyr::summarise(count = sum(count, na.rm = TRUE),
                   weight = sum(weight, na.rm = TRUE),
                   tows = sum(tows, na.rm = TRUE)) %>%
  # create a new field that divides (normalizes) the species weight by number of tows and area
  dplyr::mutate(count_per_tow = count / tows,
                weight_per_tow = weight / tows) %>%
  dplyr::select(stratum, year,
                count, weight,
                tows, count_per_tow, weight_per_tow)

bts_temporal_year_csv <- bts_temporal_year %>%
  as.data.frame() %>%
  dplyr::select(-geom)

###################################################

## by species


###################################################
###################################################
###################################################

### BTS spatial map
## spatial
# count
bts_spatial_count_map <- ggplot() +
  geom_sf(data = strata, fill = "grey80", alpha = 0.6) + 
  geom_sf(data = state, fill = "grey96", color = "grey87", lwd = 0.3) +
  geom_sf(data = bts_spatial_strata, aes(color = count_per_tow), size = 0.5, alpha = 0.4) +
  geom_sf(data = wind_farm, fill = "NA", color = "red", size = 0.5) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(bts_spatial)$xmin-1, xmax = st_bbox(bts_spatial)$xmax)+1,
           ylim = c(ymin = st_bbox(bts_spatial)$ymin-0.25, ymax = st_bbox(bts_spatial)$ymax+0.25)) +
  # labels
  labs(x = "Longitude",
       y = "Latitude",
       color = "Count\n(normalized\nby tows)",
       title = "Strata normalized count",
       subtitle = paste("Bottom Trawl Survey count data (", begin_year, " - ", end_year, ") normalized by number of tows", sep = ""),
       caption = paste("Count values for strata were calculated by normalizing count for each strata between", begin_year, "and", end_year, "in
                       all of the northern inshore (03XXX) and offshore (01XXX) strata. Normalization was calculated by dividing the aggregated
                       count within a strata and averaging it by the total number of tows completed in the strata between", begin_year, "and", end_year)) +
  guides(color = guide_colourbar(ticks.colour = "black",
                                 frame.colour = "black",
                                 barwidth = 1,
                                 barheight = 20)) +
  # add theme
  theme_bw() +
  map_theme

# weight
bts_spatial_weight_map <- ggplot() +
  geom_sf(data = strata, fill = "grey80", alpha = 0.6) + 
  geom_sf(data = state, fill = "grey96", color = "grey87", lwd = 0.3) +
  geom_sf(data = bts_spatial_strata, aes(color = weight_per_tow), size = 0.5, alpha = 0.4) +
  geom_sf(data = wind_farm, fill = "NA", color = "red", size = 0.5) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(bts_spatial)$xmin-1, xmax = st_bbox(bts_spatial)$xmax)+1,
           ylim = c(ymin = st_bbox(bts_spatial)$ymin-0.25, ymax = st_bbox(bts_spatial)$ymax+0.25)) +
  # labels
  labs(x = "Longitude",
       y = "Latitude",
       color = "Weight (kg)\n(normalized\nby tows)",
       title = "Strata normalized weight",
       subtitle = paste("Bottom Trawl Survey weight data (", begin_year, " - ", end_year, ") normalized by number of tows", sep = ""),
       caption = paste("Weight values for strata were calculated by normalizing weight for each strata between", begin_year, "and", end_year, "in
                       all of the northern inshore (03XXX) and offshore (01XXX) strata. Normalization was calculated by dividing the aggregated
                       weight within a strata and averaging it by the total number of tows completed in the strata between", begin_year, "and", end_year)) +
  guides(color = guide_colourbar(ticks.colour = "black",
                                 frame.colour = "black",
                                 barwidth = 1,
                                 barheight = 20)) +
  # add theme
  theme_bw() +
  map_theme

###################################################

## temporal
# count map
bts_temporal_count_map <- ggplot() +
  geom_sf(data = strata, fill = "grey80", alpha = 0.6) +
  geom_sf(data = state, fill = "grey96", color = "grey87", lwd = 0.3) +
  geom_sf(data = bts_temporal_year, aes(color = count_per_tow), size = 0.5, alpha = 0.4) +
  geom_sf(data = wind_farm, fill = "NA", color = "red", size = 0.5) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(bts_temporal_year)$xmin-1, xmax = st_bbox(bts_temporal_year)$xmax)+1,
           ylim = c(ymin = st_bbox(bts_temporal_year)$ymin-0.25, ymax = st_bbox(bts_temporal_year)$ymax+0.25)) +
  # labels
  labs(x = "Longitude",
       y = "Latitude",
       color = "Count\n(normalized\nby tows)",
       title = "Normalized count data",
       subtitle = paste("Bottom Trawl Survey count data (", begin_year, " - ", end_year, ") normalized by number of tows", sep = ""),
       caption = paste("Count values for strata were calculated by normalizing count for each strata between", begin_year, "and", end_year, "in
                       all of the northern inshore (03XXX) and offshore (01XXX) strata. Normalization was calculated by dividing the aggregated
                       count within a strata and averaging it by the total number of tows completed in the strata between", begin_year, "and", end_year)) +
  guides(color = guide_colourbar(ticks.colour = "black",
                                 frame.colour = "black",
                                 barwidth = 1,
                                 barheight = 20)) +
  # add theme
  theme_bw() +
  map_theme

# count animation
bts_temporal_count_map_animation <- gganimate::animate(ggplot() +
                                                         geom_sf(data = strata, fill = "grey80", alpha = 0.6) + geom_sf(data = state, fill = "grey96", color = "grey87", lwd = 0.3) +
                                                         geom_sf(data = wind_farm, fill = "NA", color = "red", size = 0.5) +
                                                         geom_sf(data = bts_temporal_year, aes(color = count_per_tow), size = 0.5, alpha = 0.4) +
                                                         # focus on the stratum
                                                         coord_sf(xlim = c(xmin = st_bbox(bts_temporal_year)$xmin-1, xmax = st_bbox(bts_temporal_year)$xmax)+1,
                                                                  ylim = c(ymin = st_bbox(bts_temporal_year)$ymin-0.25, ymax = st_bbox(bts_temporal_year)$ymax+0.25)) +
                                                         # labels
                                                         labs(x = "Longitude",
                                                              y = "Latitude",
                                                              color = "Count\n(normalized\nby tows)",
                                                              title = "Normalized count data",
                                                              subtitle = "Year: {current_frame}",
                                                              caption = paste("Count values for strata were calculated by normalizing count for each strata between", begin_year, "and", end_year, "in
                                                                       all of the northern inshore (03XXX) and offshore (01XXX) strata. Normalization was calculated by dividing the aggregated
                                                                       count within a strata and averaging it by the total number of tows completed in the strata between", begin_year, "and", end_year)) +
                                                         guides(color = guide_colourbar(ticks.colour = "black",
                                                                                        frame.colour = "black",
                                                                                        barwidth = 1,
                                                                                        barheight = 20)) +
                                                         # add theme
                                                         theme_bw() +
                                                         map_theme +
                                                         transition_manual(year) +
                                                         ease_aes('linear'),
                                                       nframes = max(bts_temporal_year$year) - min(bts_temporal_year$year),
                                                       fps = 2.5,
                                                       start_pause = 2,
                                                       end_pause = 2)

# weight map
bts_temporal_weight_map <- ggplot() +
  geom_sf(data = strata, fill = "grey80", alpha = 0.6) +
  geom_sf(data = state, fill = "grey96", color = "grey87", lwd = 0.3) +
  geom_sf(data = bts_temporal_year, aes(color = weight_per_tow), size = 0.5, alpha = 0.4) +
  geom_sf(data = wind_farm, fill = "NA", color = "red", size = 0.5) +
  # focus on the stratum
  coord_sf(xlim = c(xmin = st_bbox(bts_temporal_year)$xmin-1, xmax = st_bbox(bts_temporal_year)$xmax)+1,
           ylim = c(ymin = st_bbox(bts_temporal_year)$ymin-0.25, ymax = st_bbox(bts_temporal_year)$ymax+0.25)) +
  # labels
  labs(x = "Longitude",
       y = "Latitude",
       color = "Weight (kg)\n(normalized\nby tows)",
       title = "Normalized weight data",
       subtitle = paste("Bottom Trawl Survey weight data (", begin_year, " - ", end_year, ") normalized by number of tows", sep = ""),
       caption = paste("Weight values for strata were calculated by normalizing weight for each strata between", begin_year, "and", end_year, "in
                       all of the northern inshore (03XXX) and offshore (01XXX) strata. Normalization was calculated by dividing the aggregated
                       weight within a strata and averaging it by the total number of tows completed in the strata between", begin_year, "and", end_year)) +
  guides(color = guide_colourbar(ticks.colour = "black",
                                 frame.colour = "black",
                                 barwidth = 1,
                                 barheight = 20)) +
  # add theme
  theme_bw() +
  map_theme

# weight animation
bts_temporal_weight_map_animation <- gganimate::animate(ggplot() +
                                                         geom_sf(data = strata, fill = "grey80", alpha = 0.6) + geom_sf(data = state, fill = "grey96", color = "grey87", lwd = 0.3) +
                                                         geom_sf(data = wind_farm, fill = "NA", color = "red", size = 0.5) +
                                                         geom_sf(data = bts_temporal_year, aes(color = weight_per_tow), size = 0.5, alpha = 0.4) +
                                                         # focus on the stratum
                                                         coord_sf(xlim = c(xmin = st_bbox(bts_temporal_year)$xmin-1, xmax = st_bbox(bts_temporal_year)$xmax)+1,
                                                                  ylim = c(ymin = st_bbox(bts_temporal_year)$ymin-0.25, ymax = st_bbox(bts_temporal_year)$ymax+0.25)) +
                                                         # labels
                                                         labs(x = "Longitude",
                                                              y = "Latitude",
                                                              color = "Weight (kg)\n(normalized\nby tows)",
                                                              title = "Normalized weight data",
                                                              subtitle = "Year: {current_frame}",
                                                              caption = paste("Weight values for strata were calculated by normalizing weight for each strata between", begin_year, "and", end_year, "in
                                                                       all of the northern inshore (03XXX) and offshore (01XXX) strata. Normalization was calculated by dividing the aggregated
                                                                       weight within a strata and averaging it by the total number of tows completed in the strata between", begin_year, "and", end_year)) +
                                                         guides(color = guide_colourbar(ticks.colour = "black",
                                                                                        frame.colour = "black",
                                                                                        barwidth = 1,
                                                                                        barheight = 20)) +
                                                         # add theme
                                                         theme_bw() +
                                                         map_theme +
                                                         transition_manual(year) +
                                                         ease_aes('linear'),
                                                       nframes = max(bts_temporal_year$year) - min(bts_temporal_year$year),
                                                       fps = 2.5,
                                                       start_pause = 2,
                                                       end_pause = 2)


###################################################
###################################################
###################################################

### export data
## CSV
write.csv(bts_spatial_strata_csv, file.path(csv_dir, "bts_spatial_strata.csv"))
write.csv(bts_temporal_year_csv, file.path(csv_dir, "bts_temporal_year.csv"))

## RDS

## shapefile

###################################################
### export figures
## spatial
# count
ggsave(bts_spatial_count_map, filename = file.path(figure_dir, "bts_spatial_count_map.tiff"),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(bts_spatial_weight_map, filename = file.path(figure_dir, "bts_spatial_weight_map.tiff"),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

## temporal
# count
ggsave(bts_temporal_count_map, filename = file.path(figure_dir, "bts_temporal_count_map.tiff"),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

# weight
ggsave(bts_temporal_weight_map, filename = file.path(figure_dir, "bts_temporal_weight_map.tiff"),
       width = 12, height = 8, units = "in", dpi = 600, compression = "lzw")

### export animations
## temporal
# count
anim_save(bts_temporal_count_map_animation, filename=file.path(figure_dir, "bts_temporal_count_map_animation.gif"), duration=60,
          width=12, height=10, units="in", res=600)

# weight
anim_save(bts_temporal_weight_map_animation, filename=file.path(figure_dir, "bts_temporal_weight_map_animation.gif"), duration=60,
          width=12, height=10, units="in", res=600)