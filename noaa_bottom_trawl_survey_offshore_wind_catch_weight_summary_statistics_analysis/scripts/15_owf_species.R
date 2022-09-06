###################################################
###################################################
###################################################


######### Part 15 #########
## Wind farms survey data
## Offshore Wind Farm
## Species
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
data_dir <- "data\\a_raw_data"
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

export_dir <- "data\\d_summary_data\\d_species"
csv_dir <- "data\\f_csv_tables\\owf_summary_tables"
rds_dir <- "data\\g_rds_tables"

fig_dir <- "figures\\charts\\species"

# data parameters
start_year <- 1963
end_year <- 2019

map_theme <- theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.title.x = element_text(size = 10),
                   axis.text.x = element_text(size = 8),
                   axis.title.y = element_text(size = 10),
                   axis.text.y = element_text(size = 8),
                   plot.subtitle = element_text(size = 8),
                   plot.caption = element_text(size = 6,
                                               lineheight = 0.5),
                   legend.title = element_text(size = 8),
                   legend.text = element_text(size = 5))

###################################################
###################################################
###################################################

### load wind farm data
wind_farm_survey <- st_read(dsn = wind_geopackage, layer = "atlantic_owf_tows")
species_name <- readRDS(paste(data_dir, "species.rds", sep = "/")) %>%
  dplyr::rename(species = svspp)

###################################################
###################################################
###################################################

##### Species level breakdowns
species_function <- function(wind_farm_survey, species_name){
  # obtain the number of tows for each species across the year and season by tow
  species_tows <- wind_farm_survey  %>%
    as.data.frame() %>%
    dplyr::group_by(cruise,
                    stratum,
                    tow,
                    station,
                    id,
                    species,
                    year,
                    season) %>%
    dplyr::summarise(species_tows = n())
  
  # join number of tows for each species back to the original wind farm catch data
  owf_sp_tows <- wind_farm_survey %>%
    dplyr::inner_join(species_tows,
                      by = c("cruise",
                             "stratum",
                             "tow",
                             "station",
                             "id",
                             "species",
                             "year",
                             "season")) %>%
    dplyr::relocate(species_tows, .after = id)
  
  # calculate species catch, weight, and normalized data based on the number of tows
  owf_species <- owf_sp_tows %>%
    # subset for years of interest
    dplyr::filter(year >= start_year,
                  year <= end_year) %>%
    # run calculations for a species unique year and season combinations
    dplyr::group_by(species,
                    year,
                    season) %>%
    # calculate the year/season tows, count, weight, normalized count, and normalized weight
    dplyr::summarise(species_tows = sum(species_tows, na.rm = TRUE),
                     count = sum(count, na.rm = TRUE),
                     weight = sum(weight, na.rm = TRUE),
                     count_normalize = round((count / species_tows), 2),
                     weight_normalize = round((weight / species_tows), 5)) %>%
    # add in scientific and common names using the species field
    dplyr::inner_join(species_name,
                      by = "species") %>%
    dplyr::select(species, scientific_name, common_name,
                  year, season, species_tows,
                  count, count_normalize,
                  weight, weight_normalize)
  
  # species <- wind_farm_survey %>%
  #   # filter by years of interest
  #   dplyr::filter(year >= start_year,
  #                 year <= end_year) %>%
  #   # group data by species names, years, and seasons
  #   dplyr::group_by(species,
  #                   year,
  #                   season) %>%
  #   # calculate the total weight for the species by year and season
  #   dplyr::summarise(count = sum(count, na.rm = TRUE),
  #                    weight = sum(weight, na.rm = TRUE),
  #                    tows = sum(tow, na.rm = TRUE),
  #                    count_normalize = round((count / tows), 2),
  #                    weight_normalize = round((weight / tows), 5)) %>%
  #   dplyr::inner_join(species_name,
  #                     by = "species") %>%
  #   dplyr::select(species, scientific_name, common_name,
  #                 year, season, tows,
  #                 count, count_normalize,
  #                 weight, weight_normalize)
}

owf_species <- species_function(wind_farm_survey, species_name)

owf_species_csv <- as.data.frame(owf_species) %>%
  dplyr::select(-geom)

###################################################
###################################################
###################################################

w <- ggplot(owf_species, aes(x = year, y = weight_normalize)) +
  geom_point(color = as.character(owf_species$species)) +
  ggforce::facet_wrap_paginate(~str_to_title(owf_species$common_name),
                               # have y-axis scale dependent on the unique species
                               scales = "free_y",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1) + 
  labs(x = "Year",
       y = "Weight (kg)",
       title = "Weight by species",
       subtitle = "Normalized by tows") +
  theme_bw() +
  map_theme

npages <- ggforce::n_pages(w)
npages

pdf(file.path(paste0(fig_dir, "/", "owf_species_weight.pdf")), paper = "letter", width = 8.5, height = 11)
for(i in 1:npages){
  print(w + ggforce::facet_wrap_paginate(~str_to_title(owf_species$common_name),
                                         scales = "free_y",
                                         ncol = 1,
                                         nrow = 2,
                                         page = i))
}
dev.off()

w

###################################################

c <- ggplot(owf_species, aes(x = year, y = count_normalize)) +
  geom_point(color = as.character(owf_species$species)) +
  ggforce::facet_wrap_paginate(~str_to_title(owf_species$common_name),
                               # have y-axis scale dependent on the unique species
                               scales = "free_y",
                               # number of columns per page
                               ncol = 1,
                               # number of rows per page
                               nrow = 2,
                               # which page to display
                               page = 1) + 
  labs(x = "Year",
       y = "Count",
       title = "Count by species",
       subtitle = "Normalized by tows") +
  theme_bw() +
  map_theme

npages <- ggforce::n_pages(c)
npages

pdf(file.path(paste0(fig_dir, "/", "owf_species_count.pdf")), paper = "letter", width = 8.5, height = 11)
for(i in 1:npages){
  print(c + ggforce::facet_wrap_paginate(~str_to_title(owf_species$common_name),
                                         scales = "free_y",
                                         ncol = 1,
                                         nrow = 2,
                                         page = i))
}
dev.off()

c

###################################################
###################################################
###################################################
# export data
st_write(obj = owf_species, dsn = wind_geopackage, layer = "owf_species", append = F)
saveRDS(owf_species, file = paste0(rds_dir, "/", "owf_species.rds"))

write.csv(owf_species_csv, file = paste0(csv_dir, "/", "owf_species_data.csv"))

