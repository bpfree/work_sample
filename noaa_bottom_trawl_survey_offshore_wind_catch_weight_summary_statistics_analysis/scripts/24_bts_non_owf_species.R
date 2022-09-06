###################################################
###################################################
###################################################


######### Part 24 #########
## Wind farms survey data
## Non-OWF Species
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
data_dir <- "data\\a_raw_data"
geopackage <- "data\\geopackage\\bts_non_owf_geopackage.gpkg"

export_dir <- "data\\f_csv_tables\\bts_non_owf_summary_tables"
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
bts_non_owf_survey <- st_read(dsn = geopackage, layer = "atlantic_non_owf_tows")
species_non_owf_name <- readRDS(paste(data_dir, "species.rds", sep = "/")) %>%
  dplyr::rename(species = svspp)

###################################################
###################################################
###################################################

##### Species level breakdowns
bts_non_owf_species_function <- function(bts_non_owf_survey, species){
  bts_non_owf_species_tows <- bts_non_owf_survey  %>%
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
  
  bts_sp_tows <- bts_non_owf_survey %>%
    dplyr::inner_join(bts_non_owf_species_tows,
                      by = c("cruise",
                             "stratum",
                             "tow",
                             "station",
                             "id",
                             "species",
                             "year",
                             "season")) %>%
    dplyr::relocate(species_tows, .after = id)
  
  bts_non_owf_species <- bts_sp_tows %>%
    dplyr::filter(year >= start_year,
                  year <= end_year) %>%
    dplyr::group_by(species,
                    year,
                    season) %>%
    dplyr::summarise(species_tows = sum(species_tows, na.rm = TRUE),
                     count = sum(count, na.rm = TRUE),
                     weight = sum(weight, na.rm = TRUE),
                     count_normalize = round((count / species_tows), 2),
                     weight_normalize = round((weight / species_tows), 5)) %>%
    dplyr::inner_join(species_non_owf_name,
                      by = "species") %>%
    dplyr::select(species, scientific_name, common_name,
                  year, season, species_tows,
                  count, count_normalize,
                  weight, weight_normalize)
  
  return(bts_non_owf_species)
  
  # species <- bts_survey %>%
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

bts_non_owf_species <- bts_non_owf_species_function(bts_non_owf_survey, species_non_owf_name)

bts_non_owf_species_csv <- as.data.frame(bts_non_owf_species) %>%
  dplyr::select(-geom)

###################################################
###################################################
###################################################

w <- ggplot(bts_non_owf_species, aes(x = year, y = weight_normalize)) +
  geom_point(color = as.character(bts_non_owf_species$species)) +
  ggforce::facet_wrap_paginate(~str_to_title(bts_non_owf_species$common_name),
                               # have y-axis scale dependent on the unique species
                               scales = "free_y",
                               # number of columns per page
                               ncol = 2,
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

pdf(file.path(paste0(fig_dir, "/", "bts_non_owf_species_weight.pdf")), paper = "letter", width = 8.5, height = 11)
for(i in 1:npages){
  print(w + ggforce::facet_wrap_paginate(~str_to_title(bts_non_owf_species$common_name),
                                         scales = "free_y",
                                         ncol = 2,
                                         nrow = 2,
                                         page = i))
}
dev.off()

w

###################################################

c <- ggplot(bts_non_owf_species, aes(x = year, y = count_normalize)) +
  geom_point(color = as.character(bts_non_owf_species$species)) +
  ggforce::facet_wrap_paginate(~str_to_title(bts_non_owf_species$common_name),
                               # have y-axis scale dependent on the unique species
                               scales = "free_y",
                               # number of columns per page
                               ncol = 2,
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

pdf(file.path(paste0(fig_dir, "/", "bts_non_owf_species_count.pdf")), paper = "letter", width = 8.5, height = 11)
for(i in 1:npages){
  print(c + ggforce::facet_wrap_paginate(~str_to_title(bts_non_owf_species$common_name),
                                         scales = "free_y",
                                         ncol = 2,
                                         nrow = 2,
                                         page = i))
}
dev.off()

c

###################################################
###################################################
###################################################
# export data
st_write(obj = bts_non_owf_species, dsn = geopackage, layer = "bts_non_owf_species", append = F)
saveRDS(bts_non_owf_species, file = paste0(rds_dir, "/", "bts_non_owf_species.rds"))

write.csv(bts_non_owf_species_csv, file = paste0(export_dir, "/", "bts_non_owf_species_data.csv"))
