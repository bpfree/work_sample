###################################################
###################################################
###################################################


######### Part 9 #########
## BTS Species
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

# data directories
geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
data_dir <- "data\\a_raw_data"

fig_dir <- "figures\\charts\\species"
export_dir <- "data\\f_csv_tables\\bts_summary_tables"
rds_dir <- "data\\g_rds_tables"

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

### load representative data
bts_representative <- st_read(dsn = geopackage, layer = "bts_representative_sf")
species_name <- readRDS(paste(data_dir, "species.rds", sep = "/")) %>%
  dplyr::rename(species = svspp)

###################################################
###################################################
###################################################

##### Species level breakdowns
bts_species_function <- function(bts_representative, species){
  bts_species_tows <- bts_representative  %>%
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
  
  bts_sp_tows <- bts_representative %>%
    dplyr::inner_join(bts_species_tows,
                      by = c("cruise",
                             "stratum",
                             "tow",
                             "station",
                             "id",
                             "species",
                             "year",
                             "season")) %>%
    dplyr::relocate(species_tows, .after = id)
  
  bts_species <- bts_sp_tows %>%
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
    dplyr::inner_join(species_name,
                      by = "species") %>%
    dplyr::select(species, scientific_name, common_name,
                  year, season, species_tows,
                  count, count_normalize,
                  weight, weight_normalize)
}

bts_species <- bts_species_function(bts_representative, species_name)

bts_species_csv <- as.data.frame(bts_species) %>%
  dplyr::select(-geom)

###################################################
###################################################
###################################################

w <- ggplot(bts_species, aes(x = year, y = weight_normalize)) +
  geom_point(color = as.character(bts_species$species)) +
  ggforce::facet_wrap_paginate(~str_to_title(bts_species$common_name),
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

pdf(file.path(paste0(fig_dir, "/", "bts_species_weight.pdf")), paper = "letter", width = 8.5, height = 11)
for(i in 1:npages){
  print(w + ggforce::facet_wrap_paginate(~str_to_title(bts_species$common_name),
                                         scales = "free_y",
                                         ncol = 2,
                                         nrow = 2,
                                         page = i))
}
dev.off()

w

###################################################

c <- ggplot(bts_species, aes(x = year, y = count_normalize)) +
  geom_point(color = as.character(bts_species$species)) +
  ggforce::facet_wrap_paginate(~str_to_title(bts_species$common_name),
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

pdf(file.path(paste0(fig_dir, "/", "bts_species_count.pdf")), paper = "letter", width = 8.5, height = 11)
for(i in 1:npages){
  print(c + ggforce::facet_wrap_paginate(~str_to_title(bts_species$common_name),
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
st_write(obj = bts_species, dsn = geopackage, layer = "bts_species", append = F)
saveRDS(bts_species, file = paste0(rds_dir, "/", "bts_species.rds"))

write.csv(bts_species_csv, file = paste0(export_dir, "/", "bts_species_data.csv"))
