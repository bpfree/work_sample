###################################################
###################################################
###################################################


######### Part 28 #########
## Summary temporal reporting tables
###########################

# Clean environment
rm(list = ls())

# Install and load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DBI,dplyr,gganimate,gghighlight,ggplot2,gifski,odbc,plyr,raster,sf,sp,stringr)

### data directories
bts_geopackage <- "data\\geopackage\\bts_geopackage.gpkg"
bts_non_owf_geopackage <- "data\\geopackage\\bts_non_owf_geopackage.gpkg"
wind_geopackage <- "data\\geopackage\\wind_geopackage.gpkg"

figure_dir <- "figures\\charts\\temporal"
csv_dir <- "data\\f_csv_tables\\temporal_summary_tables"

### pre-determinants
begin_year <- 1963
end_year <- 2019

threshold_bts_count <- 120000
threshold_bts_weight <- 5000
threshold_non_count <- 120000
threshold_non_weight <- 5000
threshold_owf_count <- 9000
threshold_owf_weight <- 750

seasons <- c("Spring|Fall")
wind_farms_names <- c("South|North")
#stratums <- c("01010", "01620", "03030", "03380", "03440")

map_theme <- theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   axis.title.x = element_text(size = 10),
                   axis.text.x = element_text(size = 8),
                   axis.title.y = element_text(size = 10),
                   axis.text.y = element_text(size = 5),
                   plot.subtitle = element_text(size = 8),
                   plot.caption = element_text(size = 6,
                                               lineheight = 0.75),
                   legend.title = element_text(size = 8),
                   legend.text = element_text(size = 5))

###################################################
###################################################
###################################################

### load data
bts_temporal <- as.data.frame(st_read(dsn = bts_geopackage, layer = "bts_temporal"))
non_owf_temporal <- as.data.frame(st_read(dsn = bts_non_owf_geopackage, layer = "bts_non_owf_temporal"))
owf_temporal <- as.data.frame(st_read(dsn = wind_geopackage, layer = "owf_temporal"))

###################################################
###################################################
###################################################

### Catch and weight by strata and year
## BTS
stratum_year_bts <- bts_temporal %>%
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  dplyr::group_by(stratum,
                  year) %>%
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(count_summary))

## Non-OWF
stratum_year_non <- non_owf_temporal %>%
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year) %>%
  dplyr::group_by(stratum,
                  year) %>%
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(count_summary))

## OWF
stratum_year_owf <- owf_temporal %>%
  dplyr::filter(str_detect(season, seasons),
                year >= begin_year & year <= end_year,
                str_detect(name, wind_farms_names)) %>%
  dplyr::group_by(stratum,
                  year) %>%
  dplyr::summarise(count_summary = sum(count_per_tow),
                   weight_summary = sum(weight_per_tow)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(count_summary))

###################################################
###################################################
###################################################

### create figures
## BTS figures
# count graph
sy_bts_count <- ggplot() +
  geom_line(data = stratum_year_bts, aes(x = year, y = count_summary, color = stratum)) +
  geom_point(data = stratum_year_bts, aes(x = year, y = count_summary, color = stratum), size = 1) +
  # designated some field and threshold to highlight
  gghighlight(max(count_summary) > threshold_bts_count) +
  labs(x = "Year",
       y = "Count",
       color = "Strata",
       title = "Strata counts",
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts averaged all seasons
                        between ", begin_year, " and ", end_year, " in all of the northern inshore (03XXX) and offshore (01XXX) strata.
                        Highlighted strata are ones that have a summed normalized annual count above ", threshold_bts_count, " during
                        at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
  # change limits
  scale_y_continuous(breaks = seq(0, 350000, 50000)) +
  theme_bw() +
  map_theme

# count animation
sy_bts_count_animate <- gganimate::animate(ggplot() +
                                             geom_line(data = stratum_year_bts, aes(x = year, y = count_summary, color = stratum)) +
                                             #geom_point(data = stratum_year, aes(x = year, y = count_summary, color = stratum), size = 1) +
                                             # geom_segment(data = filter(stratum_year, count_summary > threshold), aes(x = year, y = count_summary, xend = 2021, yend = count_summary), 
                                             #              linetype = 2, colour = "grey") +
                                             gghighlight(max(count_summary) > threshold_bts_count) +
                                             theme(legend.position = "none") +
                                             labs(x = "Year",
                                                  y = "Count",
                                                  color = "Strata",
                                                  title = "Strata counts",
                                                  subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
                                                  caption = paste("Count values for strata were calculated by summing normalized counts averaged all seasons
                                                                   between ", begin_year, " and ", end_year, " in all of the northern inshore (03XXX) and offshore (01XXX) strata.
                                                                   Highlighted strata are ones that have a summed normalized annual count above ", threshold_bts_count, " during
                                                                   at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
                                             # change limits
                                             scale_y_continuous(breaks = seq(0, 350000, 50000)) +
                                             theme_bw() +
                                             map_theme +
                                             transition_reveal(id = year, along = year) +
                                             ease_aes('linear'),
                                           nframes = 100,
                                           fps = 2.5,
                                           start_pause = 10,
                                           end_pause = 10)

# weight graph
sy_bts_weight <- ggplot() +
  geom_line(data = stratum_year_bts, aes(x = year, y = weight_summary, color = stratum)) +
  geom_point(data = stratum_year_bts, aes(x = year, y = weight_summary, color = stratum), size = 1) +
  # designated some field and threshold to highlight
  gghighlight(max(weight_summary) > threshold_bts_weight) +
  labs(x = "Year",
       y = "Weight (kg)",
       color = "Strata",
       title = "Strata weights",
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights averaged all seasons
                        between ", begin_year, " and ", end_year, " in all of the northern inshore (03XXX) and offshore (01XXX) strata.
                        Highlighted strata are ones that have a summed normalized annual count above ", threshold_bts_count, " during
                        at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
  # change limits
  scale_y_continuous(breaks = seq(0, 15000, 2500)) +
  theme_bw() +
  map_theme

# weight animation
sy_bts_weight_animate <- gganimate::animate(ggplot() +
                                             geom_line(data = stratum_year_bts, aes(x = year, y = weight_summary, color = stratum)) +
                                             #geom_point(data = stratum_year, aes(x = year, y = weight_summary, color = stratum), size = 1) +
                                             # geom_segment(data = filter(stratum_year, weight_summary > threshold), aes(x = year, y = weight_summary, xend = 2021, yend = weight_summary), 
                                             #              linetype = 2, colour = "grey") +
                                             gghighlight(max(weight_summary) > threshold_bts_weight) +
                                             theme(legend.position = "none") +
                                             labs(x = "Year",
                                                  y = "Weight (kg)",
                                                  color = "Strata",
                                                  title = "Strata weights",
                                                  subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
                                                  caption = paste("Weight values for strata were calculated by summing normalized weights averaged all seasons
                                                                   between ", begin_year, " and ", end_year, " in all of the northern inshore (03XXX) and offshore (01XXX) strata.
                                                                   Highlighted strata are ones that have a summed normalized annual count above ", threshold_bts_count, " during
                                                                   at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
                                             # change limits
                                             scale_y_continuous(breaks = seq(0, 300000, 50000)) +
                                             theme_bw() +
                                             map_theme +
                                             transition_reveal(id = year, along = year) +
                                             ease_aes('linear'),
                                           nframes = 100,
                                           fps = 2.5,
                                           start_pause = 10,
                                           end_pause = 10)

###################################################

## Non-OWF figures
# count graph
sy_non_count <- ggplot() +
  geom_line(data = stratum_year_non, aes(x = year, y = count_summary, color = stratum)) +
  geom_point(data = stratum_year_non, aes(x = year, y = count_summary, color = stratum), size = 1) +
  # designated some field and threshold to highlight
  gghighlight(max(count_summary) > threshold_non_count) +
  labs(x = "Year",
      y = "Count",
      color = "Strata",
      title = "Strata counts outside offshore wind areas",
      subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
      caption = paste("Count values for strata were calculated by summing normalized counts averaged all seasons
                       between ", begin_year, " and ", end_year, "in all of the northern inshore (03XXX) and offshore (01XXX) strata
                       outside the offshore wind farm areas. Highlighted strata are ones that have a summed normalized
                       annual count above ", threshold_non_count, " during at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
  # change limits
  scale_y_continuous(breaks = seq(0, 300000, 50000)) +
  theme_bw() +
  map_theme

# count animation
sy_non_count_animate <- gganimate::animate(ggplot() +
  geom_line(data = stratum_year_non, aes(x = year, y = count_summary, color = stratum)) +
  #geom_point(data = stratum_year, aes(x = year, y = count_summary, color = stratum), size = 1) +
  # geom_segment(data = filter(stratum_year, count_summary > threshold), aes(x = year, y = count_summary, xend = 2021, yend = count_summary), 
  #              linetype = 2, colour = "grey") +
  gghighlight(max(count_summary) > threshold_non_count) +
  theme(legend.position = "none") +
  labs(x = "Year",
       y = "Count",
       color = "Strata",
       title = "Strata counts outside offshore wind areas",
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts averaged all seasons
                        between ", begin_year, " and ", end_year, "in all of the northern inshore (03XXX) and offshore (01XXX) strata
                        outside the offshore wind farm areas. Highlighted strata are ones that have a summed normalized
                        annual count above ", threshold_non_count, " during at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
  # change limits
  scale_y_continuous(breaks = seq(0, 300000, 50000)) +
  theme_bw() +
  map_theme +
  transition_reveal(id = year, along = year) +
  ease_aes('linear'),
  nframes = 100,
  fps = 2.5,
  start_pause = 10,
  end_pause = 10)

# weight graph
sy_non_weight <- ggplot() +
  geom_line(data = stratum_year_non, aes(x = year, y = weight_summary, color = stratum)) +
  geom_point(data = stratum_year_non, aes(x = year, y = weight_summary, color = stratum), size = 1) +
  # designated some field and threshold to highlight
  gghighlight(max(weight_summary) > threshold_non_weight) +
  labs(x = "Year",
       y = "Weight (kg)",
       color = "Strata",
       title = "Strata weights outside offshore wind areas",
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights averaged all seasons
                        between ", begin_year, " and ", end_year, "in all of the northern inshore (03XXX) and offshore (01XXX) strata
                        outside the offshore wind farm areas. Highlighted strata are ones that have a summed normalized
                        annual count above ", threshold_non_count, " during at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
  # change limits
  scale_y_continuous(breaks = seq(0, 12500, 2500)) +
  theme_bw() +
  map_theme

# weight animation
sy_non_weight_animate <- gganimate::animate(ggplot() +
                                             geom_line(data = stratum_year_non, aes(x = year, y = weight_summary, color = stratum)) +
                                             #geom_point(data = stratum_year, aes(x = year, y = weight_summary, color = stratum), size = 1) +
                                             # geom_segment(data = filter(stratum_year, weight_summary > threshold), aes(x = year, y = weight_summary, xend = 2021, yend = weight_summary), 
                                             #              linetype = 2, colour = "grey") +
                                             gghighlight(max(weight_summary) > threshold_non_weight) +
                                             theme(legend.position = "none") +
                                             labs(x = "Year",
                                                  y = "Weight (kg)",
                                                  color = "Strata",
                                                  title = "Strata weights outside offshore wind areas",
                                                  subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
                                                  caption = paste("Weight values for strata were calculated by summing normalized weights averaged all seasons
                                                                   between ", begin_year, " and ", end_year, "in all of the northern inshore (03XXX) and offshore (01XXX) strata
                                                                   outside the offshore wind farm areas. Highlighted strata are ones that have a summed normalized
                                                                   annual count above ", threshold_non_count, " during at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
                                             # change limits
                                             scale_y_continuous(breaks = seq(0, 12500, 2500)) +
                                             theme_bw() +
                                             map_theme +
                                             transition_reveal(id = year, along = year) +
                                             ease_aes('linear'),
                                           nframes = 100,
                                           fps = 2.5,
                                           start_pause = 10,
                                           end_pause = 10)

###################################################

## OWF figures
# count graph
sy_owf_count <- ggplot() +
  geom_line(data = stratum_year_owf, aes(x = year, y = count_summary, color = stratum)) +
  geom_point(data = stratum_year_owf, aes(x = year, y = count_summary, color = stratum), size = 1) +
  # designated some field and threshold to highlight
  gghighlight(max(count_summary) > threshold_owf_count) +
  labs(x = "Year",
       y = "Count",
       color = "Strata",
       title = "Strata counts inside offshore wind areas",
       subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
       caption = paste("Count values for strata were calculated by summing normalized counts averaged all seasons
                        between ", begin_year, " and ", end_year, " in all of the northern inshore (03XXX) and offshore (01XXX) strata of the offshore
                        wind farm areas. Highlighted strata are ones that have a summed normalized annual count above ", threshold_owf_count, " during
                        at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
  # change limits
  scale_y_continuous(breaks = seq(0, 12500, 2500)) +
  theme_bw() +
  map_theme

# count animation
sy_owf_count_animate <- gganimate::animate(ggplot() +
                          geom_line(data = stratum_year_owf, aes(x = year, y = count_summary, color = stratum)) +
                          #geom_point(data = stratum_year, aes(x = year, y = count_summary, color = stratum), size = 1) +
                          # geom_segment(data = filter(stratum_year, count_summary > threshold), aes(x = year, y = count_summary, xend = 2021, yend = count_summary), 
                          #              linetype = 2, colour = "grey") +
                          gghighlight(max(count_summary) > threshold_owf_count) +
                          theme(legend.position = "none") +
                          labs(x = "Year",
                               y = "Count",
                               color = "Strata",
                               title = "Strata counts inside offshore wind areas",
                               subtitle = paste("Bottom Trawl Survey counts from", begin_year, "-", end_year),
                               caption = paste("Count values for strata were calculated by summing normalized counts averaged all seasons
                                                between ", begin_year, " and ", end_year, " in all of the northern inshore (03XXX) and offshore (01XXX) strata of the offshore
                                                wind farm areas. Highlighted strata are ones that have a summed normalized annual count above ", threshold_owf_count, " during
                                                at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
                          # change limits
                          scale_y_continuous(breaks = seq(0, 12500, 2500)) +
                          theme_bw() +
                          map_theme +
                          transition_reveal(id = year, along = year) +
                          ease_aes('linear'),
                        nframes = 100,
                        fps = 2.5,
                        start_pause = 10,
                        end_pause = 10)

# weight graph
sy_owf_weight <- ggplot() +
  geom_line(data = stratum_year_owf, aes(x = year, y = weight_summary, color = stratum)) +
  geom_point(data = stratum_year_owf, aes(x = year, y = weight_summary, color = stratum), size = 1) +
  # designated some field and threshold to highlight
  gghighlight(max(weight_summary) > threshold_owf_weight) +
  labs(x = "Year",
       y = "Weight (kg)",
       color = "Strata",
       title = "Strata weights inside offshore wind areas",
       subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
       caption = paste("Weight values for strata were calculated by summing normalized weights averaged all seasons
                        between ", begin_year, " and ", end_year, " in all of the northern inshore (03XXX) and offshore (01XXX) strata of the offshore
                        wind farm areas. Highlighted strata are ones that have a summed normalized annual count above ", threshold_owf_count, " during
                        at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
  # change limits
  scale_y_continuous(breaks = seq(0, 2000, 500)) +
  theme_bw() +
  map_theme

# weight animation
sy_owf_weight_animate <- gganimate::animate(ggplot() +
                                             geom_line(data = stratum_year_owf, aes(x = year, y = weight_summary, color = stratum)) +
                                             #geom_point(data = stratum_year, aes(x = year, y = weight_summary, color = stratum), size = 1) +
                                             # geom_segment(data = filter(stratum_year, weight_summary > threshold), aes(x = year, y = weight_summary, xend = 2021, yend = weight_summary), 
                                             #              linetype = 2, colour = "grey") +
                                             gghighlight(max(weight_summary) > threshold_owf_weight) +
                                             theme(legend.position = "none") +
                                             labs(x = "Year",
                                                  y = "Weight (kg)",
                                                  color = "Strata",
                                                  title = "Strata weights inside offshore wind areas",
                                                  subtitle = paste("Bottom Trawl Survey weights from", begin_year, "-", end_year),
                                                  caption = paste("Weight values for strata were calculated by summing normalized weights averaged all seasons
                                                                   between ", begin_year, " and ", end_year, " in all of the northern inshore (03XXX) and offshore (01XXX) strata of the offshore
                                                                   wind farm areas. Highlighted strata are ones that have a summed normalized annual count above ", threshold_owf_count, " during
                                                                   at least one year between the years of interest (",begin_year, " - ", end_year,").", sep = "")) +
                                             # change limits
                                             scale_y_continuous(breaks = seq(0, 2000, 500)) +
                                             theme_bw() +
                                             map_theme +
                                             transition_reveal(id = year, along = year) +
                                             ease_aes('linear'),
                                           nframes = 100,
                                           fps = 2.5,
                                           start_pause = 10,
                                           end_pause = 10)


# sy_animate <- ggplot() +
#   geom_line(data = stratum_year, aes(x = year, y = count_summary, color = stratum)) +
#   geom_segment(data = stratum_year, aes(x = year, y = count_summary, xend = 2021, yend = count_summary), 
#                linetype = 2, colour = "grey") +
#   geom_point(data = stratum_year, aes(x = year, y = count_summary, color = stratum), size = 1) +
#   # designated some field and threshold to highlight
#   gghighlight(max(count_summary) > threshold) +
#   # geom_text(data = filter(stratum_year, count_summary > 12000), 
#   #           aes(x = 2020, y = count_summary, label = stratum), hjust = 0) +
#   labs(x = "Year",
#        y = "Count",
#        color = "Strata",
#        title = "Strata counts in offshore wind areas",
#        subtitle = "Bottom Trawl Survey counts from 1963 - 2019",
#        caption = "Count values for strata were calculated by summing normalized counts\n
#        averaged all seasons between 1963 and 2019 in all of the northern inshore (01XXX)\n
#       and offshore (03XXX) strata of the offshore wind farm areas. Highlighted strata\n
#       are ones that have a summed normalized annual count above 12000 during at least one\n
#       year between the years of interest.") +
#   # change limits
#   scale_y_continuous(breaks = seq(0, 25000, 2500)) +
#   theme_bw() +
#   map_theme +
#   transition_reveal(id = year, along = year) +
#   ease_aes('linear')
# sy_animate
# 
# g <- gganimate::animate(sy_animate,
#                    nframes = 100,
#                    fps = 2.5,
#                    start_pause = 10,
#                    end_pause = 10)

###################################################
###################################################
###################################################

### export data
## CSV
# Bottom trawl survey
write.csv(stratum_year_bts, file.path(csv_dir, paste("bts_stratum_year_",begin_year,"-",end_year,".csv", sep = "")))

# Non-offshore wind farm bottom trawl survey
write.csv(stratum_year_non, file.path(csv_dir, paste("non_stratum_year_",begin_year,"-",end_year,".csv", sep = "")))

# Offshore wind farm bottom trawl survey
write.csv(stratum_year_owf, file.path(csv_dir, paste("owf_stratum_year_",begin_year,"-",end_year,".csv", sep = "")))

## RDS

## shapefile

###################################################


### export figures
## BTS figures
# count
ggsave(sy_bts_count, filename = file.path(figure_dir, "bts_strata_counts_year.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

anim_save(sy_bts_count_animate, filename=file.path(figure_dir, "bts_strata_counts_year_animate.gif"), duration=40,
          width=4.5, height=6.5, units="in", res=600)

# weight
ggsave(sy_bts_weight, filename = file.path(figure_dir, "bts_strata_weights_year.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

anim_save(sy_bts_weight_animate, filename=file.path(figure_dir, "bts_strata_weights_year_animate.gif"), duration=40,
          width=4.5, height=6.5, units="in", res=600)

###################################################

## Non-OWF figures
# count
ggsave(sy_non_count, filename = file.path(figure_dir, "non_owf_strata_counts_year.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

anim_save(sy_non_count_animate, filename=file.path(figure_dir, "non_owf_strata_counts_year_animate.gif"), duration=40,
          width=4.5, height=6.5, units="in", res=600)

# weight
ggsave(sy_non_weight, filename = file.path(figure_dir, "non_owf_strata_weights_year.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

anim_save(sy_non_weight_animate, filename=file.path(figure_dir, "non_owf_strata_weights_year_animate.gif"), duration=40,
          width=4.5, height=6.5, units="in", res=600)

###################################################

## OWF figures
# count
ggsave(sy_owf_count, filename = file.path(figure_dir, "owf_strata_counts_year.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

anim_save(sy_owf_count_animate, filename=file.path(figure_dir, "owf_strata_counts_year_animate.gif"), duration=40,
          width=4.5, height=6.5, units="in", res=600)

# weight
ggsave(sy_owf_weight, filename = file.path(figure_dir, "owf_strata_weights_year.tiff"),
       width = 6.5, height = 4.5, units = "in", dpi = 600, compression = "lzw")

anim_save(sy_owf_weight_animate, filename=file.path(figure_dir, "owf_strata_weights_year_animate.gif"), duration=40,
          width=4.5, height=6.5, units="in", res=600)