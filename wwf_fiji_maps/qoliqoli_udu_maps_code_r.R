###################################
# Udu District Report Maps
###################################

# Map setup
###########################
## Defining map elements
# Colors
# administrative
land_col <- "#878787"
water_col <- "#a6cee3"

# ecological
coral_col <- "#e31a1c" # red
seagrass_col <- "#86bb56" # light green
mangrove96_col <- "#408536"
mangrove16_col <- "#33a02c" # green
mangrove_gain <- "#a481b6" # purple
mangrove_loss <- "#fdbf6f" # orange

# geomorphical
plat_col <- "#befbff" # dark orange
rs_col <- "#92739d"   # light yellow
srs_col <- "#ffba15"  # tan
rc_col <- "#cd6812"   # deep purple
orf_col <- "#614272"  # dark purple
irf_col <- "#288471"  # mild purple
trf_col <- "#77d0fc"  # light purple
sl_col <- "#e69113"   # tile blue
unk_col <- "#B2B2B2"  # grey

# geophysical
bath_col <- bath_color <- (RColorBrewer::brewer.pal(9,"Blues"))
sed_col <- (RColorBrewer::brewer.pal(9,"YlOrRd"))

# survey sites
site_shape <- c(21,22,23,24)
site_color <- c("#ffffb3","#fb8072","#bebada","#8dd3c7")

# unique site colors
eia_col <- "#ffffb3" # light yellow
new_col <- "#fb8072" # light pink
rfc_col <- "#bebada" # light purple
wwf_col <- "#8dd3c7" # light green

# survey site shapes
eia_shape <- 21 # circle
new_shape <- 22 # square
rfc_shape <- 23 # diamond
wwf_shape <- 24 # triangle up

# scale bar
scalebar_udu <- annotation_scale(width_hint = 0.2, # percent of the plot occupied (20%)
                                    pad_x = unit(4.2, "in"), # how much padded from the x=0 position
                                    pad_y = unit(0.1, "in")) # how much padded from the y=0 position

# north arrow
narrow_udu <- annotation_north_arrow(height = unit(0.25, "in"), 
                                        width = unit(0.2, "in"),
                                        pad_x = unit(5.7, "in"),
                                        pad_y = unit(0.1, "in"),
                                        style = north_arrow_orienteering(
                                          line_width = 1,
                                          line_col = "black",
                                          fill = c("white", "black"),
                                          text_col = "black",
                                          text_family = "",
                                          text_face = NULL,
                                          text_size = 5,
                                          text_angle = 0))

# map themes
map_theme_udu <- theme(axis.text=element_text(size=8),
                          axis.title=element_text(size=10),
                          plot.title=element_text(size=12),
                          panel.grid.major = element_line(color = "transparent"), 
                          panel.grid.minor = element_line(color = "transparent"),
                          panel.background = element_rect(fill = water_col),
                          axis.text.y = element_text(angle = 90, hjust = 0.5),
                          legend.position =  c(0.92,0.20), # alternative = bottom
                          legend.title = element_blank(), # remove the legend title
                          legend.text = element_text(size=6), # text size of the descriptor
                          legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                          legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                          legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                          legend.spacing.y = unit(0.025, "in"),
                          legend.key.size = unit(0.1, "in"), # size of the color box
                          legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                          legend.margin = margin(0, 0.0, 0, 0, "in"), # reduce spacing between legend elements
                          axis.line = element_line(colour = "black"))

map_theme_udu_geomorphic <- theme(axis.text=element_text(size=8),
                                     axis.title=element_text(size=10),
                                     plot.title=element_text(size=12),
                                     panel.grid.major = element_line(color = "transparent"), 
                                     panel.grid.minor = element_line(color = "transparent"),
                                     panel.background = element_rect(fill = water_col),
                                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                                     legend.title = element_text(size = 6), # size of the title
                                     legend.position =  c(0.88,0.30), # alternative = bottom
                                     legend.text = element_text(size=4), # text size of the descriptor
                                     legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                                     legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                                     legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                                     legend.spacing.y = unit(0.025, "in"),
                                     legend.key.size = unit(0.1, "in"), # size of the color box
                                     legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                                     legend.margin = margin(0, 0, 0, 0, "in"), # reduce spacing between legend elements
                                     axis.line = element_line(colour = "black"))

map_theme_udu_sediment <- theme(axis.text=element_text(size=8),
                                   axis.title=element_text(size=10),
                                   plot.title=element_text(size=12),
                                   panel.grid.major = element_line(color = "transparent"), 
                                   panel.grid.minor = element_line(color = "transparent"),
                                   panel.background = element_rect(fill = water_col),
                                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                                   legend.title = element_text(size = 4), # size of the title
                                   legend.position =  c(0.95,0.30), # alternative = bottom
                                   legend.text = element_text(size=4), # text size of the descriptor
                                   legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                                   legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                                   legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                                   legend.spacing.y = unit(0.025, "in"),
                                   legend.key.size = unit(0.1, "in"), # size of the color box
                                   legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                                   legend.margin = margin(0, 0, 0, 0, "in"), # reduce spacing between legend elements
                                   axis.line = element_line(colour = "black"))

map_theme_udu_bathymetry <- theme(axis.text=element_text(size=8),
                                     axis.title=element_text(size=10),
                                     plot.title=element_text(size=12),
                                     panel.grid.major = element_line(color = "transparent"), 
                                     panel.grid.minor = element_line(color = "transparent"),
                                     panel.background = element_rect(fill = water_col),
                                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                                     legend.title = element_text(size = 4), # size of the title
                                     legend.position =  c(0.95,0.30), # alternative = bottom
                                     legend.text = element_text(size=4), # text size of the descriptor
                                     legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                                     legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                                     legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                                     legend.spacing.y = unit(0.025, "in"),
                                     legend.key.size = unit(0.1, "in"), # size of the color box
                                     legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                                     legend.margin = margin(0, 0, 0, 0, "in"), # reduce spacing between legend elements
                                     axis.line = element_line(colour = "black"))

map_theme_udu_survey <- theme(axis.text=element_text(size=8),
                                 axis.title=element_text(size=10),
                                 plot.title=element_text(size=12),
                                 panel.grid.major = element_line(color = "transparent"), 
                                 panel.grid.minor = element_line(color = "transparent"),
                                 panel.background = element_rect(fill = water_col),
                                 axis.text.y = element_text(angle = 90, hjust = 0.5),
                                 legend.title = element_text(size = 6), # size of the title
                                 legend.position =  c(0.94,0.30), # alternative = bottom
                                 legend.text = element_text(size=4), # text size of the descriptor
                                 legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                                 legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                                 legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                                 legend.spacing.y = unit(0.025, "in"),
                                 legend.key.size = unit(0.1, "in"), # size of the color box
                                 legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                                 legend.margin = margin(0, 0, 0, 0, "in"), # reduce spacing between legend elements
                                 axis.line = element_line(colour = "black"))

# get the Udu district
i <- 9

for (i in 9){
  # get qoliqoli
  qoliqoli_do <- qoliqoli[i,]
  # get the limits
  xlim_qoli <- c(xmin = st_bbox(qoliqoli_do)$xmin-1000, xmax = st_bbox(qoliqoli_do)$xmax+1000)
  ylim_qoli <- c(xmin = st_bbox(qoliqoli_do)$ymin, xmax = st_bbox(qoliqoli_do)$ymax)
  
  # extract qoliqoli name
  qoli_name <- qoliqoli_do$District
  
  # x-axis limits
  if(i==9){xbreaks <- seq(-1.0,180.0,0.1)} # Udu
  
  # create coral map
  qoliqoli_coral <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), color = "grey50", size = 0.5) +
    # load coral data
    geom_tile(data = coral_map, aes(x=longitude, y=latitude, fill=coral)) + 
    # load qoliqoli
    geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_qoli, 
             ylim = ylim_qoli) + 
    # legend
    scale_fill_manual(values=coral_col,
                      label = "Coral reefs") + 
    scale_color_manual(values = "grey30") +
    scale_linetype_manual(values = "3313") +
    # x-axis breaks
    scale_x_longitude(breaks = xbreaks) + 
    # labels + title
    labs(x="",y="",title="") + 
    # map elements
    scalebar_udu +
    narrow_udu +
    # theme
    theme_bw() + 
    map_theme_udu
  
  # create seagrass map
  qoliqoli_seagrass <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), color = "grey50", size = 0.5) +
    # load seagrass data
    geom_tile(data = seagrass_map, aes(x=longitude, y=latitude, fill=seagrass)) + 
    # load qoliqoli
    geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_qoli, 
             ylim = ylim_qoli) + 
    # legend
    scale_fill_manual(values=seagrass_col) + 
    scale_color_manual(values = "grey30") +
    scale_linetype_manual(values = "3313") +
    # x-axis breaks
    scale_x_longitude(breaks = xbreaks) + 
    # labels + title
    labs(x="",y="",title="") + 
    # map elements
    scalebar_udu +
    narrow_udu +
    # theme
    theme_bw() + 
    map_theme_udu
  
  # create mangrove map
  qoliqoli_mangrove <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), color = "grey50", size = 0.5) +
    # load mangrove data
    geom_tile(data = mangrove16_map, aes(x=longitude, y=latitude, fill=mangrove)) + 
    # load qoliqoli
    geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_qoli, 
             ylim = ylim_qoli) + 
    # legend
    scale_fill_manual(values=mangrove16_col) + 
    scale_color_manual(values="grey30") +
    scale_linetype_manual(values = "3313") +
    # x-axis breaks
    scale_x_longitude(breaks = xbreaks) +
    # labels + title
    labs(x="",y="",title="") +  
    # map elements
    scalebar_udu +
    narrow_udu +
    # theme
    theme_bw() + 
    map_theme_udu
  
  # create mangrove change map
  qoliqoli_mangrove_change <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), color = "grey50", size = 0.5) +
    # load mangrove change data
    geom_tile(data = mangrove_gain_map, aes(x=longitude, y=latitude, fill=gain)) + 
    geom_tile(data = mangrove_loss_map, aes(x=longitude, y=latitude, fill=loss)) + 
    # load qoliqoli
    geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_qoli, 
             ylim = ylim_qoli) + 
    # x-axis breaks
    scale_x_longitude(breaks = xbreaks) + 
    # legend
    scale_fill_manual(labels = c("Mangrove gain",
                                 "Mangrove loss"),
                      values=c(mangrove_gain,
                               mangrove_loss)) + 
    scale_color_manual(values="grey30") +
    scale_linetype_manual(values = "3313") +
    # labels + title
    labs(x="",y="",title="") + 
    # map elements
    scalebar_udu +
    narrow_udu +
    # theme
    theme_bw() + 
    map_theme_udu
  
  # create geomorphic map
  # irf_map_sample <- sample_frac(irf_map, 0.01)
  # orf_map_sample <- sample_frac(orf_map, 0.01)
  # plat_map_sample <- sample_frac(plat_map, 0.01)
  # rc_map_sample <- sample_frac(rc_map, 0.01)
  # rs_map_sample <- sample_frac(rs_map, 0.01)
  # sl_map_sample <- sample_frac(sl_map, 0.01)
  # srs_map_sample <- sample_frac(srs_map, 0.01)
  # trf_map_sample <- sample_frac(trf_map, 0.01)
  # unk_map_sample <- sample_frac(unk_map, 0.01)
  
  qoliqoli_geomorphic <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), color = "grey50", size = 0.5) +
    # load geomorphic data
    geom_tile(data = irf_map, aes(x=longitude, y=latitude, fill=irf)) + # inner reef flat
    geom_tile(data = orf_map, aes(x=longitude, y=latitude, fill=orf)) + # outer reef flat
    geom_tile(data = plat_map, aes(x=longitude, y=latitude, fill=plat)) + # plateau
    geom_tile(data = rc_map, aes(x=longitude, y=latitude, fill=rc)) + # reef crest
    geom_tile(data = rs_map, aes(x=longitude, y=latitude, fill=rs)) + # reef slope
    geom_tile(data = sl_map, aes(x=longitude, y=latitude, fill=sl)) + # shallow lagoon
    geom_tile(data = srs_map, aes(x=longitude, y=latitude, fill=srs)) + # sheltered reef slope
    geom_tile(data = trf_map, aes(x=longitude, y=latitude, fill=trf)) + # terrestrial reef flat
    geom_tile(data = unk_map, aes(x=longitude, y=latitude, fill=unk)) + # unknown
    # load qoliqoli
    geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_qoli, 
             ylim = ylim_qoli) + 
    # x-axis breaks
    scale_x_longitude(breaks = xbreaks) + 
    # legend
    scale_fill_manual(name = "Geomorphic zone",
                      guide = guide_legend(ncol = 2),
                      values=c(irf_col,
                               orf_col,
                               plat_col,
                               rc_col,
                               rs_col,
                               sl_col,
                               srs_col,
                               trf_col,
                               unk_col)) + 
    scale_color_manual(name = "District",
                       values = "grey30") +
    scale_linetype_manual(name = "Great Sea Reef",
                          label = "",
                          values = "3313") +
    # labels + title
    labs(x="",y="",title="") + 
    # map elements
    scalebar_udu +
    narrow_udu +
    # theme
    theme_bw() + 
    map_theme_udu_geomorphic
  
  # Create sedimentation map
  # sediment_map_sample <- sample_frac(sediment_map,0.01)
  
  qoliqoli_sediment <- ggplot() + 
    # load sediment data
    geom_tile(data = sediment_map, aes(x=longitude, y=latitude, fill = relative)) +
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), color = "grey50", size = 0.5) +
    # load qoliqoli
    geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_qoli, 
             ylim = ylim_qoli) + 
    # color sedimentation
    scale_fill_gradientn(name = "Turbidity \n(relative)",
                         colors = sed_col,
                         breaks = seq(0,10,2.5),
                         na.value=NA) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # district legend
    scale_color_manual(name = "District",
                       values = "grey30") +
    scale_linetype_manual(name = "Great Sea Reef",
                          label = "",
                          values = "3313") + 
    # labels + title
    labs(x="",y="",title="") + 
    # change x-axis breaks
    scale_x_longitude(breaks = xbreaks) +
    # map elements
    scalebar_udu +
    narrow_udu +
    # theme
    theme_bw() + 
    map_theme_udu_sediment
  
  # Create the loop for the provincial bathymetry maps
  # bath_map_sample <- sample_frac(bath_map,0.01)
  
  # create plot
  qoliqoli_bath <- ggplot() + 
    # load bathmetry data
    geom_tile(data = bath_map, aes(x=longitude, y=latitude, fill = depth)) +
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), color = "grey50", size = 0.5) +
    # load qoliqoli
    geom_sf(data = qoliqoli_do, fill = NA, aes(color = District), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_qoli,
             ylim = ylim_qoli) + 
    # color bathymetry
    scale_fill_gradientn(name = "Depth (m)",
                         colors = bath_color,
                         na.value=NA) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) + 
    # qoliqoli legend
    scale_color_manual(name = "District",
                       values = "grey30") +
    scale_linetype_manual(name = "Great Sea Reef",
                          label = "",
                          values = "3313") +
    # x-axis breaks
    scale_x_longitude(breaks = xbreaks) + 
    # labels + title
    labs(x="",y="",title="") + 
    # map elements
    scalebar_udu +
    narrow_udu +
    # theme
    theme_bw() + 
    map_theme_udu_bathymetry
  
  # Create the loop for the provincial survery site maps
  # coral_map_sample <- sample_frac(coral_map,0.01)
  
  qoliqoli_survey <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), size = 0.5) +
    # load coral data
    geom_tile(data = coral_map, aes(x=longitude,y=latitude, color="Coral"), fill = coral_col) +
    # load qoliqoli
    geom_sf(data = qoliqoli_do, fill = NA, aes(linetype = District), size = 0.5) +
    # load suvery site data
    geom_sf(data = surv_site, aes(fill=surveyor, shape=surveyor),show.legend = "point") +
    # focus on the area of interest
    coord_sf(xlim = xlim_qoli, 
             ylim = ylim_qoli) + 
    # x-axis breaks
    scale_x_longitude(breaks = xbreaks) +
    # survey shape
    scale_shape_manual(name = "Survey Site",
                       labels = c("Ba EIA",
                                  "New Site",
                                  "Reef Check",
                                  "WWF"),
                       values = c(eia_shape,
                                  new_shape,
                                  rfc_shape,
                                  wwf_shape),
                       guide = guide_legend(override.aes = list(fill = c(eia_col,
                                                                         new_col,
                                                                         rfc_col,
                                                                         wwf_col)))) + 
    # surveyor fill
    scale_fill_manual(labels = c("Ba EIA",
                                 "New Site",
                                 "Reef Check",
                                 "WWF"),
                      values = c(eia_col,
                                 new_col,
                                 rfc_col,
                                 wwf_col)) +
    # Great Sea Reef legend
    scale_linetype_manual(name = "Borders",
                          values = c("3313","solid"),
                          guide = guide_legend(override.aes = list(color = c("grey50","grey30"),
                                                                   shape = c(NA,NA)))) + 
    # coral legend
    scale_color_manual(name = "Benthic habitat",
                       values = coral_col,
                       label = "Coral reefs",
                       guide = guide_legend(override.aes = list(fill = coral_col,
                                                                shape = NA))) + 
    # remove fill symbology
    guides(fill = FALSE) + 
    # repel text of sites in area of interest
    ggrepel::geom_text_repel(data = filter(surv_site, district == qoli_name),
                             mapping = aes(x = longitude,
                                           y = latitude,
                                           label = site,
                                           geometry = geometry),
                             stat = "sf_coordinates",
                             size = 2,
                             fontface = "bold",
                             nudge_x = 20,
                             nudge_y = 30,
                             max.iter = 1500) +
    # labels + title
    labs(x="",y="", title="") + 
    # map elements
    scalebar_udu +
    narrow_udu +
    # theme
    theme_bw() + 
    map_theme_udu_survey
  
  
  # Export plots
  out_coral <- paste0(qoli_name,"_coral.tiff")
  ggsave(qoliqoli_coral, filename=file.path(qoliqoli_map_dir, out_coral), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_seagrass <- paste0(qoli_name,"_seagrass.tiff")
  ggsave(qoliqoli_seagrass, filename=file.path(qoliqoli_map_dir, out_seagrass), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_mangrove <- paste0(qoli_name,"_mangrove.tiff")
  ggsave(qoliqoli_mangrove, filename=file.path(qoliqoli_map_dir, out_mangrove), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_mangchange <- paste0(qoli_name,"_mangrove_change.tiff")
  ggsave(qoliqoli_mangrove_change, filename=file.path(qoliqoli_map_dir, out_mangchange), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_geomorphic <- paste0(qoli_name,"_geomorphic.tiff")
  ggsave(qoliqoli_geomorphic, filename=file.path(qoliqoli_map_dir, out_geomorphic), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_sed <- paste0(qoli_name,"_sedimentation.tiff")
  ggsave(qoliqoli_sediment, filename=file.path(qoliqoli_map_dir, out_sed), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_bath <- paste0(qoli_name,"_bathymetry.tiff")
  ggsave(qoliqoli_bath, filename=file.path(qoliqoli_map_dir, out_bath), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_survey <- paste0(qoli_name,"_survey.tiff")
  ggsave(qoliqoli_survey, filename=file.path(qoliqoli_map_dir, out_survey), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
}
