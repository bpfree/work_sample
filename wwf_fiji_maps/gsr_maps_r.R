############################
# Great Sea Reef Report Maps
############################

# Map setup
###########################
## Defining map elements
# Colors
# administrative
land_col <- "#878787"
water_col <- "#a6cee3"

# ecological
coral_col <- "#e31a1c" # red
seagrass_col <- "#b2df8a" # light green
mangrove96_col <- "#408536"
mangrove16_col <- "#33a02c" # green
mangrove_gain <- "#cab2d6" # purple
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
scalebar_gsr <- annotation_scale(width_hint = 0.2, # percent of the plot occupied (20%)
                                  pad_x = unit(0.35, "in"), # how much padded from the x=0 position
                                  pad_y = unit(3.65, "in")) # how much padded from the y=0 position

# north arrow
narrow_gsr <- annotation_north_arrow(height = unit(0.25, "in"), 
                                      width = unit(0.2, "in"),
                                      pad_x = unit(0.07, "in"),
                                      pad_y = unit(3.5, "in"),
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
map_theme_gsr <- theme(axis.text=element_text(size=8),
                        axis.title=element_text(size=10),
                        plot.title=element_text(size=12),
                        panel.grid.major = element_line(color = "transparent"), 
                        panel.grid.minor = element_line(color = "transparent"),
                        panel.background = element_rect(fill = water_col),
                        axis.text.y = element_text(angle = 90, hjust = 0.5),
                        legend.position =  c(0.92,0.05), # alternative = bottom
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

map_theme_gsr_mangchange <- theme(axis.text=element_text(size=8),
                                  axis.title = element_text(size = 10),
                                  plot.title = element_text(size = 12),
                                  panel.grid.major = element_line(color = "transparent"),
                                  panel.grid.minor = element_line(color = "transparent"),
                                  panel.background = element_rect(fill = water_col),
                                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                                  legend.position =  c(0.92, 0.07), # alternative = bottom
                                  legend.title = element_blank(), # remove the legend title
                                  legend.text = element_text(size = 6), # text size of the descriptor
                                  legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                                  legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                                  legend.box.margin = margin(1, 1, 1, 1), # add some space between the box and the text
                                  legend.spacing.y = unit(0.025, "in"),
                                  legend.key.size = unit(0.1, "in"), # size of the color box
                                  legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                                  legend.margin = margin(0, 0.0, 0, 0, "in"), # reduce spacing between legend elements
                                  axis.line = element_line(colour = "black"))

map_theme_gsr_geomorphic <- theme(axis.text=element_text(size=8),
                                   axis.title=element_text(size=10),
                                   plot.title=element_text(size=12),
                                   panel.grid.major = element_line(color = "transparent"), 
                                   panel.grid.minor = element_line(color = "transparent"),
                                   panel.background = element_rect(fill = water_col),
                                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                                   legend.title = element_text(size = 8), # size of the title
                                   legend.position =  c(0.82,0.15), # alternative = bottom
                                   legend.text = element_text(size=6), # text size of the descriptor
                                   legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                                   legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                                   legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                                   legend.spacing.y = unit(0.025, "in"),
                                   legend.key.size = unit(0.1, "in"), # size of the color box
                                   legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                                   legend.margin = margin(0, 0, 0, 0, "in"), # reduce spacing between legend elements
                                   axis.line = element_line(colour = "black"))

map_theme_gsr_sediment <- theme(axis.text=element_text(size=8),
                                 axis.title=element_text(size=10),
                                 plot.title=element_text(size=12),
                                 panel.grid.major = element_line(color = "transparent"), 
                                 panel.grid.minor = element_line(color = "transparent"),
                                 panel.background = element_rect(fill = water_col),
                                 axis.text.y = element_text(angle = 90, hjust = 0.5),
                                 legend.title = element_text(size = 8), # size of the title
                                 legend.position =  c(0.90,0.20), # alternative = bottom
                                 legend.text = element_text(size=6), # text size of the descriptor
                                 legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                                 legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                                 legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                                 legend.spacing.y = unit(0.025, "in"),
                                 legend.key.size = unit(0.1, "in"), # size of the color box
                                 legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                                 legend.margin = margin(0, 0, 0, 0, "in"), # reduce spacing between legend elements
                                 axis.line = element_line(colour = "black"))

map_theme_gsr_bathymetry <- theme(axis.text=element_text(size=8),
                                   axis.title=element_text(size=10),
                                   plot.title=element_text(size=12),
                                   panel.grid.major = element_line(color = "transparent"), 
                                   panel.grid.minor = element_line(color = "transparent"),
                                   panel.background = element_rect(fill = water_col),
                                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                                   legend.title = element_text(size = 8), # size of the title
                                   legend.position =  c(0.90,0.20), # alternative = bottom
                                   legend.text = element_text(size=6), # text size of the descriptor
                                   legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                                   legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                                   legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                                   legend.spacing.y = unit(0.025, "in"),
                                   legend.key.size = unit(0.1, "in"), # size of the color box
                                   legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                                   legend.margin = margin(0, 0, 0, 0, "in"), # reduce spacing between legend elements
                                   axis.line = element_line(colour = "black"))

map_theme_gsr_survey <- theme(axis.text=element_text(size=8),
                               axis.title=element_text(size=10),
                               plot.title=element_text(size=12),
                               panel.grid.major = element_line(color = "transparent"), 
                               panel.grid.minor = element_line(color = "transparent"),
                               panel.background = element_rect(fill = water_col),
                               axis.text.y = element_text(angle = 90, hjust = 0.5),
                               legend.title = element_text(size = 8), # size of the title
                               legend.position =  c(0.92,0.15), # alternative = bottom
                               legend.text = element_text(size=6), # text size of the descriptor
                               legend.background = element_rect(fill = "transparent"), # make the box transparent --> "transparent"
                               legend.box.background = element_rect(fill = "white", color = "#4C84A2"), # white background with blue border
                               legend.box.margin = margin(1,1,1,1), # add some space between the box and the text
                               legend.spacing.y = unit(0.025, "in"),
                               legend.key.size = unit(0.1, "in"), # size of the color box
                               legend.key = element_rect(fill = "transparent"), # make the background of the key clear
                               legend.margin = margin(0, 0, 0, 0, "in"), # reduce spacing between legend elements
                               axis.line = element_line(colour = "black"))

# get the Great Sea Reef boundary
i <- 1

# Create the loop for the Great Sea Reef coral maps

for (i in 1){
  # get Great Sea Reef
  gsr_do <- gsr[i,]
  # get the limits
  xlim_gsr <- c(xmin = st_bbox(gsr_do)$xmin-5000, xmax = st_bbox(gsr_do)$xmax+5000)
  ylim_gsr <- c(xmin = st_bbox(gsr_do)$ymin, xmax = st_bbox(gsr_do)$ymax)
  
  xlim_vl <- c(xmin = 650000, xmax = 845000)
  ylim_vl <- c(ymin = 8120000, ymax = 8220000)
  
  # extract gsr name
  gsr_name <- "GSR"
  
  # create coral map
  # coral_map_sample <- sample_frac(coral_map, 0.1)
  
  gsr_coral <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load coral data
    geom_tile(data = coral_map, aes(x=longitude, y=latitude, fill=coral)) + 
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_gsr, 
             ylim = ylim_gsr) + 
    # legend
    scale_fill_manual(values=coral_col,
                      label = "Coral reefs") + 
    scale_color_manual(values = "grey30") + 
    # x-axis edits
    scale_x_longitude(breaks = seq(-178,180,0.5)) +
    # labels + title
    labs(x="",y="", title="") + 
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr
  
  # create seagrass plot
  # seagrass_map_sample <- sample_frac(seagrass_map, 0.1)
  
  gsr_seagrass <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load seagrass data
    geom_tile(data = seagrass_map, aes(x=longitude, y=latitude, fill=seagrass)) + 
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_gsr, 
             ylim = ylim_gsr) + 
    # legend
    scale_fill_manual(values=seagrass_col) + 
    scale_color_manual(values="grey30") + 
    # x-axis edits
    scale_x_longitude(breaks = seq(-178,180,0.5)) +
    # labels + title
    labs(x="",y="",title="") + 
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr

  # create mangrove map
  # mangrove_map_sample <- sample_frac(mangrove_map, 0.01)
  
  gsr_mangrove <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load mangrove data
    geom_tile(data = mangrove16_map, aes(x=longitude, y=latitude, fill=mangrove)) + 
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_gsr, 
             ylim = ylim_gsr) + 
    # legend
    scale_fill_manual(values=mangrove16_col) + 
    scale_color_manual(values="grey30") +
    # labels + title
    labs(x="",y="", title="") + 
    # x-axis edits
    scale_x_longitude(breaks = seq(-178,180,0.5)) +
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr
  
  # create mangrove change map
  gsr_mangrove_change <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load mangrove change data
    geom_tile(data = mangrove_gain_map, aes(x=longitude, y=latitude, fill=gain)) + 
    geom_tile(data = mangrove_loss_map, aes(x=longitude, y=latitude, fill=loss)) + 
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_gsr, 
             ylim = ylim_gsr) + 
    # legend
    scale_fill_manual(labels = c("Mangrove gain",
                                 "Mangrove loss"),
                      values=c(mangrove_gain,
                               mangrove_loss)) + 
    scale_color_manual(values="grey30") +
    # x-axis edits
    scale_x_longitude(breaks = seq(-178,180,0.5)) +
    # labels + title
    labs(x="",y="", title="") + 
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr_mangchange

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
  
  gsr_geomorphic <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
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
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = ""), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_gsr, 
             ylim = ylim_gsr) + 
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
    scale_color_manual(name = "Great Sea Reef",
                       values = "grey30") +
    # x-axis breaks
    scale_x_longitude(breaks = seq(-178,180,0.5)) + 
    # labels + title
    labs(x="",y="", title="") + 
    # map elements
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr_geomorphic

  # Create sedimentation map
  # sediment_map_sample <- sample_frac(sediment_map,0.01)
  
  gsr_sediment <- ggplot() + 
    # load bathmetry data
    geom_tile(data = sediment_map, aes(x=longitude, y=latitude, fill = relative)) +
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = ""), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_gsr, 
             ylim = ylim_gsr) + 
    # color sedimentation
    scale_fill_gradientn(name = "Turbidity \n(relative)",
                         colors = sed_col,
                         breaks = seq(0,10,2.5),
                         na.value=NA) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    scale_color_manual(name = "Great Sea Reef",
                       values = "grey30") +
    # labels + title
    labs(x="",y="", title="") + 
    # x-axis breaks
    scale_x_longitude(breaks = seq(-178,180,0.5)) + 
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr_sediment
  
  # Create sedimentation map (Vanua Levu)
  # sediment_map_sample <- sample_frac(sediment_map,0.01)
  
  gsr_sediment_vl <- ggplot() + 
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = ""), size = 0.5) +
    # load bathmetry data
    geom_tile(data = sediment_map, aes(x=longitude, y=latitude, fill = relative)) +
    # focus on the area of interest
    coord_sf(xlim = xlim_vl, 
             ylim = ylim_vl) + 
    # add ticks and longitude
    scale_x_longitude(breaks = seq(178,180,0.5)) +
    # color sedimentation
    scale_fill_gradientn(name = "Turbidity \n(relative)",
                         colors = sed_col,
                         breaks = seq(0,10,2.5),
                         na.value=NA) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    scale_color_manual(name = "Great Sea Reef",
                       values = "grey30") +
    # labels + title
    labs(x="",y="",title="") + 
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr_sediment
  
  # Create the loop for the provincial bathymetry maps
  # bath_map_sample <- sample_frac(bath_map,0.01)
  
  gsr_bath <- ggplot() + 
    # load bathmetry data
    geom_tile(data = bath_map, aes(x=longitude, y=latitude, fill = depth)) +
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = ""), size = 0.5) +
    # focus on the area of interest
    coord_sf(xlim = xlim_gsr, 
             ylim = ylim_gsr) + 
    # color bathymetry
    scale_fill_gradientn(name = "Depth (m)",
                         colors = bath_color,
                         na.value=NA) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    scale_color_manual(name = "Great Sea Reef",
                       values = "grey30") +
    # x-axis edits
    scale_x_longitude(breaks = seq(-178,180,0.5)) +
    # labels + title
    labs(x="",y="", title="") + 
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr_bathymetry

  # Create the loop for the provincial survery site maps
  # coral_map_sample <- sample_frac(coral_map,0.01)
  
  gsr_survey <- ggplot() +
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(linetype = "Great Sea Reef"), color = "grey30", size = 0.5) +
    # load coral data
    geom_tile(data = coral_map, aes(x=longitude,y=latitude, color="Coral"), fill = coral_col) +
    # load suvery site data
    geom_sf(data = surv_site, aes(fill=factor(surveyor),
                                  shape=factor(surveyor)),show.legend = "point") +
    # focus on the area of interest
    coord_sf(xlim = xlim_gsr, 
             ylim = ylim_gsr) + 
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
    scale_linetype_manual(name = "Great Sea Reef",
                          values = c("solid"),
                          guide = guide_legend(override.aes = list(color = "grey30",
                                                                   shape = NA))) + 
    # coral legend
    scale_color_manual(name = "Benthic habitat",
                       values = coral_col,
                       label = "Coral",
                       guide = guide_legend(override.aes = list(fill = coral_col,
                                                                shape = NA))) + 
    # remove fill symbology
    guides(fill = FALSE) + 
    # labels + title
    labs(x="",y="", title="") + 
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() + 
    map_theme_gsr_survey
  
  # Great Sea Reef with provinces
  gsr_provinces <- ggplot() +
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr, fill = NA, aes(color = "Great Sea Reef"), size = 0.5) +
    # load province
    geom_sf(data = provinces, fill = NA, aes(color = "Provinces"),size = 0.5) +
    # legend
    scale_color_manual(values = c("grey30",
                                  "grey50")) +
    # focus on Great Sea Reef
    coord_sf(xlim = xlim_gsr,
             ylim = ylim_gsr) +
    # x-axis edits
    scale_x_longitude(breaks = seq(-178, 180, 0.5)) +
    # labels + title
    labs(x = "", y = "", title = "") +
    # labeling provinces
    ggrepel::geom_text_repel(data = provinces,
                             mapping = aes(label = Name,
                                           geometry = geometry),
                             stat = "sf_coordinates",
                             size = 2,
                             fontface = "bold",
                             max.iter = 1500) +
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() +
    map_theme_gsr
    
  # Great Sea Reef with districts
  gsr_districts <- ggplot() +
    # load Fiji land
    geom_sf(data = fiji, fill = land_col, color = NA) +
    # load Great Sea Reef
    geom_sf(data = gsr,fill = NA,aes(color = "Great Sea Reef"),size = 0.5) +
    # load districts
    geom_sf(data = qoliqoli, fill = NA, aes(color = "Districts"),size = 0.5) +
    # legend
    scale_color_manual(values = c("grey50",
                                  "grey30")) +
    # focus on Great Sea Reef
    coord_sf(xlim = xlim_gsr,
             ylim = ylim_gsr) +
    # x-axis edits
    scale_x_longitude(breaks = seq(-178, 180, 0.5)) +
    # labels + title
    labs(x = "", y = "", title = "") +
    # labeling provinces
    ggrepel::geom_text_repel(data = qoliqoli,
                             mapping = aes(label = District,
                                           geometry = geometry),
                             stat = "sf_coordinates",
                             size = 2,
                             fontface = "bold",
                             max.iter = 1500) +
    # map elements
    scalebar_gsr +
    narrow_gsr +
    # theme
    theme_bw() +
    map_theme_gsr
  
  
  # Export plot
  out_file <- paste0(gsr_name,"_coral.tiff")
  ggsave(gsr_coral, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_seagrass.tiff")
  ggsave(gsr_seagrass, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_mangrove.tiff")
  ggsave(gsr_mangrove, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_mangrove_change.tiff")
  ggsave(gsr_mangrove_change, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_geomorphic.tiff")
  ggsave(gsr_geomorphic, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_sedimentation.tiff")
  ggsave(gsr_sediment, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_sedimentation_VanuaLevu.tiff")
  ggsave(gsr_sediment_vl, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_bathymetry.tiff")
  ggsave(gsr_bath, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_survey.tiff")
  ggsave(gsr_survey, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_provinces.tiff")
  ggsave(gsr_provinces, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  out_file <- paste0(gsr_name,"_districts.tiff")
  ggsave(gsr_districts, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600, compression = "lzw")
  
  out_file <- paste0(gsr_name,"_provinces.pdf")
  ggsave(gsr_provinces, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600)
  out_file <- paste0(gsr_name,"_provinces.eps")
  ggsave(gsr_provinces, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600)
  out_file <- paste0(gsr_name,"_districts.pdf")
  ggsave(gsr_districts, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600)
  out_file <- paste0(gsr_name,"_districts.eps")
  ggsave(gsr_districts, filename=file.path(gsr_map_dir, out_file), width=6.5,
         height=4.5, units="in", dpi=600)
}
