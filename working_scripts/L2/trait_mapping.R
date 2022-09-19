birds <- read.csv("/Volumes/BETH'S DRIV/GBIF_data_2022/bird_2017.csv")

birds_2 <- read.csv("/Volumes/BETH'S DRIV/GBIF_data_2022/bird_2017_2.csv")

bird_trait_data <- read.csv("/Users/bethgerstner/Desktop/database_lowland_edits/final_databases/complete_databases_2022/bird_database_final_complete_8_22.csv")

#read in bird database

library(dplyr)
library(maps)
library(ggplot2)
library(tmap)
library(rnaturalearth)
library(leaflet)
library(sf)
library(ggsn)
library(rgeos)
library(cowplot)
library(BAMMtools)

#select the spatial columns 
bird_occs <- birds %>%
  select(species, decimalLatitude, decimalLongitude)

#retain only unique records
bird_occs_unique <-unique(bird_occs[c("species","decimalLatitude","decimalLongitude")])

#pull in Ecuador occurrences - these couldn't load correctly in Excel
bird_occs_2 <- birds_2 %>%
  select(species, decimalLatitude, decimalLongitude)

#unique occurrences in Ecuador
bird_occs_unique_2 <-unique(bird_occs_2[c("species","decimalLatitude","decimalLongitude")])


#join bird occurrences

full_bird_occ <- rbind(bird_occs_unique, bird_occs_unique_2)

#merge in traits
full_bird_occ$IUCN_species_name <- full_bird_occ$species
full_bird_occ <- full_bird_occ[,-c(1)]
bird_dataset <- merge(full_bird_occ, bird_trait_data, by="IUCN_species_name")


#pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# country subset. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")


#scalebar(neighbours, location= "bottomright",dist = 200, st.dist=.1, st.size=2, height=0.1, transform = TRUE, dist_unit = "km", model = 'WGS84') +
  #north(study_region, location="topleft", scale=0.5, symbol=1) +

bird_dataset$body_mass_e <-as.numeric(bird_dataset$body_mass_e)

study_region_crop <-st_crop(study_region, xmin = -83, xmax = -70, ymin = -7, ymax = 13)


test <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = bird_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=body_mass_e), size=.1) 

#where should the breaks be so we can see color differences
getJenksBreaks(bird_dataset$body_mass_e, 10, subset = NULL)

bins <-  c(5.08,   41.00,  103.97,  181.85,  322.00,  532.00,  806.71, 1600.10, 2187.62, 4133.00)

#Chose to break at lower values to help with viewing. All values higher than the last break are then included in that range.
bird_mass <-test + scale_fill_binned(
  alpha=1,
  begin=0,
  end=1,
  limits = c(5,350), 
  breaks = c(41, 103, 181, 322,532),
  type="viridis",
  na.value = "grey50",
  direction = -1,
  guide = guide_colorsteps(draw.ulim = F, draw.llim = F, even.steps = TRUE), aesthetics = "colour", guide_legend("Body mass (g)")) 


?scale_color_stepsn

#bird_mass_test <-test + scale_color_stepsn(colours = c("purple4","lightblue", "blue", "darkblue", "lightgreen", "darkgreen","red","green","yellow"), breaks = c(4, 41, 106, 200, 314, 485, 807, 1600, 2872, 4133), na.value = "grey50", guide_legend("Body mass (g)"))


final_bird_mass_1 <-bird_mass + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank()) +  ylab("Latitude") + xlab("Longitude") 

library (ggspatial)
final_bird_mass <- final_bird_mass_1 + ggspatial::annotation_scale(
  location = "bl", 
  pad_x = unit(.06, "in"), pad_y = unit(.15,"in"),
  bar_cols = c("grey60", "white"),
  text_family = "Arial"
) +
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.1, "in"), pad_y = unit(2, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "Arial"
    )
  )

#Inset map

#Load in SA
#Load in study area
#create ggplot map and can remove the zoom in part below

SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


wkt_full_study_region <-readWKT("POLYGON((-116.39878 32.14502,-116.18439 25.589,-103.35463 18.51935,-78.19944 5.42044,-82.02377 -5.17495,-75.35233 -17.47622,-70.61516 -17.66095,-73.69595 -39.73744,-76.89099 -49.35018,-71.89115 -55.00772,-64.31938 -57.53321,-68.33512 -51.38147,-63.74197 -48.20784,-65.71778 -44.50498,-56.70514 -36.93942,-57.44673 -33.15855,-57.44673 -26.14211,-62.84294 -21.53527,-57.83652 -18.17212,-61.04452 -13.37745,-65.16414 -7.68073,-72.90818 -8.70505,-70.65325 -4.52629,-66.15564 0.14301,-60.45847 3.35677,-59.58068 8.57429,-68.03483 12.85263,-76.02387 9.6308,-81.75551 9.26437,-82.18541 13.87265,-85.9107 17.83896,-87.64715 21.66655,-95.89125 19.41326,-96.31007 26.3313,-106.27585 31.97955,-116.39878 32.14502))")

crs(wkt_full_study_region) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


plot(SA_study_region[1])
plot(wkt_full_study_region)

wkt_full_study_region_sf <-st_as_sf(wkt_full_study_region)

SA_study_region_df <- fortify(SA_study_region)
wkt_full_study_region_df <- fortify(wkt_full_study_region_sf)

test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw() + geom_sf(data=wkt_full_study_region_df, alpha=.30, fill="blue")

#+ geom_polygon(data = wkt_full_study_region, aes(x = decimalLongitude, y = decimalLatitude), size=.0001)


#get zoom box and outline
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -83, 
    xmax = -70, 
    ymin = -7, 
    ymax = 13),
    fill = NA, 
    colour = "red",
    size = 0.6,
    alpha=.8
  )


#density plot
# Use semi-transparent fill

library(scales)
m_birds <-ggplot(bird_trait_data, aes(x=body_mass_e), alpha=.4) + geom_density(fill="lightcoral", alpha=.7) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) + xlab("Body mass (g)") +  geom_vline(aes(xintercept = median(body_mass_e, na.rm = T)), colour = "red", linetype ="longdash", size = .8)


#calculate mean and median to put on graph
summary(bird_trait_data$body_mass_e)

#add inset map
final_bird_mass_inset <-ggdraw(final_bird_mass) +
  draw_plot(
    {
      inset_map_box +
        theme(legend.position = "none", axis.title.x = element_blank(),
              axis.title.y = element_blank(), axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks = element_blank(),
              plot.background = element_blank(),
              panel.grid.major = element_blank())
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.22, #was .11
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.73,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.25)

final_bird_mass_inset

#add inset density plot

final_bird_mass_inset_density_f <-ggdraw(final_bird_mass_inset) +
  draw_plot(
    {
      m_birds +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.3))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.515, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.11,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.14, 
    height = 0.22)



#Same for mammals

mam <- read.csv("/Volumes/BETH'S DRIV/GBIF_data_2022/mam_2017.csv")
mam2 <- read.csv("/Volumes/BETH'S DRIV/GBIF_data_2022/mam_2017_2.csv")

mam_full <-rbind(mam, mam2)
mam_trait_data <- read.csv("/Users/bethgerstner/Desktop/database_lowland_edits/final_databases/complete_databases_2022/mammal_database_final_complete_8_23.csv")


#select the spatial columns 
mam_occs <- mam_full %>%
  dplyr::select(species, decimalLatitude, decimalLongitude)

#retain only unique records
full_mam_occ <-unique(mam_occs[c("species","decimalLatitude","decimalLongitude")])


#merge in traits
full_mam_occ$IUCN_species_name <- full_mam_occ$species
full_mam_occ <- full_mam_occ[,-c(1)]
mam_dataset <- merge(full_mam_occ, mam_trait_data, by="IUCN_species_name")

mam_dataset$body_mass_e <-as.numeric(mam_dataset$body_mass_e)


mam_test_new <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = mam_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=body_mass_e), size=.01)

#where should the breaks be so we can see color differences
mam_jenks <-mam_dataset$body_mass_e
getJenksBreaks(mam_jenks, 10, subset = NULL)

#bins_mam <- c( 5.60,   1537.52,  5000.00, 7274.95,  9599.97, 16633.17,  21267.32,  32233.69,  48144.91, 140000.63)

mam_mass <-mam_test_new + scale_fill_binned(
  alpha=1,
  begin=.1,
  end=1,
  limits = c(0,7500), 
  breaks = c(5.6,   1537.52,   3250,   5000,   7274.95),
  type="viridis",
  na.value = "grey50",
  direction=-1,
  guide = guide_colorsteps(draw.ulim = F, draw.llim = T, even.steps = T), aesthetics = "colour", guide_legend("Body mass (g)"))


final_mam_mass <-mam_mass + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank()) + ylab("") + xlab("") 
library(MASS)
library(scales)

#mammal density 
m_mam <-ggplot(mam_trait_data, aes(x=body_mass_e), alpha=.4) + geom_density(fill="lightseagreen", alpha=.7) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) + xlab("Body mass (g)") +  geom_vline(aes(xintercept = median(body_mass_e, na.rm = T)),  colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(mam_trait_data$body_mass_e)

final_mam_mass_inset_density <-ggdraw(final_mam_mass) +
  draw_plot(
    {
      m_mam +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.3))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.515, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.11,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.14, 
    height = 0.22)


final_mam_mass_inset_density

#Inset map

#Load in SA
#Load in study area
#create ggplot map and can remove the zoom in part below

SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")


wkt_full_study_region <-readWKT("POLYGON((-116.39878 32.14502,-116.18439 25.589,-103.35463 18.51935,-78.19944 5.42044,-82.02377 -5.17495,-75.35233 -17.47622,-70.61516 -17.66095,-73.69595 -39.73744,-76.89099 -49.35018,-71.89115 -55.00772,-64.31938 -57.53321,-68.33512 -51.38147,-63.74197 -48.20784,-65.71778 -44.50498,-56.70514 -36.93942,-57.44673 -33.15855,-57.44673 -26.14211,-62.84294 -21.53527,-57.83652 -18.17212,-61.04452 -13.37745,-65.16414 -7.68073,-72.90818 -8.70505,-70.65325 -4.52629,-66.15564 0.14301,-60.45847 3.35677,-59.58068 8.57429,-68.03483 12.85263,-76.02387 9.6308,-81.75551 9.26437,-82.18541 13.87265,-85.9107 17.83896,-87.64715 21.66655,-95.89125 19.41326,-96.31007 26.3313,-106.27585 31.97955,-116.39878 32.14502))")

crs(wkt_full_study_region) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


plot(SA_study_region[1])
plot(wkt_full_study_region)

wkt_full_study_region_sf <-st_as_sf(wkt_full_study_region)

SA_study_region_df <- fortify(SA_study_region)
wkt_full_study_region_df <- fortify(wkt_full_study_region_sf)

test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw() + geom_sf(data=wkt_full_study_region_df, alpha=.30, fill="blue")

#+ geom_polygon(data = wkt_full_study_region, aes(x = decimalLongitude, y = decimalLatitude), size=.0001)


#get zoom box and outline
inset_map_box <- 
  test_inset +
  geom_rect(aes(
    xmin = -83, 
    xmax = -70, 
    ymin = -7, 
    ymax = 13),
    fill = NA, 
    colour = "red",
    size = 0.6,
    alpha=.8
  )

# another trait for birds
bird_dataset$generation_time
bird_dataset$generation_time <-as.numeric(bird_dataset$generation_time)

#create plot of trait
bird_gen_time <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = bird_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=generation_time), size=.1) 

#where should the breaks be so we can see color differences
getJenksBreaks(bird_dataset$generation_time, 5, subset = NULL)

#bins <- c( 1.93,  4.10,  5.80,  8.80, 15.16)

bird_gen <-bird_gen_time + scale_fill_binned(
  low= "mistyrose1",
  high="magenta4",
  limits = c(0,14), 
  breaks = c(1.9, 4.1,  5.8, 8.8, 15.2),
  na.value = "grey50",
  guide = guide_colorsteps(draw.ulim = T, draw.llim = T, even.steps = TRUE), aesthetics = "colour", guide_legend("Generation time (y)")) 


?scale_color_stepsn

#bird_mass_test <-test + scale_color_stepsn(colours = c("purple4","lightblue", "blue", "darkblue", "lightgreen", "darkgreen","red","green","yellow"), breaks = c(4, 41, 106, 200, 314, 485, 807, 1600, 2872, 4133), na.value = "grey50", guide_legend("Body mass (g)"))


final_bird_gen <-bird_gen + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank()) + ylab("Latitude") + xlab("Longitude") 


#density plot
# Use semi-transparent fill


m_birds_gen <-ggplot(bird_trait_data, aes(x=generation_time), alpha=.4) + geom_density(fill="lightcoral", alpha=.7) + xlab("Generation time (y)") +  geom_vline(aes(xintercept = median(generation_time, na.rm = T)), colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(bird_trait_data$generation_time)

#add inset density plot

final_bird_gen_inset <-ggdraw(final_bird_gen) +
  draw_plot(
    {
      m_birds_gen +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.3))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.515, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.11,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.14, 
    height = 0.22)


#mammal gentime

mam_dataset$generation_time <-as.numeric(mam_dataset$generation_time)


mam_gen <- ggplot() + geom_sf(data = study_region_crop) + theme_bw() + geom_point(data = mam_dataset, aes(x = decimalLongitude, y = decimalLatitude, color=generation_time), size=.01)

#where should the breaks be so we can see color differences
mam_jenks_gen <-mam_dataset$generation_time
getJenksBreaks(mam_jenks_gen, 5, subset = NULL)

#bins_mam <- c(1,  3.8,  6.80,  11.4, 16.00)

mam_gen_final <-mam_gen + scale_fill_binned(
  low= "mistyrose1",
  high="magenta4",
  limits = c(0,14), 
  breaks = c(1.0,  3.8,  6.8,  11.4, 16.0),
  na.value = "grey50",
  guide = guide_colorsteps(draw.ulim = F, draw.llim = T, even.steps = T), aesthetics = "colour", guide_legend("Generation time (y)"))


final_mam_gen <-mam_gen_final + theme(legend.title = element_text(size=12, color = "black", face="bold"), legend.justification=c(0,1),legend.position=c(0.05, 0.7), legend.background = element_blank(), legend.key = element_blank())+ ylab("") + xlab("") 

library(MASS)
library(scales)

mam_trait_data$generation_time <- as.numeric(mam_trait_data$generation_time)

#mammal density 
m_mam_gen <-ggplot(mam_trait_data, aes(x=generation_time), alpha=.4) + geom_density(fill="lightseagreen", alpha=.7) + xlab("Generation time (y)") +  geom_vline(aes(xintercept = median(generation_time, na.rm = T)), colour = "red", linetype ="longdash", size = .8)

#calculate mean and median to put on graph
summary(mam_trait_data$generation_time)

final_mam_gen_inset_density <-ggdraw(final_mam_gen) +
  draw_plot(
    {
      m_mam_gen +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.3))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.515, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.11,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.14, 
    height = 0.22)


final_bird_gen_inset <-ggdraw(final_bird_gen) +
  draw_plot(
    {
      m_birds_gen +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_blank(),
              panel.border = element_rect(colour = "black", fill=NA, size=.5))
    },
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.58, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.12,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.28)











final_mam_gen_inset_density

# Same for mammals

#multipanel
mass_plot <-plot_grid(final_bird_mass_inset_density_f, NULL, final_mam_mass_inset_density, rel_widths = c(1, -0.1, 1), align = "hv", nrow = 1)

gen_time_plot <- plot_grid(final_bird_gen_inset, NULL, final_mam_gen_inset_density, rel_widths = c(1, -0.1, 1), align = "hv",  nrow = 1)

full_trait_plot <- plot_grid(mass_plot, NULL, gen_time_plot, rel_widths = c(1, -0.1, 1), align="hv", nrow=2)
















#how to zoom in
#coord_sf(xlim = c(-83, -70), ylim = c(-7, 13), expand = FALSE)
test+ scale_colour_discrete(breaks = 1:5) + theme(legend.position = c("left"))
                          
                          
factor <- factor(bird_dataset$body_mass_value_g_e)
getJenksBreaks(factor, 10, subset = NULL)
b <- breaks(factor, 1:5)
 


