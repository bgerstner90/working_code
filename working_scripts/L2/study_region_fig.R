#Creating study area/occurrence map for birds and mammals




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
library(gridExtra)
library (ggspatial)

birds <- read.csv("D:/GBIF_data_2022/bird_2017.csv")

birds_2 <- read.csv("D:/GBIF_data_2022/bird_2017_2.csv")

#select the spatial columns 
bird_occs <- birds %>%
  dplyr::select(species, decimalLatitude, decimalLongitude)

#retain only unique records
bird_occs_unique <-unique(bird_occs[c("species","decimalLatitude","decimalLongitude")])

#pull in Ecuador occurrences - these couldn't load correctly in Excel
bird_occs_2 <- birds_2 %>%
  dplyr::select(species, decimalLatitude, decimalLongitude)

#unique occurrences in Ecuador
bird_occs_unique_2 <-unique(bird_occs_2[c("species","decimalLatitude","decimalLongitude")])


#join bird occurrences

full_bird_occ <- rbind(bird_occs_unique, bird_occs_unique_2)


#pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# country subset. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")


study_region_crop <-st_crop(study_region, xmin = -83, xmax = -70, ymin = -7, ymax = 13)
plot(study_region_crop)

cloud_forest <- raster("D:/MSU/Lab/Data/Cloud_forest_prediction_Jetz_2016/MODCF_CloudForestPrediction.tif")

cloud_forest_clip <- crop(cloud_forest, study_region_crop)

cloud_forest_p <- rasterToPoints(cloud_forest_clip, spatial = TRUE)
# Then to a 'conventional' dataframe
cloud_forest_df  <- data.frame(cloud_forest_p)


bird_occs <- ggplot() + theme_bw() + geom_raster(data=cloud_forest_df, aes(x=x, y=y, fill=MODCF_CloudForestPrediction)) + scale_fill_gradient(low="white", high="black") + geom_sf(data = study_region_crop, fill = NA) +geom_point(data = full_bird_occ, aes(x = decimalLongitude, y = decimalLatitude), color="lightcoral", size=.01, alpha=.1) + xlab("Longitude")+ ylab("Latitude")


final_bird_occs <- bird_occs + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue")) + ggspatial::annotation_scale(
  location = "br", 
  pad_x = unit(0.33, "in"), pad_y = unit(0.47,"in"),
  bar_cols = c("grey60", "white"),
  text_family = "Arial"
) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.5, "in"), pad_y = unit(0.65, "in"),
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


#add inset map
final_bird_occs_inset <-ggdraw(final_bird_occs) +
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
    x = 0.24, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.73,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.25)

final_bird_occs_inset


#Same for mammals

mam <- read.csv("/Volumes/BETH'S DRIV/GBIF_data_2022/mam_2017.csv")
mam2 <- read.csv("/Volumes/BETH'S DRIV/GBIF_data_2022/mam_2017_2.csv")

mam_full <-rbind(mam, mam2)


#select the spatial columns 
mam_occs <- mam_full %>%
  dplyr::select(species, decimalLatitude, decimalLongitude)

#retain only unique records
full_mam_occ <-unique(mam_occs[c("species","decimalLatitude","decimalLongitude")])


#merge in traits
full_mam_occ$IUCN_species_name <- full_mam_occ$species
full_mam_occ <- full_mam_occ[,-c(1)]


mam_occs <- ggplot() + theme_bw() + geom_raster(data=cloud_forest_df, aes(x=x, y=y, fill=MODCF_CloudForestPrediction)) + scale_fill_gradient(low="white", high="black") + geom_sf(data = study_region_crop, fill = NA) + geom_point(data = full_mam_occ, aes(x = decimalLongitude, y = decimalLatitude), size=.01, color="lightseagreen")
library(ggsn)

#add features
final_mam_occs <-mam_occs + theme(legend.position = "none", panel.background = element_rect(fill = "aliceblue"))+ scalebar(x.min = -84, x.max = -81.7, y.min =-3, y.max = -5,dist = 100, st.dist=.1, st.size=2.1, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + ylab("") + xlab("") 


final_mam_occs

#multipanel plot
grid.arrange(final_bird_occs_inset, final_mam_occs, nrow = 1)












