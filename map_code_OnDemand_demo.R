## HPCC OnDemand demo
## 11/21/22
## Data from: Beth Gerstner
## Presented by: Pat Bills

#Section 1: Getting into OnDemand
#1. First, make sure you have access to the HPC. These instructions are for interfacing with the HPC easily (e.g., manipulating folders, getting file paths etc) (***Pat feel free to fill this in***)
#PC: MobaXTerm
#Mac: Terminal/Map Drive to computer

#2. Go to this website to access OnDemand and log in:
# https://cilogon.org/authorize?response_type=code&scope=openid%20profile%20email%20org.cilogon.userinfo&client_id=cilogon%3A%2Fclient_id%2F7f3abb985356946fa1f68d2de33720c4&state=drcrhIfwovJY6Y2-LS4vcNuPo9M&redirect_uri=https%3A%2F%2Fondemand.hpcc.msu.edu%2Foidc&nonce=zWT82pJsMzLQDyS4mYD94XU132-ijxZ1FC6bjAkHIy8&idphint=urn%3Amace%3Aincommon%3Amsu.edu

#3. Go to interactive jobs --> RStudio

#4.  Set parameters for the job you want to run
# In this case you do not need something super powerful. Number of hours can be 1, core can be 4, amount of memory 16gb. Once you submit you'll have to wait for your job to start. When it's ready you'll be able to launch it.

#5. Click the launch button and you're officially in RStudio.

#Download all required data from this website:
# 

# Section 2: Worked example
# Original Author: Beth E. Gerstner

#Overview: Creates a study area/occurrence map for birds within the Frugivoria database. The occurrence data was obtained from GBIF for all species within the database. The occurrence records for birds and mammals is overlaid on top of probability of cloud forest.

#Data input: GBIF dataset, cloud forest probability layer (MODCF_CloudForestPrediction.tif) from  Wilson & Jetz 2016.

#Data outputs: ggplot maps of study region and occurrence records available for bird and mammal species in the database. 


library(dplyr)
library(raster)
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
library(dismo)

pdf("my_plot.pdf")

# Read in GBIF data for species of interest
olinguito <- gbif(genus="Bassaricyon", species="neblina", args=NULL, geo=TRUE, sp=FALSE, removeZeros=FALSE, download=TRUE, ntries=5, nrecs=300, start=1, end=Inf)

olinguito <- olinguito[,c("species","lat","lon")]

# Retain only unique records. This helps prevent overloading R when mapping. There's no reason to have more than one occurrence for a species at the same location unless we are mapping richness.
olinguito_un  <-unique(olinguito[c("species","lat","lon")])


# Pull in world map
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# Subset world map. In this case we are removing the Galapagos by defining the bounding box around the Ecuador polygon.
study_region <- worldMap %>% filter(sovereignt == "Ecuador" | sovereignt == "Colombia"| sovereignt == "Panama"| sovereignt == "Venezuela" | sovereignt == "Peru" | sovereignt == "Bolivia" | sovereignt == "Brazil")


# Crop the study region to the bounding box of interest
study_region_crop <-st_crop(study_region, xmin = -80, xmax = -74, ymin = -1.7, ymax = 10)

# We included a cloud forest probability layer (MODCF_CloudForestPrediction.tif) from  Wilson & Jetz 2016. DOWNLOAD ONTO HPC AND COPY PATH HERE.
# Citation: Wilson, A.M. & Jetz, W. (2016) Remotely Sensed High-Resolution Global Cloud Dynamics for Predicting Ecosystem and Biodiversity Distributions. PLOS Biology, 14, e1002415.


cloud_forest <- raster("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_frugivores/andes_geodiv/MODCF_CloudForestPrediction.tif") 

# Crop cloud forest layer to that of the study region
cloud_forest_clip <- crop(cloud_forest, study_region_crop)

# Need the raster to be a dataframe for mapping. Convert to points to do this.
cloud_forest_p <- rasterToPoints(cloud_forest_clip, spatial = TRUE)

# Then convert to a 'conventional' dataframe
cloud_forest_df  <- data.frame(cloud_forest_p)


olinguito_plot <- ggplot() + theme_bw() + geom_raster(data=cloud_forest_df, aes(x=x, y=y, fill=MODCF_CloudForestPrediction)) + scale_fill_gradient(low="white", high="black") + geom_sf(data = study_region_crop, fill = NA) +geom_point(data = olinguito_un, aes(x = lon, y = lat), color="red", size=1, alpha=.5)

#Add scale
final_olinguito <-olinguito_plot + theme(legend.position="none", panel.background = element_rect(fill = "aliceblue"))+ scalebar(x.min = -77, x.max = -74.5, y.min =-1.3, y.max = 0,dist = 100, st.dist=.1, st.size=1.9, height=.19, transform = TRUE, dist_unit = "km", model = 'WGS84') + north(study_region_crop, location="bottomleft", scale=.07, symbol=1) + ylab("Latitude") + xlab("Longitude") 


#Create an inset map

#Load in South America
#Load in study area
#create ggplot map and can remove the zoom in part below

#Load in Latin American study region
SA_study_region <- worldMap %>% filter(region_wb == "Latin America & Caribbean")

#Full study region of dataset. This was a drawn polgyon to highlight areas containing moist montane regions.
wkt_full_study_region <-readWKT("POLYGON((-116.39878 32.14502,-116.18439 25.589,-103.35463 18.51935,-78.19944 5.42044,-82.02377 -5.17495,-75.35233 -17.47622,-70.61516 -17.66095,-73.69595 -39.73744,-76.89099 -49.35018,-71.89115 -55.00772,-64.31938 -57.53321,-68.33512 -51.38147,-63.74197 -48.20784,-65.71778 -44.50498,-56.70514 -36.93942,-57.44673 -33.15855,-57.44673 -26.14211,-62.84294 -21.53527,-57.83652 -18.17212,-61.04452 -13.37745,-65.16414 -7.68073,-72.90818 -8.70505,-70.65325 -4.52629,-66.15564 0.14301,-60.45847 3.35677,-59.58068 8.57429,-68.03483 12.85263,-76.02387 9.6308,-81.75551 9.26437,-82.18541 13.87265,-85.9107 17.83896,-87.64715 21.66655,-95.89125 19.41326,-96.31007 26.3313,-106.27585 31.97955,-116.39878 32.14502))")

#Change CRRS
crs(wkt_full_study_region) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


plot(SA_study_region[1])
plot(wkt_full_study_region)

#Change to spatial dataframe
wkt_full_study_region_sf <-st_as_sf(wkt_full_study_region)

SA_study_region_df <- fortify(SA_study_region)
wkt_full_study_region_df <- fortify(wkt_full_study_region_sf)

#check the way the inset looks
test_inset <- ggplot() + geom_sf(data = SA_study_region_df) + theme_bw() + geom_sf(data=wkt_full_study_region_df, alpha=.30, fill="blue")


# Get zoom box and outline study region used for GBIF example
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


# Add inset map to bird occurrence map
final_olinguito_inset <-ggdraw(final_olinguito) +
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
    x = 0.25, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.73,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.40, 
    height = 0.25)

final_olinguito_inset

setwd("/mnt/ufs18/rs-008/plz-lab/DATA/neotropical_frugivores/Data/")
dev.off()
