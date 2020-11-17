
################################################################################################################
# US Utility-Scale Mapping
# Author: Jayne Piepenburg, CEE
# Last updated: 11/17/2020
################################################################################################################

# Install packages, run as needed on new computer
## install.packages("ggplot2","geojsonio","broom","dplyr","rgdal","rmapshaper","leaflet","tmap")

# Load pacakges
library(ggplot2)
library(geojsonio)
library(broom)
library(dplyr)
library(rgdal)
library(rmapshaper)
library(spdplyr)
library(leaflet)
library(tmap)

## Get data-------------------------------------------------------------------------------------------------------------------------
# Set WD to where EIA data are stored locally
setwd("Z:/Shared/Public/EVALUATION/Current Work/Annual Industry Report/")

# Read in EIA-861 data from local CSV file
advanced_meter_2019 <- read.csv("./2020/EIA Data/2019 FR/Advanced_Meters_2019.csv",stringsAsFactors=FALSE,na.strings=c("","NA","."))
### Acronymns:
## AMR: Number AMR-Automated Meter Reading
## AMI: Number AMI-Advanced Metering Infrastructure
## HEN: Number Home Area Network
## Other: Number Non AMR/AMI Meters
## Total: Total Numbers of Meters
## MWh: Energy Served-AMI (MWh)
## DDA: Customers with Daily Digital Access
## DLC: Customers with DIrect Load COntrol

# Load HIFLD data via API connection to website
spdf <- geojson_read("https://opendata.arcgis.com/datasets/c4fd0b01c2544a2f83440dab292f0980_0.geojson",  what = "sp")
# 'fortify' the data to get a dataframe format, as required by ggplot2
spdf_fortified <- tidy(spdf, region = "ID")

## Manipulate data------------------------------------------------------------------------------------------------------------------
# merge EIA861 data with data.frame object in SpatialPolygonsDataFrame (spdf)
advanced_meter_2019$Utility.Number <- as.character(advanced_meter_2019$Utility.Number)
spdf@data <- left_join(spdf@data, advanced_meter_2019, by=c("ID"="Utility.Number"))

spdf@data <- spdf@data %>%
  mutate(AMR_prop = (as.numeric(AMR_Total)/CUSTOMERS),
         AMI_prop = (as.numeric(AMI_Total)/CUSTOMERS),
         HEN_prop = (as.numeric(HEN_Total)/CUSTOMERS),
         Other_prop = (as.numeric(Other_Total)/CUSTOMERS),
         Total_prop = (as.numeric(Total_Total)/CUSTOMERS),
         MWh_prop = (as.numeric(MWh_Total)/TOTAL_MWH),
         DDA_prop = (as.numeric(DDA_Total)/CUSTOMERS),
         DLC_prop = (as.numeric(DLC_Total)/CUSTOMERS)) # Compute the proportion of customers (or proportion of MWh) for plotting


# Add new variables to 'fortified' spdf data as well for static mapping using ggplot2
spdf_fortified <- left_join(spdf_fortified, spdf@data, by=c("id"="ID"))
# Create vector of US territories to omit from dataframe to improve mapping
US_territories <- c("AS","GU","MP","PR","VI") #American Samoa, Guam, Mariana Islands, Puerto Rico, Virgin Islands


## Make maps------------------------------------------------------------------------------------------------------------------------

### Static maps

# Create static map using ggplot: 



ggplot(data=spdf_fortified[!(spdf_fortified$STATE %in% US_territories),]) +
  geom_polygon(aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
  theme_void() +
  coord_map()

ggplot() +
  geom_polygon(data=spdf_fortified,
               aes(fill = CUSTOMERS, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()




### Interactive Maps

# Create map using tmap: 
tm_shape(spdf) +
  tm_polygons("CUSTOMERS", 
              style="quantile", 
              title="Proportion of Customers with Direct Load Control")

# Create an interactive world map using leaflet:
## Create a color palette for the map:
mypalette <- colorNumeric( palette="viridis", domain=spdf@data$DLC_prop, na.color="transparent")
mypalette(c(45,43))

## Create interactive map object
m <- leaflet(spdf) %>% 
  addTiles()  %>% 
  setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( fillColor = ~mypalette(DLC_prop), stroke=FALSE )
