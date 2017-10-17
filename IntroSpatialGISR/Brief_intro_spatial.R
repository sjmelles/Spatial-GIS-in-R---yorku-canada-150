##############################################################################
## York University Environmental Synthesis Week, Canada 150 ##
## Tuesday, Oct 17th	10am-12pm	Lumbers 306 ##
## Introduction to spatial analysis/GIS	stephanie melles ##

## Based on material in a tutorial by Robin Lovelace, Creating maps in R ##
## Intro-spatial-rl.pdf available at: https://rpubs.com/RobinLovelace##
##############################################################################

# load and install libraries; you will need to install tmap and maptools, 
# possibly a few others.
# install.packages("leaflet")
library(raster)
library(leaflet)
library(grid)
library(rgdal)
library(tmap)
library(ggplot2)
library(rgeos)
library(maptools)
library(dplyr)
# library(devtools)
library(rgdal)

# download data
download.file("http://opendata.toronto.ca/gcc/neighbourhoods_planning_areas_wgs84.zip", 
              "data/neighbourhoods_planning_areas_wgs84.zip")
# unzip file
unzip("data/neighbourhoods_planning_areas_wgs84.zip", files = NULL, list = FALSE, 
      overwrite = TRUE, junkpaths = FALSE, exdir = "data", unzip = "internal", 
      setTimes = FALSE)

# read in the shapefile
TorontoMap = readOGR(dsn = "data", layer = "NEIGHBORHOODS_WGS84")

# here's where we'll get to today...
leaflet() %>%
  addTiles() %>%
  addPolygons(data = TorontoMap) %>%
  addPopups(-79.3788, 43.6577, '<b>Ryerson University Campus</b>')

# Generate data
x <- 1:400
y <- sin(x / 10) * exp(x * -0.01)
plot(x, y) # plot the result

# Start with a basic plot of the object, TorontoMap
plot(TorontoMap)
head(TorontoMap@data, n = 2)

# load related data on health and wellbeing in the City
TorontoHealth <- read.csv("data/City_Tor_WB-Health_2011.csv")
head(TorontoHealth)

nrow(TorontoMap) # return the number of rows in the data attribute table of our shapefile. 
# This is equivalent to the number of polygons (neighbourhoods) in the shapefile. 
nrow(TorontoHealth) 
names(TorontoHealth)

head(TorontoMap@data) # returns first five lines of datatable
# AREA_S_CD represents the neighbourhood ID, 
# but this variable was read in as character string 
# because of the way it was coded in Excel.
# The variable was then converted to a FACTOR variable when imported in R.
class(TorontoMap$AREA_S_CD) # factor

TorontoMap$AREA_S_CD<-as.numeric(TorontoMap$AREA_S_CD) 
# change variable class to numeric before we can do the join

head(TorontoMap$AREA_S_CD) # dataset to add to (results not shown)
head(TorontoHealth$NID) # the variables to join

#perform the join
TorontoMap@data <- left_join(TorontoMap@data, TorontoHealth, by = c('AREA_S_CD' = 'NID'))

# use the tmap package to view some of the data we just joined
qtm(TorontoMap, "PremMort") # plot the basic map

#investigate the structure of our shapefile object
head(TorontoMap@data, n = 2)
mean(TorontoMap@data$PremMort) # calculates average rate of deaths per 100,000) 

# find out the classes of all variables in the data table associated with our shapefile
sapply(TorontoMap@data, class)

# plot is one of the most useful functions in R, 
plot(TorontoMap) # shown above - try it on your computer

# select rows of TorontoMap@data where PremMort is less than 120
TorontoMap@data[TorontoMap$PremMort < 120, 1:3]

# Select zones where Premature Mortality is between 200 and 300
sel <- TorontoMap$PremMort > 200 & TorontoMap$PremMort < 300
plot(TorontoMap[sel, ]) # output not shown here

# add selected zones to map
plot(TorontoMap, col = "lightgrey") # plot the neighbourhood shapefile object
sel <- TorontoMap$PremMort > 250
plot(TorontoMap[ sel, ], col = "turquoise", add = TRUE)
plot(TorontoMap[ sel, ], col = "turquoise", add = TRUE, title(main= "Neighbourhoods with higher PM"))

# explore thematic mapper package
vignette("tmap-nutshell")

qtm(shp = TorontoMap, fill = c("PremMort", "BCS"), 
    fill.palette = "Blues", ncol=2) 

?tmap # get more info on tmap

# Explore ggplot
p <- ggplot(TorontoMap@data, aes(BCS, CCS))
p
p + geom_point() 
p + geom_point(aes(colour = "red"))

#Explore leaflet
summary(TorontoMap)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = TorontoMap)

m = leaflet() %>% addTiles()  # a map with the default OSM tile layer
m # follow 'Leaflet' link on map

m %>% fitBounds(-79, 43, -80, 44) # check out fitBounds details 

# move the center to Ryerson
m = m %>% setView(-79.3788, 43.6577, zoom = 17)
m
m %>% addPopups(-79.3788, 43.6577, '<b>Ryerson University Campus</b>')
