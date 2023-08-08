#    __  __                           _             _____  
#   |  \/  |                         (_)           |  __ \ 
#   | \  / |   __ _   _ __    ___     _   _ __     | |__) |
#   | |\/| |  / _` | | '_ \  / __|   | | | '_ \    |  _  / 
#   | |  | | | (_| | | |_) | \__ \   | | | | | |   | | \ \ 
#   |_|  |_|  \__,_| | .__/  |___/   |_| |_| |_|   |_|  \_\
#                    | |                                   
#                    |_|                                   
#
# Basic maps in R ####
# using the
# maptiles, sf, sp, and prettymapr packages
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#
# load packages needed to make maps
library(sp)
library(sf)
library(maptiles)
library(prettymapr)
library(flextable)
library(rosm)
library(ggmap)

# make some palette options to use later if needed
pal4lite <- c("black", "purple2", "blue2", "cyan4", "forestgreen", 
              "darkorange", "gold3", "red3", "gray80", "white")
pal4liteTransp <- c("#000000","#912CEEb0","#0000EEb0","#008B8Bb0","#228B22b0",
                    "#CDAD00b0", "#FF8C00b0", "#CD0000b0", "#CCCCCC", "#FFFFFF")
pal4dark <- c("white", "pink1", "orange", "khaki1", "lightgreen", "cadetblue1", 
              "deepskyblue", "plum", "gray80", "black")

# read some data files to make map annotations...
git <- "https://raw.githubusercontent.com/Ratey-AtUWA/"
afr_map <- read.csv(file=paste0(git,"spatial/main/afr_map_v2.csv"), 
                      stringsAsFactors = TRUE)
places <- read.csv(file=paste0(git,"learningR/main/places.csv"),
                   stringsAsFactors = TRUE)

# ...and read data to plot on some of the maps
afs19 <- read.csv(file = paste0(git,"learningR/main/afs19.csv"),
                  stringsAsFactors = TRUE)

## Preparing to make maps ####

# first we define some commonly-used coordinate reference systems
# which define the projection of or GPS data

LongLat <- CRS("+proj=longlat +ellps=WGS84 
           +datum=WGS84 +no_defs") # uses Earth ellipsis specs from WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50, S hemisphere!
webMercator <- CRS(paste0("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0",
                          " +a=6378137 +b=6378137 +units=m +no_defs"))

# We will use these coordinate reference system objects later
# for now we're going to work in UTM coordinates

# We define the area we need for our map and save it in an object called 'extent'
# 'extent' is a 'simple features' object which refers to a formal standard (ISO
# 19125-1:2004) that describes how objects in the real world can be represented
# in computers - see https://r-spatial.github.io/sf/articles/sf1.html 
# (this is why we need the 'sf' or 'Simple Features' package by Pebesma (2018))

# The easiest way to get bounding coordinates (in latitude, longitude) is by using
# Google Maps or Google Earth. Google Earth allows the option to set the 
# coordinate system to UTM. Otherwise we would need to convert our SpatialPoints 
# object (see the **Appendix**). If our input coordinates are Longitude--Latitude,
# note that south latitudes (and west longitudes) are negative, and we want
# decimal degrees rather than degrees-minutes-seconds.

# For coordinates in the sp and maptiles packages, x coordinates
# are Eastings or Longitudes, and y coordinates are Northings or Latitudes.

# Make a map extent object
extent <- 
  st_as_sf(SpatialPoints(coords = 
                           data.frame(x = c(399800,400700),
                                      y = c(6467900,6468400)), 
                         proj4string = UTM50))

# NOTE: The projection we specify here will be the one the map plots in!

# We now need some functions from the 'maptiles' package (Giraud 2021). We're
# using one of the OpenStreetMap tile options, but the following tile providers
# also work:
# OpenStreetMap, OpenStreetMap.HOT, Esri.WorldStreetMap, Esri.WorldTopoMap,
# Esri.WorldImagery, Esri.WorldGrayCanvas, Stamen.Toner, Stamen.TonerBackground,
# Stamen.TonerHybrid, Stamen.TonerLines, Stamen.TonerLabels, Stamen.TonerLite,
# CartoDB.Positron, CartoDB.DarkMatter, CartoDB.Voyager (all CartoDB... tiles
# have variants which work), OpenTopoMap

# The option crop = TRUE is included to crop the tiles to our defined
# area in the 'extent' object. If we leave it out, the map may change
# shape, as it will use only square (uncropped) map tiles.

aftiles <- get_tiles(extent, provider = "OpenStreetMap.HOT", crop = TRUE)

# Plotting the maptiles map ####

# The 'aftiles' object we created is a 'SpatRaster' object which needs the
# maptiles package loaded to be able to plot it. We need to set the outer
# margins of the plot area (by default they are zeros) to allow plotting of 
# axes etc.

par(oma=c(3,3,1,1), lend="square")
plot(aftiles)

box(lwd=6,col="white") # tidies up skewness of UTM tiles a bit
axis(1, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.5, font=2)
axis(2, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.5, font=2)
box()
# add the prettymapr features:
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)

# since we have a map in UTM coordinates, we can now add plots of our data based
# on UTM locations (with a legend, of course).

with(afs19, points(Easting, Northing, lwd = 2, 
                   pch = c(0,1,2,3,5,6,15,17,18,19)[Group], 
                   col = rainbow(10,v=0.6,end=0.75)[Group]))
legend("bottomright", legend=levels(as.factor(afs19$Group)),
       pch = c(0,1,2,3,5,6,15,17,18,19), col = rainbow(10,v=0.6,end=0.75),
       pt.lwd = 2, title = "Group", inset = 0.02, ncol = 2)

# We can also add digitized map features such as wetland ponds, drains, etc.
# Ideally we would add these *before* plotting the data.
with(afr_map, lines(drain_E, drain_N, col = "cadetblue3", lwd = 2))
with(afr_map, lines(wetland_E, wetland_N, col = "cadetblue3", lwd = 1, lty = 2))

# Finally we would most likely want to add some text. 
# Text labels should also be added *before* plotting the data.
text(c(400263, 399962, 400047), c(6468174, 6468083, 6468237),
     labels = c("Chapman Drain","Kitchener Drain", "Woolcock Drain"),
     pos = c(2,2,4), cex = 0.8, font = 3, col = "cadetblue")

## Using maptiles to make a bubble map #### 

# (mostly as before, until we add symbols)

par(oma=c(3,3,1,1), lend="square")
plot(aftiles)
box(lwd=6,col="white") # tidies up skewness of UTM tiles a bit
axis(1, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.5, font=2)
axis(2, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.5, font=2)
box()
with(afr_map, lines(drain_E, drain_N, col = "cadetblue3", lwd = 2))
with(afr_map, lines(wetland_E, wetland_N, col = "cadetblue3", lwd = 1, lty = 2))
text(c(400263, 399962, 400047), c(6468174, 6468083, 6468237),
     labels = c("Chapman Drain","Kitchener Drain", "Woolcock Drain"),
     pos = c(2,2,4), cex = 0.8, font = 3, col = "cadetblue")
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)

# make bubbles using the symbols() function
with(afs19, symbols(Easting, Northing, add = TRUE, circles = 0.4*sqrt(Zn),
                    inches = FALSE, fg = "purple", bg = "#8000FF40"))
# manual legend (if-else control, in case first value is zero) 
if (pretty(afs19$Zn)[1] < 0.001) {
  bublo <- pretty(afs19$Zn)[2]/2
} else {
  bublo <- pretty(afs19$Zn)[1]
}
bubhi <- pretty(afs19$Zn)[NROW(pretty(afs19$Zn))]
symbols(c(400600,400600),c(6468040,6467980), circles=0.4*sqrt(c(bublo,bubhi)), add=T,
        lwd=1, inches=F, fg = "purple", bg = "#8000FF40")
text(c(400600,400620,400620),c(6468100,6468040,6467980), 
     labels=c("Zn (mg/kg)",bublo,bubhi), cex=0.85, pos = c(1,4,4))

## Percentile'bubble' maps ####

# As with the area-proportional bubble maps above, first we make the base map as
# we've done before.

# We make a new column in our data frame by cutting the measurement of interest,
# in this example Zn, into percentiles. The new column called QZn is
# a factor which identifies which percentile of Zn concentration each sample is
# in. We then use this factor to define symbols, sizes, and colours for each
# sample location. We add a line break to some text labels using '\n'.

par(oma=c(3,3,1,1), lend="square")
plot(aftiles)
box(lwd=6,col="white") # tidies up rotation of UTM tiles a bit
axis(1, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.5, font=2)
axis(2, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.5, font=2)
box()
with(afr_map, lines(drain_E, drain_N, col = "cadetblue3", lwd = 2))
with(afr_map, polygon(wetland_E, wetland_N, col = "cadetblue2", 
                      border = "cadetblue3"))
text(c(400263, 399962, 400047), c(6468174, 6468083, 6468237),
     labels = c("Chapman\nDrain","Kitchener\nDrain", "Woolcock\nDrain"),
     pos = c(2,2,4), cex = 0.8, font = 3, col = "cadetblue")
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)

### Adding the percentile bubble plot
afs19$QZn <- cut(afs19$Zn, quantile(afs19$Zn, 
                  p=c(0,0.02,0.05,0.25,0.5,0.75,0.95,0.98,1), 
                  na.rm=T), labels=c("Q0-02","Q02-05","Q05-25","Q25-50",
                                     "Q50-75","Q75-95","Q95-98","Q98-max"))
palette(pal4liteTransp) # predefined palette with some semitransparent colours

# plot the points with attributes defined by the percentile factor QZn...
with(afs19, 
     points(Easting, Northing, 
            pch = c(22,22,22,3,4,21,21,21)[QZn], 
            col = c(1,1,1,4,5,1,1,1)[QZn], bg = c(1:8)[QZn], 
            lwd = c(1,1,1,2,2,1,1)[QZn], 
            cex = c(0.5,0.65,0.8,1,1,1.5,2.2,3)[QZn])
)
# ...and add a matching legend
legend("bottomright", legend = levels(afs19$QZn), title="Zn",
       pch = c(22,22,22,3,4,21,21,21), col = c(1,1,1,4,5,1,1,1),
       pt.lwd = c(1,1,1,2,2,1,1), pt.bg = c(1:8), 
       pt.cex = c(0.5,0.65,0.8,1,1,1.5,2.2,3),
       bty = "n", inset = 0.02, cex = 0.85, y.intersp = 1.2)
palette(pal4lite) # reset to non-transparent palette (optional)
afs19$QZn <- NULL # to delete quantile column (optional; you can leave it in)

# The resulting percentile bubble map adds value to the 'standard' bubble map, 
# as it adds some statistical meaning to the bubble sizes.
# A similar map could be created by using the Tukey boxplot thresholds 
# instead of percentiles which could show potential outliers
# (i.e., using the boxplot.stats() function to generate categories instead of
# the quantile() function.)

## Alternative 2 -- Maps in R using the ggmap package

# The ggmap package (Kahle and Wickham 2013) is an extension of ggplot,
# so it's easier to use if you are familiar with ggplot and the
# associated family of packages. *The Google maps key is owned by the Unit*
# *Coordinator, so please use it responsibly!*

# First we make a ggmap object:

library(ggmap)
register_google(key = "AIzaSyDU7QiTWE4RGFFQNmhWy51n7e4RBeHKjc0")
udubua.gg <- get_googlemap(center=c(115.8213,-31.98165), 
                           zoom = 16, maptype = "terrain", color = "bw")

# Next we plot the ggmap object using ggplot grammar. It's possible to just plot 
# the object (i.e. run ggmap(udubua.gg) ), but it's good to have more 
# control over plot options AND to plot some data over the base map. In the 
# example, we use the aesthetic in geom_point() to plot 
# different categories with different shapes and colours, with the categories
# defined by the factor Type.

ggmap(udubua.gg) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_text(aes(x = 115.825, y = -31.98, label = "Swan\nRiver",
                fontface = "italic", family="sans"), 
            size = 4, vjust = 0, hjust = 0, color="gray40") + 
  geom_point(aes(x = Longitude, y = Latitude, col=Type, shape=Type), 
             data = places, size=3)                                 +
  theme(axis.text=element_text(size=9, color="black"),
        axis.title=element_text(size=11,face="bold"))
```
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Alternative 3 -- the 'rosm' package ####

# The rosm package (Dunnington 2022) allows users to produce background maps from
# several map tile providers.

library(rosm)

# The following map tile types seem to work using osm.plot() in the 
# rosm package:
# "osm", "hotstyle", "stamenbw", "stamenwatercolor", "cartodark", "cartolight"  

# The following map tile types seem to work using bmaps.plot() in the 
# rosm package:
# "Aerial", "AerialWithLabels"   

# Plot an osm-type map
par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), lend=2, ljoin=1, tcl=0.25)
# specify limits of map in order N, E, S, W
udubua.rosm <- makebbox(-31.9751,115.8284,-31.9882,115.8143)
# plot tiles
osm.plot(udubua.rosm, type="cartolight", zoomin=0)
# add some points to the plot
osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184), 
           c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765), 
           col="purple", lwd=2, pch=7, cex=1.5)
box(which = "plot")

# another option using a Bing map as background
par(mar=c(1,1,1,1))
bmaps.plot(udubua.rosm, type="AerialWithLabels", zoomin=0)
osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184), 
           c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765), 
           col="yellow", lwd=2, pch=10, cex=1.5)
osm.text(c(115.825,115.826), c(-31.98, -31.988),
         labels = c("Matilda\nBay", "Pelican Point"),
         col = c("powderblue","honeydew"), font = 3, cex = 0.75)

# embedding rosm functions inside prettymap()
par(mar = c(4,4,1,1))
prettymap({
  osm.plot(udubua.rosm, type="stamenbw", zoomin = -1)
  osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184), 
             c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765), 
             col="red", lwd=3, pch = 15, cex=1.5)
  osm.text(115.825, -31.98, labels = "Matilda\nBay", 
           font = 3, col = "azure")
  osm.text(115.824, -31.9865, labels = "Pelican Point", 
           font = 3, col = "navy")
},
oma = c(3,3,1,1), drawarrow = T, arrow.scale = 1, 
arrow.border = "pink", arrow.text.col = "pink",
scale.htin = 0.15, scale.label.cex = 1.2, scale.label.col = "pink", 
scale.linecol = "pink", scale.pos = "bottomright"
)
legend("right", legend = "Coffee is\nfound here",
       pch = 15, col = "red",
       inset = 0.02, cex = 0.9, pt.cex = 1,
       x.intersp = 0.5, y.intersp=0.7)
axis(1,tcl=-0.2); axis(2,tcl=-0.2)

# The rosm axes, however, are in web Mercator coordinates (and are messy).
# This is aproblem with no easy fix. Either we don't include axes (not ideal),
# figure out how to plot them, or use another package.

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Alternative 4 using the OpenStreetMap package (needs working Java) ####

# We can also use the OpenStreetMap R package (Fellows, 2019) to make a plot-able
# map object, based on the coordinates which bound a rectangular area. We need the
# north-west (upper left) and south-east (lower right) coordinates of the
# rectangular map we want to plot. (**Note** that the coordinates are in the order
# (Latitude, Longitude), i.e. (y,x), whereas (x,y) seems more intuitive.)

# Note that there can be problems trying to use the openstreetmap package,
# especially on Mac/Apple computers. This relates to the need for a
# working Java installation (Windows and Mac) and the requirement for the XQuartz 
# app on Mac OS. Otherwise the openstreetmap package can be very useful!

# NOTE: The PDF document accompanying this code uses a different example

library(OpenStreetMap) # load package if it isn't already

# make an OpenStreetMap object . . .
# (replace -xx.xxxx,xxx.xxxx with actual coodinates,
#  obtained from Google maps or Google Earth)
afr.osm <- openmap(upperLeft = c(-xx.xxxx,xxx.xxxx),  # latitude first
                   lowerRight = c(-xx.xxxx,xxx.xxxx), 
                   zoom = 16, type = "osm")
# ...and convert the osm object to a utm projection map
afr.utm <- openproj(afr.osm, 
    projection = "+proj=utm +zone=50 +south")
#
# plot the map after setting plotting parameters...
par(mar=c(3,3,1,1), mgp=c(2,0.5,0))
plot(afr.utm, removeMargin = FALSE)
# ...manually add axes and axis titles
axis(1)
axis(2)
box()
mtext(side=1, line=1.6, 
      text="Easting, UTM Zone 50 (m)",
      font=2)
mtext(side=2, line=1.6, 
      text="Northing, UTM Zone 50 (m)",
      font=2)
#
# use prettymapr functions to add essential map annotations
addnortharrow()
addscalebar(plotepsg = 28350)
#
# make a nice colour palette to plot points
palette(c("black",rainbow(9, v=0.7)))
#
# plot points at sample locations, categorised by factor 'Group'
points(afs19$Easting, afs19$Northing, 
       pch=seq(0,9)[afs19$Group],
       col=seq(1,10)[afs19$Group])
legend("bottomright", ncol=2,
       legend=levels(afs19$Group),
       pch=seq(0,9),
       col=seq(1,10),
       title="Group")
#
# 'bubble' plot to show relative concentrations of 
# an element at sample locations
symbols(afs19$Easting, afs19$Northing, add=T,
        circles=sqrt(afs19$Ca), inches=0.1,
        bg="#FFFF0040")
# (it would be good to add some sort of legend to this -
#  see how we did it above for the maptiles map!)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

## Appendix - coordinate conversions ####

# Converting UTM to LongLat
  
#   Make a spatial object (package 'sp') containing the UTM coordinates

# The example uses explicit values (as used previously to generate the maptiles
# map), but the coordinates could also be obtained from a data frame - edit to
# suit!
  
utm.temp <- SpatialPoints(coords = data.frame(x = c(399800,400700),
                                              y = c(6467900,6468400)), 
                          proj4string = UTM50)

# To extract coordinates from a data frame, for example:
# (**NOTE** - missing coordinates are not allowed!)

utmCoords <- na.omit(afs19[,c("Easting","Northing")]) # remove rows with NAs
utmTemp <- SpatialPoints(coords = utmCoords[, c("Easting","Northing")],
                         proj4string = UTM50)

# We then use the spTransform() function from the 'sp' package to 
# convert to long-lat, with results in another spatial object:
  
longlat.temp <- spTransform(utm.temp, CRSobj = LongLat)

# If we wanted to write the coordinates back into a data frame (for illustration
# purposes we call the data frame 'data1'), we would run something like
# the following code:
  
# correctly name columns in temporary long-lat object
colnames(longlat.temp@coords) <- c("Longitude","Latitude")
# write converted coordinates back into data frame
data1$Longitude <- longlat.temp@coords[,"Longitude"]
data1$Latitude <- longlat.temp@coords[,"Latitude"]
# check them
data1[,c("Longitude", "Latitude")]
# remove temporary objects
rm(list = c("ll.temp","utm.temp"))

# References and R packages ####

# Dunnington, Dewey (2017). prettymapr: Scale Bar, North Arrow, and Pretty Margins
# in R. R package version 0.2.2. <https://CRAN.R-project.org/package=prettymapr>.

# Dunnington D (2022). _rosm: Plot Raster Map Tiles from Open Street Map and 
# Other Sources_. R package version 0.2.6, 
# <https://CRAN.R-project.org/package=rosm>.
  
# Fellows, Ian and using the JMapViewer library by Jan Peter Stotz (2019).
# OpenStreetMap: Access to Open Street Map Raster Images. R package version
# 0.3.4. <https://CRAN.R-project.org/package=OpenStreetMap>.

# Giraud T (2021). _maptiles: Download and Display Map Tiles_. R package version 
# 0.3.0, <https://CRAN.R-project.org/package=maptiles>.

# Gohel D (2022). _flextable: Functions for Tabular Reporting_. R package version 
# 0.7.2, <https://CRAN.R-project.org/package=flextable>.
  
# Pebesma, E., 2018. Simple Features for R: Standardized Support for Spatial 
# VectorData. The R Journal 10(1), 439-446, 
# <https://doi.org/10.32614/RJ-2018-009>. (package 'sf')

# Pebesma, E.J., R.S. Bivand, 2005. Classes and methods for spatial data in R. 
# R News 5(2), https://cran.r-project.org/doc/Rnews/. (package 'sp')

# [end code]