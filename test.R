#    __  __                           _             _____  
#   |  \/  |                         (_)           |  __ \ 
#   | \  / |   __ _   _ __    ___     _   _ __     | |__) |
#   | |\/| |  / _` | | '_ \  / __|   | | | '_ \    |  _  / 
#   | |  | | | (_| | | |_) | \__ \   | | | | | |   | | \ \ 
#   |_|  |_|  \__,_| | .__/  |___/   |_| |_| |_|   |_|  \_\
#                    | |                                   
#                    |_|                                   

# # Basic maps in R with graphics elements plotted over tile backgrounds
# 
# This guide shows some of the more common and/or easy ways to make maps in R
# using map-tile-based backgrounds with data plotted over the base maps. We
# introduce *coordinate reference systems*, *four packages* which allow tile-based
# map drawing, a few ways of *including user data*, and an Appendix on simple
# *coordinate conversion*. 
# The intention is to provide options to plot informative maps, which include the
# type of data collected in this unit, relatively simply. We *will not* cover
# vector-based maps, importing GIS shapefiles, interpolation of point data,
# chloropleth maps, and many other map plotting methods.
# 
## ----world map for decoration, fig.align='center', fig.height=2, fig.width=5, message=FALSE, warning=FALSE, out.width='45%', echo=FALSE----
require(ggplot2); require(maps); mp <- NULL
mapWorld <- borders("world", colour="darkseagreen3", fill="darkseagreen3")
ggplot() + mapWorld + 
  xlim(-180,180) + theme_minimal()

#### load packages needed to make maps ####
# install them first if you don't have them on your own device
library(sp)
library(sf)
library(maptiles)
library(prettymapr)
library(rosm)
library(ggmap)
library(ggsn)
# un-comment next line if you can run it from the console without crashing R!
# library(OpenStreetMap) 

## ----load maps data and do setups ####
git <- "https://raw.githubusercontent.com/Ratey-AtUWA/"
afr_map <- read.csv(file=paste0(git,"spatial/main/afr_map_v2.csv"), 
                      stringsAsFactors = TRUE)
places <- read.csv(file=paste0(git,"learningR/main/places.csv"),
                   stringsAsFactors = TRUE)
taita <- data.frame(lon = c(174.967,174.9549,174.9489,174.9521), 
                    lat = c(-41.1761,-41.1812,-41.1831,-41.1838), 
                    name = c("Tait\u0101 College", "Tait\u0101 School",
                          "Fraser Park","Home")) # we need this later...
taita$name <- as.factor(taita$name) # ...and this
pal4lite <- c("black", "purple2", "blue2", "cyan4", "forestgreen", 
              "darkorange", "gold3", "red3", "gray80", "white")
pal4liteTransp <- c("#000000","#912CEEb0","#0000EEb0","#008B8Bb0","#228B22b0",
                    "#CDAD00b0", "#FF8C00b0", "#CD0000b0", "#CCCCCC", "#FFFFFF")
pal4dark <- c("white", "pink1", "orange", "khaki1", "lightgreen", "cadetblue1", 
              "deepskyblue", "plum", "gray80", "black")
windowsFonts(nar=windowsFont("Arial Narrow"), monosans=windowsFont("Consolas"))

#### read Ashfield Flats data  ####
afs19 <- read.csv(file=paste0(git,"learningR/main/afs19.csv"), 
                  stringsAsFactors = TRUE) # use this one
afs22 <- read.csv(file=paste0(git,"learningR/main/afs22S1.csv"), 
                  stringsAsFactors = TRUE)

#### define some commonly-used coordinate reference systems  ####
# these define the projection of our GPS data

LongLat <- CRS("+proj=longlat +ellps=WGS84 
           +datum=WGS84 +no_defs") # uses Earth ellipsis specs from WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50, S hemisphere!

# We will use these coordinate reference system objects later. For now
# we're going to work in UTM coordinates.
# 
# Next we'll convert our data frames to *Simple Features* spatial data, using
# the R package sf (Pebesma, 2018). Simple Features is a formal standard (ISO
# 19125-1:2004) that describes how objects in the real world can be represented
# in computers - see <https://r-spatial.github.io/sf/articles/sf1.html>.
# 
# We also make equivalents of these two sf-dataframes in the
# longitude--latitiude coordinate system, using the st_transform() function:

afs19utm <- st_as_sf(afs19[-9,], # omitting row 9 where coordinates are missing
                     coords = c("Easting","Northing"),
                     crs = UTM50)
afs22utm <- st_as_sf(afs22, coords = c("Easting","Northing"),
                     crs = UTM50)
afs19ll <- st_transform(afs19utm, crs = LongLat)
afs21ll <- st_transform(afs19utm, crs = LongLat)

# We define the area we need for our map and save it in a simple features object called extent.
# 
# An easy way to get bounding coordinates (in longitude, latitude) is by using
# Google Maps or Google Earth. Google Earth allows the option to set the
# coordinate system to UTM. If we generate latitude--longitude coordinates, we
# would need to convert our *Simple Features* object (see the **Appendix**). If
# our input coordinates are Longitude--Latitude, note that south latitudes (and
# west longitudes) are negative, and we want decimal degrees rather than
# degrees-minutes-seconds. Also note that coordinates from Google Maps and
# Google Earth (except in saved .kml files) are in the order (Latitude,
# Longitude); i.e. (y,x), whereas (x,y) (Longitude, Latitude) seems to make
# more sense.
# 
# For coordinates in the sf and maptiles packages, x coordinates are commonly
# Eastings or Longitudes, and y coordinates are commonly Northings or Latitudes.
# 
## make map extent object ####
extent <- st_as_sf(data.frame(x=c(399800,400700),y=c(6467900,6468400)),
                   coords = c("x","y"), crs = UTM50)

# 
# **NOTE**: The projection we specify here will be the one the map plots in!
# 
#### Getting the map tile data ####
# 
# We now need some functions from the [maptiles package (Giraud 2021).
# We're using one of the OpenStreetMap tile options, but the following
# tile providers also work: 
# OpenStreetMap, OpenStreetMap.HOT, Esri.WorldStreetMap, Esri.WorldTopoMap,
# Esri.WorldImagery, Esri.WorldGrayCanvas, Stamen.Toner, Stamen.TonerBackground,
# Stamen.TonerHybrid, Stamen.TonerLines, Stamen.TonerLabels, Stamen.TonerLite,
# CartoDB.Positron, CartoDB.DarkMatter, CartoDB.Voyager (all CartoDB... tiles have
# variants which work), and OpenTopoMap 

# The option crop = TRUE is included to crop the tiles to our defined area in
# the [extent object. If we leave it out, the map may change shape as it will
# use only square (uncropped) map tiles.
# 
## ----get_tiles
aftiles <- get_tiles(extent, provider = "OpenStreetMap.HOT", crop = TRUE)

#### Plotting the map ####
#
# The 'aftiles' object we created is a 'SpatRaster' object which needs the
# maptiles package loaded to be able to plot it. We need to set the outer
# margins of the plot area (by default they are zeros) to allow plotting
# of axes etc.

par(oma=c(3,3,1,1), lend="square")
plot(aftiles)

box(lwd=6,col="white") # tidies up skewness of UTM tiles a bit
axis(1, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.5, font=2)
axis(2, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.5, font=2)
box()

# 
# The next chunk of code adds prettymapr features (scale bar and north arrow).
# In this code, plotepsg = 32750 refers to the EPSG code for the UTM projection
# in Zone 50 (EPSG 32750), which we need so that the scale bar shows the correct
# distances. (Long-Lat is EPSG 4326 in WGS84)
# 
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)
# 
#### Adding our data and map annotations ####
# 
# Very often we would like to **add our own information to a map**, such
# as the location of samples, often with different sizes, shapes, or
# colours of symbols to represent numerical information.
# 
# Since we have a map in UTM coordinates, we can now add plots of our data based
# on UTM locations (with a legend, of course). We can plot the points from the
# non-spatial data frame afs19, but here we plot the points from afs19utm, with
# the add=TRUE option. We add a legend to the plot in the usual way.

# adding data from a simple features data frame
with(afs19utm, plot(geometry, add=TRUE, lwd = 2, 
                    pch = c(0,1,2,3,5,6,15,17,18,19)[Group], 
                    col = rainbow(10,v=0.6,end=0.75)[Group]))
legend("bottomright", legend=levels(as.factor(afs19$Group)),
       pch = c(0,1,2,3,5,6,15,17,18,19), col = rainbow(10,v=0.6,end=0.75),
       pt.lwd = 2, title = "Group", inset = 0.02, ncol = 2)

# We can also add digitized map features such as wetland ponds, drains,
# etc., if these are not on the map tiles already. Ideally we would add
# these before plotting the data, to avoid over-plotting.
# 
with(afr_map, lines(drain_E, drain_N, col = "cadetblue", lwd = 2))
with(afr_map, lines(wetland_E, wetland_N, col = "cadetblue", 
                    lwd = 1, lty = 1))

# Finally, we would most likely want to add some text. Text labels should
# also be added *before* plotting the data.

text(c(400263, 399962, 400047), c(6468174, 6468083, 6468237),
     labels = c("Chapman Drain","Kitchener Drain", "Woolcock Drain"),
     pos = c(2,2,4), cex = 0.8, font = 3, col = "cadetblue")

#### Making a bubble map ####
# 
# We use essentially the same code as for the maps above (except plotting the
# annotations and data in the correct order!). Then we use the symbols()
# function to make the 'bubbles', making sure that we include the options add =
# TRUE so we overplot the base map, and inches = FALSE so we can manually scale
# the bubbles. The factor of 0.4 used to scale the circles in the code for
# bubbles and legend is *found by trial-and-error* (it *is* possible to write an
# algorithm to estimate a scaling factor from the data). A map like this is a
# good exploratory data analysis tool, as even without the legend it shows any
# unevenness in concentrations, including where high concentrations are located.
# 
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
# plot bubbles using the symbols() function
with(afs19, symbols(Easting, Northing, add = TRUE, circles = 0.4*sqrt(Zn),
                    inches = FALSE, fg = "purple", bg = "#8000FF40"))
# manual legend
if (pretty(afs19$Zn)[1] < 0.001) {
  bublo <- pretty(afs19$Zn)[2]/2
} else {
  bublo <- pretty(afs19$Zn)[1]
}
bubhi <- pretty(afs19$Zn)[NROW(pretty(afs19$Zn))]
symbols(c(400600,400600),c(6468040,6467980), circles=0.4*sqrt(c(bublo,bubhi)), 
        add=T, lwd=1, inches=F, fg = "purple", bg = "#8000FF40")
text(c(400600,400620,400620),c(6468100,6468040,6467980), 
     labels=c("Zn (mg/kg)",bublo,bubhi), cex=0.85, pos = c(1,4,4))

#### Percentile bubble map ####
# 
# We make a new column in our data frame by cutting the measurement of interest,
# in this example Zn, into percentiles. The new column called QZn is a
# factor which identifies which percentile of Zn concentration each sample is
# in. We then use this factor to define symbols, sizes, and colours for each
# sample location. We add a line break to text labels using \n.
# 
par(oma=c(3,3,1,1), lend="square")
plot(aftiles)
box(lwd=6,col="white") # tidies up skewness of UTM tiles a bit
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
# percentile plot
afs19$QZn <- cut(afs19$Zn, quantile(afs19$Zn, 
                 p=c(0,0.02,0.05,0.25,0.5,0.75,0.95,0.98,1), 
                 na.rm=T), labels=c("Q0-02","Q02-05","Q05-25","Q25-50",
                                    "Q50-75","Q75-95","Q95-98","Q98-max"))
palette(pal4liteTransp) # use colours with semi-transparency (defined near top)
# use percentile factor column to categorize points
with(afs19, 
     points(Easting, Northing, 
            pch = c(22,22,22,3,4,21,21,21)[QZn], 
            col = c(1,1,1,4,5,1,1,1)[QZn], bg = c(1:8)[QZn],
            lwd = c(1,1,1,2,2,1,1)[QZn], 
            cex = c(0.5,0.65,0.8,1,1,1.5,2.2,3)[QZn])
     )
legend("bottomright", legend = levels(afs19$QZn), title="Zn",
       pch = c(22,22,22,3,4,21,21,21), col = c(1,1,1,4,5,1,1,1),
       pt.lwd = c(1,1,1,2,2,1,1), pt.bg = c(1:8), 
       pt.cex = c(0.5,0.65,0.8,1,1,1.5,2.2,3),
       bty = "n", inset = 0.02, cex = 0.85, y.intersp = 1.2)
palette(pal4lite) # change back to non-transparent palette (optional)
afs19$QZn <- NULL # to delete quantile column (optional; you can leave it in)

# The resulting percentile bubble map adds value to the 'standard' bubble map,
# as it adds some statistical meaning to the bubble sizes. A similar map could
# be created by using the Tukey boxplot thresholds instead of percentiles which
# could show potential outliers (i.e. using the boxplot.stats() function to
# generate categories instead of the quantile() function.)

# ----------------------------=<>=----=<>=----=<>=---------------------------- #
#### Alternative 2 -- Maps in R using the ggmap package ####
# 
# The ggmap package (Kahle and Wickham 2013) is an extension of ggplot, so it's
# easier to use if you are familiar with ggplot and the associated family of
# packages. *The Google maps key is owned by* *the Unit Coordinator, so please
# use it responsibly!*
# 
# First we make a ggmap object, locating the map by its central coordinate with
# the extent controlled by the zoom = option:
# 
library(ggmap)
register_google(key = "AIzaSyDU7QiTWE4RGFFQNmhWy51n7e4RBeHKjc0")
udubua.gg <- get_googlemap(center=c(115.8213,-31.98165), 
                           zoom = 16, maptype = "terrain", color = "bw")

# Next we plot the ggmap object using ggplot grammar. It's possible to just plot
# the object (i.e. run ggmap(udubua.gg)), but it's good to have more control
# over plot options **and** to plot some data over the base map. In the example,
# we use the aesthetic in geom_point() to plot different categories with
# different shapes and colours, with the categories defined by the factor Type.

# A range of map styles is available by selecting a combination of one of
# maptype = "terrain", "satellite", "roadmap", or "hybrid",
# together with color = "color" or "bw".

# Note: the ggsm package is designed to add scale bar and north arrow to ggmaps,
# but it seems buggy and we currently don't recommend it.

ggmap(udubua.gg) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_text(aes(x = 115.825, y = -31.98, label = "Swan\nRiver",
                fontface = "italic", family="sans"), 
            size = 4, vjust = 0, hjust = 0, color="gray40") + 
  geom_point(aes(x = Longitude, y = Latitude, col=Type, shape=Type), 
             data = places, size=3) +
  theme(axis.text=element_text(size=9, color="black"),
        axis.title=element_text(size=11,face="bold"))

# There are numerous possibilities with ggmap and the features made possible by
# ggplot2 that are not illustrated here. For example, we could use size as an
# aesthetic (i.e. include within aes(...) with size = variableName), to generate
# something like a bubble map. 
# Although there is an option to set the map size (and therefore aspect ratio)
# in the get_googlemap() function, we have found that this doesn't work
# consistently, so we recommend leaving the map dimensions and aspect ratio at
# their default values.
# 
#### Other map types using ggmap ####
# 
# Another option currently implemented in ggmap are some of the Stamen map
# tiles, accessible with the get_[stamenmap() function.
# 
#   - The Stamen "terrain" tiles seem to be available up to zoom = 13, and are
#     good for showing maps at the scale of 1-2 suburbs or a small town (example
#     below), with 'hill shading' to show terrain.
#   - The Stamen toner variants are minimalist maps which are good at
#     de-emphasizing the base map background to emphasise added data (see the
#     example further down).
#   - The Stamen "watercolor" tiles are also available.
# 
tai_Stamen <- get_stamenmap(bbox=c(left=174.9, bottom = -41.2,
                                   right = 175.0, top = -41.15),
                            zoom=13, # seems to be max zoom for terrain
                            maptype = "terrain", color = "color")
ggmap(tai_Stamen) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_rect(aes(xmin=174.94, xmax=174.96, ymin=-41.178,ymax=-41.188),
               colour="red3", fill=NA, size=1.2) +
  geom_text(aes(x = 174.961, y = -41.183, label = "live\nplay\nschool",
                fontface = "bold", family="mono"), 
            size = 5, vjust = 0.5, hjust = 0, color="red3", lineheight=0.8) +
  geom_text(aes(x = 174.91, y = -41.16, 
                label = paste("(author's childhood neighbourhood,\n",
                              "Tait\u0101,","Aotearoa New Zealand)"),
                fontface = "italic", family="serif"),
            size = 4.5, lineheight=0.8, vjust = 0, hjust = 0, color="#004000") +
  geom_point(aes(x = lon, y = lat, color=name), data=taita, size = 4) +
  scale_color_manual(values = c("navy","goldenrod","darkorchid","seagreen")) +
  theme(axis.text=element_text(size=9, color="black"),
        axis.title=element_text(size=11,face="bold")) + 
  theme_bw()

# 
# The next example* uses the simple features information in one of the data
# frames we made at the beginning. The sf package introduces a new 'geom',
# geom_sf() for plotting in ggmap. We need to use the option inherit.aes = FALSE
# in geom_sf, to overrides the default aesthetics from the ggmap object.
# We also add coord_sf to ensure that all layers use a common CRS (oordinate
# reference system).
# Run help(geom_sf) for more information.
# 
afr.gg <- get_map(c(115.9445,-31.9175), zoom=17, col = "bw", 
                  maptype = "toner-lite")

ggmap(afr.gg) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_text(aes(x = 115.944, y = -31.918, label = "Ashfield\nFlats",
                fontface = "italic", family="sans"), 
            size = 4, color="gray65", lineheight=0.8) +
  geom_text(aes(x = 115.942, y = -31.9201, label = "Swan River",
                fontface="italic", family="sans"), size=4, color="gray65") +
  geom_path(aes(x = drain_lon, y=drain_lat), data=afr_map, 
            color = "slategray2", size = 1.25) +
  geom_polygon(aes(x=wetland_lon, y=wetland_lat), data = afr_map,
               color = "slategray", fill="azure3") + 
  geom_sf(data = afs21ll, aes(bg=Zn, size=Zn), shape=21, inherit.aes = FALSE) +
  scale_fill_viridis_c(alpha = 0.7) + 
  scale_size_area(breaks = c(30,100,300,1000,3000,5000), max_size = 9) +
  theme_bw() +
  coord_sf(crs = st_crs(4326))

# ----------------------------=<>=----=<>=----=<>=---------------------------- #
#### Alternative 3 -- the 'rosm' package ####
# 
# The rosm package (Dunnington 2022) allows users to produce background
# maps from several map tile providers.
# 
# The following map tile types seem to work using osm.plot() in the rosm
# package:
# "osm", "hotstyle", "stamenbw", "stamenwatercolor", "cartodark", "cartolight"
# 
# The following map tile types seem to work using bmaps.plot() in the rosm
# package:
# "Aerial", "AerialWithLabels"

par(mar = c(.1,.1,.1,.1), mgp = c(1.7,0.3,0), lend=2, ljoin=1, tcl=0.25)

# specify limits of map in order N, E, S, W
udubua.rosm <- makebbox(-31.9751,115.8284,-31.9882,115.8143)

# plot an osm-type map
osm.plot(udubua.rosm, type="cartolight", zoomin=0)

# add some points to the plot
osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184), 
           c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765), 
           col="purple", lwd=2, pch=7, cex=1.5)
box(which = "plot")

# The resulting map uses the cartolight map tile style to produce a base map
# with light colours -- this can be useful when we are trying to emphasise the
# data rather than the map features.
# 
##### Another rosm option using a Bing map as background 

par(mar=c(.1,.1,.1,.1))
bmaps.plot(udubua.rosm, type="AerialWithLabels", zoomin=0)
osm.points(c(115.82, 115.8186, 115.8211, 115.8182, 115.8184), 
           c(-31.9835, -31.9829, -31.9821, -31.9790, -31.9765), 
           col="yellow", lwd=2, pch=10, cex=1.5)
osm.text(c(115.825,115.826), c(-31.98, -31.988),
         labels = c("Matilda\nBay", "Pelican Point"),
         col = c("powderblue","honeydew"), font = 3, cex = 0.75)

# As shown by the output, the Bing map tiles are aerial photographs. Sometimes
# this is useful for showing features like vegetation etc. on the base map.
# 
##### Embedding rosm functions inside prettymap()

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

# It's helpful to include a scale and North arrow on the plot, but the
# axes in the map we just made, however, are in web Mercator coordinates
# (and are messy). This is a problem with no easy fix. Either we don't include
# axes (not ideal, since we should show some coordinates), figure out how
# to plot them, or use another package.
# 
#### Alternative 4 - use the OpenStreetMap package to make an initial map object
# 
# We can also use the OpenStreetMap R package (Fellows, 2019) to make a
# plot-able map object, based on the coordinates which bound a rectangular area.
# We need the north-west (upper left) and south-east (lower right) coordinates
# of the rectangular map we want to plot. (**Note** that the coordinates are in
# the order (Latitude, Longitude); i.e. (y,x), whereas (x,y) seems more
# intuitive.)
# 
# **NOTE** that there can be problems trying to use the OpenStreetMap package,
# especially on Mac/Apple computers.
# This relates to the need for a working Java installation (Windows and Mac) and
# the requirement for the XQuartz app on Mac OS. Otherwise the OpenStreetMap
# package can be very useful!

# make a basic osm map an plot it
UWA_osm <- openmap(upperLeft = c(-31.974, 115.812), # latitude first
                   lowerRight = c(-31.988, 115.828), 
                   zoom = 16,
                   type = "osm")
plot(UWA_osm)

# 
# This map needs some attention. Without some prior knowledge, we don't know
# where on Earth's surface this map represents, we don't know the direction it's
# oriented in, and we don't know the scale. We can add a north arrow to indicate
# direction, a scale bar to show the scale, and axes to show any coordinates
# which should show the location on Earth.

require(prettymapr)
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4)
plot(UWA_osm, removeMargin = FALSE)
axis(1)
axis(2)
addnortharrow()
addscalebar(plotepsg = 3857) # 3857 is EPSG code for web Mercator
box()

# 
# This map is better -- we now know scale and direction, but finding our
# location on Earth is tricky because the OpenStreetMap map projection is
# "Google Mercator" (*a.k.a*. "Web Mercator") which is not a projection most
# people are familiar with. We need a map in a commonly-used projection such as
# Longitude-Latitude, or Universal Transverse Mercator (UTM). We can change
# projections using the openproj() function in the OpenStreetMap package.

require(OpenStreetMap)
UWA_utm <- openproj(UWA_osm, projection = "+proj=utm +zone=50 +south")
cat("Show upper left (p1) and lower right (p2) coordinates\n")
print(UWA_utm$bbox, digits=6) # show bounding coordinates to check

# plot the map in its new projection
par(mar = c(3, 3, 0.5, 0.5), mgp = c(1.6, 0.3, 0), tcl = 0.4)
plot(UWA_utm, removeMargin = FALSE)
axis(1)
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.6, font = 2, cex = 1.2)
axis(2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.6, font = 2, cex = 1.2)
addnortharrow(scale = 1.2)
addscalebar(plotepsg = 32750, htin = 0.15, label.cex = 1.2)
box()

# 
# This map is now just what we need -- we know scale and direction, and we have
# axes in the Universal Transverse Mercator (UTM) projection with units of
# metres, which we would normally set up our field GPS to show.
# 
#### Final Words ####
# 
# We recommend using either the maptiles or ggmap packages to draw maps with
# tiled backgrounds, as they allow use of the state-of-the-art simple features
# system via the sf package.
# 
#### Appendix - coordinate conversions  ####
# 
# 1. Converting UTM to LongLat
# 
# Make a spatial (package sp) object containing the UTM coordinates
# 
# Example uses explicit values (as used previously to generate the maptiles
# map), but the coordinates could also be obtained from a data frame - edit to
# suit!

utm.temp <- st_as_sf(data.frame(x=c(399800,400700),y=c(6467900,6468400)),
                     coords = c("x","y"), crs = UTM50)

# We then use the st_transform() function from the sf package to convert to
# long-lat (or another projection), with results in another spatial object:

longlat.temp <- st_transform(utm.temp, crs = LongLat)

# 
# We now have two spatial objects which we can use (for instance) to define the
# map extent for a maptiles map:
# 
# using converted objects
require(sf); require(maptiles) # load packages if not done already
# using the utm object
tiles_utm <- get_tiles(utm.temp, provider = "OpenStreetMap", crop = TRUE)
# using the longitude-latitude object
tiles_longlat <- get_tiles(longlat.temp, provider = "OpenStreetMap",
                          crop = TRUE)
# . . . and so on

# 
# To extract coordinates from a data frame, for example: 
# (**NOTE** - missing coordinates are not allowed!)
# 
## ----utm spatial obj from dataframe, eval=FALSE--------------------------------------
## utmCoords <- na.omit(afs19[,c("Easting","Northing")]) # remove rows with NAs
## utmTemp <- SpatialPoints(coords = utmCoords[, c("Easting","Northing")],
##                          proj4string = UTM50)

# 
# We then use the 
# [spTransform 
# function from the [sp
# package to convert to long-lat, with results in another spatial object:
# 
## ----convert utm spatial object to longlat, message=FALSE, warning=FALSE, eval=FALSE----
## longlat.temp <- spTransform(utm.temp, CRSobj = LongLat)

# 
# If we wanted to write the coordinates back into a data frame (for
# illustration purposes we call the data frame 
# [data1), we would
# run something like the following code:
# 
## ----write converted coordinates to dataframe, eval=FALSE----------------------------
## # correctly name columns in temporary long-lat object
## colnames(longlat.temp@coords) <- c("Longitude","Latitude")
## # write converted coordinates back into data frame
## data1$Longitude <- longlat.temp@coords[,"Longitude"]
## data1$Latitude <- longlat.temp@coords[,"Latitude"]
## 
## data1[,c("Longitude", "Latitude")] # check them
## rm(list = c("ll.temp","utm.temp")) # remove temporary objects

#### References and R packages ####
# 
# Dunnington, Dewey (2017). prettymapr: Scale Bar, North Arrow, and Pretty
# Margins in R. R package version 0.2.2.
# <https://CRAN.R-project.org/package=prettymapr>.
# 
# Dunnington D (2022). *rosm: Plot Raster Map Tiles from Open Street Map
# and* *Other Sources*. R package version 0.2.6,
# <https://CRAN.R-project.org/package=rosm>.
# 
# Fellows, Ian and using the JMapViewer library by Jan Peter Stotz (2019).
# *OpenStreetMap: Access to Open Street Map Raster Images*. R package
# version 0.3.4. <https://CRAN.R-project.org/package=OpenStreetMap>.
# 
# Giraud T (2021). *maptiles: Download and Display Map Tiles*. R package
# version 0.3.0, <https://CRAN.R-project.org/package=maptiles>.
# 
# Pebesma, E., 2018. Simple Features for R: Standardized Support for
# Spatial VectorData. *The R Journal* **10** (1), 439-446,
# <https://doi.org/10.32614/RJ-2018-009>. (package **sf**)
# 
# Pebesma, E.J., R.S. Bivand, 2005. Classes and methods for spatial data
# in R. *R News* **5** (2), <https://cran.r-project.org/doc/Rnews/>.
# (package **sp**)