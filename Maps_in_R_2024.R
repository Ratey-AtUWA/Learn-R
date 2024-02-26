#    __  __                           _             _____    
#   |  \/  |                         (_)           |  __ \   
#   | \  / |   __ _   _ __    ___     _   _ __     | |__) |  
#   | |\/| |  / _` | | '_ \  / __|   | | | '_ \    |  _  /   
#   | |  | | | (_| | | |_) | \__ \   | | | | | |   | | \ \   
#   |_|  |_|  \__,_| | .__/  |___/   |_| |_| |_|   |_|  \_\  
#                    | |                                     
#                    |_|
#    ___     ___    __     _  _
#   |__ \   / _ \  |__ \  | || |                             
#      ) | | | | |    ) | | || |_                            
#     / /  | | | |   / /  |__   _|                           
#    / /_  | |_| |  / /_     | |                             
#   |____|  \___/  |____|    |_|                             

  # Introduction
  
# This guide shows some of the more common and/or easy ways to make maps in
# **R** using map-tile-based backgrounds with data plotted over the base maps.
# We introduce *coordinate reference systems*, *recommended packages* which
# allow tile-based map drawing, a few ways of *including user data*, and an
# Appendix on simple *coordinate conversion*.

#   The intention is to provide options to plot informative maps, which include the
# type of data collected in this class, relatively simply. We *will not* cover
# vector-based maps, importing GIS shapefiles, interpolation of point data,
# chloropleth maps, and many other map plotting methods.


### load packages needed to make maps

# ...also read some *data* we need for map annotations, make some *colour*
#   *palettes* to use later, and set any alternative Windows *fonts*:
  
library(sf)            # Simple Features spatial data in R
library(maptiles)      # get open-source map tiles for background maps
library(prettymapr)    # add scale bar and north arrows to maps
library(viridis)       # colourblind-friendly colour palettes
library(scico)         # more colourblind-friendly colour palettes
library(ggmap)         # plotting spatial data with ggplot syntax
library(ggsn)          # add scale and north to ggmap maps (doesn't always work)

# load maps data and do setups
git <- "https://raw.githubusercontent.com/Ratey-AtUWA/"
afr_map <- read.csv(file=paste0(git,"spatial/main/afr_map_v2.csv"), 
                    stringsAsFactors = TRUE)
places <- read.csv(file=paste0(git,"Learn-R/main/places.csv"),
                   stringsAsFactors = TRUE)
pal4lite <- c("black", viridis::plasma(8), "white")
pal4liteTransp <- c("black", 
                    scico::scico(8, alpha = 0.7, palette = "hawaii", beg=0.1),
                    "white")
pal4dark <- c("white", viridis::turbo(8, beg=0.2, end=0.9,dir=1), "black")

## Preparing to make maps

# First we read the data we need: from `.csv` files on a website, into an **R**
#   data frame:
  
# afs19 <- read.table('clipboard', header=T, sep="\t", stringsAsFactors = TRUE)
# alternative code reading data from a file:
afs19 <- read.csv(file=paste0(git,"Learn-R/main/afs19.csv"), 
                  stringsAsFactors = TRUE) # use this one
afs22 <- read.csv(file=paste0(git,"Learn-R/main/afs22S1.csv"), 
                  stringsAsFactors = TRUE)


## **Notes about maps** #####

# [distilled from the author instructions of a few relevant journals] 

# Include a **scale**, and **north arrow** (and where relevant, **co-ordinates** of latitude (°N, °S) and longitude (°W, °E); grid-based systems such as UTM are also ok, and can allow scale to be omitted if grid measurements are in metres).

# The use of colour unnecessarily is discouraged. 

# - All text must be large enough to be readable. Avoid making the lettering too large for the figure - this can result in a "cartoonish" appearance. 

# - Use a clear, sans serif typeface - the **R** default is Arial (Helvetica, Segoe UI, Source Sans Pro, Ubuntu, etc. are all OK).  

# Try to keep all text in a figure (including axis labels, contour labels, latitude and longitude, scale text, inset text, *etc.*) around the same size to aid reducibility and/or enlargement.

# Use a white background behind lettering that crosses a dark or textured area in a figure.

# Maps must show locations of any significant places/sample points, etc., mentioned in your report's text or tables. These might include cities, rivers, lakes, ponds, drains, landmarks, presumed sources of contamination, and so on. (These features are not always present on map tiles.)

# A country map is required for all studies, locating the study area. Adjacent countries must be located and named. [*Optional for this class*].


# Before we start downloading map data, we define and save some commonly-used
# coordinate reference systems which describe the map projection of our GPS
# data. The function `st_crs()` is from the `sf` package (see explanation
# below), and as the argument for the `st_crs()` function we use the EPSG code
# for the desired projection. See https://epsg.io for more information.

# define CRS, warning=FALSE, error=FALSE, results='hold'}
LongLat <- st_crs(4326) # uses Earth ellipsis specs from WGS84 datum
UTM50S <- st_crs(32750) # just for Zone 50, S hemisphere!

# We will use these coordinate reference system objects later. For now
# we're going to work in UTM coordinates.
# 
# Next we'll convert our data frames to *Simple Features* spatial data, using the 
# **R** package `sf` (Pebesma, 2018). Simple Features is a formal standard 
# (ISO 19125-1:2004) that describes how objects in the real world can be 
# represented in computers - see
# <https://r-spatial.github.io/sf/articles/sf1.html>.
# 
# We also make equivalents of these two sf-dataframes in the longitude-latitude
# coordinate system, using the `st_transform` function.

afs19utm <- st_as_sf(afs19[-9,], # omitting row 9 where coordinates are missing
                     coords = c("Easting","Northing"),
                     crs = UTM50S)
afs22utm <- st_as_sf(afs22, coords = c("Easting","Northing"),
                     crs = UTM50S)
afs19ll <- st_transform(afs19utm, crs = LongLat)
afs22ll <- st_transform(afs22utm, crs = LongLat)


## Alternative 1 - Maps in R using the maptiles package ####

### Defining the mapped area

# We define the area we need for our map and save it in a simple features object
# called `extent`.

# An easy way to get bounding coordinates (in longitude, latitude) is by using
# Google Maps or Google Earth. Google Earth allows the option to set the
# coordinate system to UTM. If we generated latitude-longitude coordinates, we
# would need to convert our *Simple Features* object (see the **Appendix**).
# If our input coordinates are Longitude-Latitude, note that south latitudes (and
# west longitudes) are negative, and we want decimal degrees rather than
# degrees-minutes-seconds. Also note that coordinates from Google Maps and Google
# Earth (except in saved `.kml` files) are in the order (Latitude, Longitude);
#i.e. (y,x), whereas (x,y) (Longitude, Latitude) seems to make more sense.

# For coordinates in the `sf` and `maptiles` packages, $x$ coordinates are
# commonly Eastings or Longitudes, and $y$ coordinates are commonly Northings or
# Latitudes.

# make map extent object, warning=FALSE, error=FALSE, results='hold'}
extent <- st_as_sf(data.frame(x=c(399800,400700),y=c(6467900,6468400)),
                   coords = c("x","y"), crs = UTM50S)

# **NOTE**: The projection we specify here will be the one the map plots in!

### Getting the map tile data

# We now need some functions from the `maptiles` package (Giraud 2021). We're
# using one of the OpenStreetMap tile options, but the following tile providers
# also work: 
# `OpenStreetMap, OpenStreetMap.HOT, Esri.WorldStreetMap, Esri.WorldTopoMap,
# Esri.WorldImagery, Esri.WorldGrayCanvas`, `CartoDB.Positron,
# CartoDB.DarkMatter, CartoDB.Voyager` (all CartoDB... tiles have variants which
# work), and `OpenTopoMap` 
# If you have an account with an API key, you can also use the tiles from
# https://www.thunderforest.com/ (Thunderforest 'hobby project' accounts are
# free). Sadly the Stamen tiles are no longer available.
# The option `crop = TRUE` is included to crop the tiles to our defined area in
# the `extent` object. If we leave it out, the map may change shape as it will
# use only square (uncropped) map tiles.

# get_tiles, warning=FALSE, error=FALSE, results='hold'}
# NOTE: projection of input object e.g. 'extent' sets map projection
aftiles <- maptiles::get_tiles(extent, provider = "OpenStreetMap.HOT", crop = TRUE)

### Plotting the map

# The `aftiles` object we created is a `SpatRaster` object which needs the
# `maptiles` (or `terra`) package loaded to be able to plot it - see 
# the code & output below.

par(oma=c(0,0,0,0), lend="square", xpd=TRUE)
plot_tiles(aftiles, adjust=F, axes=TRUE, mar=c(3,3,1,1)) # use axes = TRUE

mtext("Easting (UTM Zone 50, m)", side = 1, line = 3.2, font=2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 2.3, font=2)
```

The next chunk of code adds the `prettymapr` features shown in Figure 2. In this
code, `plotepsg = 32750` refers to the EPSG code for the UTM projection in Zone
50 (EPSG 32750), which we need to include so that the scale bar shows the
correct distances. (Long-Lat is EPSG 4326 in WGS84)

**NOTE**: If the `addscalebar` function does not work, run this line of code:
  # load-modified-addscalebar-function, eval=FALSE}
source("https://github.com/Ratey-AtUWA/Learn-R/raw/main/scalebar_use_sf_prettymapr.R")
```
(This will replace the function in the `prettymapr` package with a modified 
 version that uses the `sf` package for coordinate conversions instead of `sp` and `rgdal`.)

# . . . continuing previous code . . .
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)

# -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-
# **Map annotations using maptiles - Hints**
  
# Plotting in the `maptiles` package using the `plot_tiles()` function (which is
# actually done by the `plotRGB()` from the `terra` package) tries to set the
# plot margins based on the dimensions of the `SpatRaster` map object. You may
# need to adjust your settings; here are some tips:
  
# Set the `'base-R'` plot margins to zero using `mar=c(0,0,0,0)` in the `par()`  function, and set `xpd=TRUE` to allow plotting outside the maptiles plot area (which is often much smaller than you see on screen!). 

# use the options `axes=TRUE, mar=c(3,3,1,1)` in the `plot_tiles()` function (the actual size of the margins can be different, but this is where you need to set them)
    # adjust the height and width of the plot area to best match the map

# you may need to adjust the positions of the map annotations like axis titles, north arrow, and scale bar. 

# The position of the axis titles is adjusted with the `line=` option in the `mtext()` function.

# The positions of the north arrow and scale bar are adjusted with the `padin=` option in the `addnortharrow()` and `addscalebar()` functions.
# -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-
  
par(oma=c(0,0,0,0), lend="square", xpd=TRUE)
plot_tiles(aftiles, adjust=F, axes=TRUE, mar=c(3,3,0.5,0.5)) # use axes = TRUE

mtext("Easting (UTM Zone 50, m)", side = 1, line = 3.2, font=2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 2.3, font=2)
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)

### Adding our data and map annotations

Very often we would like to **add our own information to a map**, such
as the location of samples, often with different sizes, shapes, or
colours of symbols to represent numerical information.

# Since we have a map in UTM coordinates, we can now add plots of our data based
# on UTM locations (with a legend, of course - see code & output below. We can
# plot the points from the non-spatial data frame `afs19`, but here we `plot`
# the points from `afs19utm`, with the `add=TRUE` option. We add a legend to the
# plot in the usual way.

par(oma=c(0,0,0,0), lend="square", xpd=TRUE)
plot_tiles(aftiles, adjust=F, axes=TRUE, mar=c(3,3,0.5,0.5)) # use axes = TRUE

mtext("Easting (UTM Zone 50, m)", side = 1, line = 3.2, font=2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 2.3, font=2)
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)
clrz <- plasma(15)[6:15]
with(afs19utm, plot(geometry, add=TRUE,  
                    pch = rep(21:25,2)[Group], 
                    bg = clrz[Group]))
legend("bottomright", legend=levels(as.factor(afs19$Group)),
       pch = rep(21:25,2), pt.bg = clrz,
       title = "Group", inset = 0.02, ncol = 2)
```

# We can also add digitized map features such as wetland ponds, drains, etc., if
# these are not on the map tiles already. Ideally we would add these **before**
# plotting the data, to avoid the type of overplotting shown in the output of
# the code below.

par(oma=c(0,0,0,0), lend="square", xpd=TRUE)
plot_tiles(aftiles, adjust=F, axes=TRUE, mar=c(3,3,0.5,0.5)) # use axes = TRUE

mtext("Easting (UTM Zone 50, m)", side = 1, line = 3.2, font=2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 2.3, font=2)
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)
clrz <- plasma(15)[6:15]
with(afs19utm, plot(geometry, add=TRUE,  
                    pch = rep(21:25,2)[Group], 
                    bg = clrz[Group]))
legend("bottomright", legend=levels(as.factor(afs19$Group)),
       pch = rep(21:25,2), pt.bg = clrz,
       title = "Group", inset = 0.02, ncol = 2)
with(afr_map, 
     lines(drain_E, drain_N, col = "cadetblue", lwd = 2))
with(afr_map, lines(wetland_E, wetland_N, col = "cadetblue", 
                    lwd = 1, lty = 1))

# Finally, we would most likely want to add some text. Text labels should also
# be added *before* plotting the data. The final map is shown in the output of
# the code below.

par(oma=c(0,0,0,0), lend="square", xpd=TRUE)
plot_tiles(aftiles, adjust=F, axes=TRUE, mar=c(3,3,0.5,0.5)) # use axes = TRUE

mtext("Easting (UTM Zone 50, m)", side = 1, line = 3.2, font=2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 2.3, font=2)
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)
clrz <- plasma(15)[6:15]
with(afs19utm, plot(geometry, add=TRUE,  
                    pch = rep(21:25,2)[Group], 
                    bg = clrz[Group]))
legend("bottomright", legend=levels(as.factor(afs19$Group)),
       pch = rep(21:25,2), pt.bg = clrz,
       title = "Group", inset = 0.02, ncol = 2)
with(afr_map, 
     lines(drain_E, drain_N, col = "cadetblue", lwd = 2))
with(afr_map, lines(wetland_E, wetland_N, col = "cadetblue", 
                    lwd = 1, lty = 1))
text(c(400263, 399962, 400047), c(6468174, 6468083, 6468237),
     labels = c("Chapman Drain","Kitchener Drain", "Woolcock Drain"),
     pos = c(2,2,4), cex = 0.8, font = 3, col = "cadetblue")
```

### Making a bubble map ####

# We use essentially the same code as for the maps above (except plotting the
# annotations and data in the correct order!). Then we use the `symbols()`
# function to make the 'bubbles', making sure that we include the options `add =
# TRUE` so we overplot the map, and `inches = FALSE` so we can manually scale
# the bubbles. The factor of `0.4` used to scale the circles in the code for
# bubbles and legend is *found by trial-and-error* (it *is* possible to write a
# simple algorithm to estimate a scaling factor from the data). A map like that
# shown in the output of the code below is a good exploratory data analysis
# tool, as even without the legend it shows any unevenness in concentrations,
# including where high concentrations are located.

par(oma=c(0,0,0,0), lend="square", xpd=TRUE)
plot_tiles(aftiles, adjust=F, axes=TRUE, mar=c(3,3,0.5,0.5)) # use axes = TRUE

mtext("Easting (UTM Zone 50, m)", side = 1, line = 3.2, font=2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 2.3, font=2)
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)
text(c(400225, 399955), c(6468190, 6468083),
     labels = c("Chapman\nDrain","Kitchener\nDrain"),
     pos = c(1,2,4), cex = 0.8, font = 3, col = "cadetblue")
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
symbols(c(400600,400600),c(6468040,6467980), circles=0.4*sqrt(c(bublo,bubhi)), add=T,
        lwd=1, inches=F, fg = "purple", bg = "#8000FF40")
text(c(400600,400620,400620),c(6468100,6468040,6467980), 
     labels=c("Zn (mg/kg)",bublo,bubhi), cex=0.85, pos = c(1,4,4))
```

### Categorized (e.g. percentile) bubble map ####

# We make a new column in our data frame by cutting the measurement of interest,
# in this example **Zn**, into percentiles. The new column called `QZn` is a
# factor which identifies which percentile of Zn concentration each sample is
# in. We then use this factor to define symbols, sizes, and colours for each
# sample location. We add a line break to text labels using \n.

par(oma=c(0,0,0,0), lend="square", xpd=TRUE)
plot_tiles(aftiles, adjust=F, axes=TRUE, mar=c(3,3,0.5,0.5)) # use axes = TRUE

mtext("Easting (UTM Zone 50, m)", side = 1, line = 3.2, font=2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 2.3, font=2)
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)
with(afr_map[afr_map$Drain!="Chapman_St",], lines(drain_E, drain_N, col = "slategray3", lwd = 2))
with(afr_map, polygon(wetland_E, wetland_N, col = "cadetblue2", 
                      border = "cadetblue3"))
text(c(400225, 399955, 400047), c(6468190, 6468083, 6468237),
     labels = c("Chapman\nDrain","Kitchener\nDrain", "Woolcock\nDrain"),
     pos = c(1,2,4), cex = 0.8, font = 3, col = "cadetblue")

# percentile plot
afs19$QZn <- cut(afs19$Zn, 
                 quantile(afs19$Zn, 
                      p=c(0,0.02,0.05,0.25,0.5,0.75,0.95,0.98,1), 
                      na.rm=T), labels=c("Q0-02","Q02-05","Q05-25","Q25-50",
                                         "Q50-75","Q75-95","Q95-98","Q98-max"))
palette(pal4liteTransp) # use colours with semi-transparency (defined near top)
# use percentile factor column to categorize points
with(afs19, 
     points(Easting, Northing, 
            pch = c(22,22,22,3,4,21,21,21)[QZn], 
            col = c(1,1,1,4,5,1,1,1)[QZn], bg = c(9:2)[QZn],
            lwd = c(1,1,1,2,2,1,1)[QZn], 
            cex = c(0.7,0.9,1.1,1.3,1.3,2,3,4)[QZn])
)
legend("bottomright", legend = levels(afs19$QZn), title="Zn",
       pch = c(22,22,22,3,4,21,21,21), col = c(1,1,1,4,5,1,1,1),
       pt.lwd = c(1,1,1,2,2,1,1), pt.bg = c(9:2 ), 
       pt.cex = c(0.7,0.9,1.1,1.3,1.3,2,3,4),
       bty = "n", inset = c(0.01,0.1), cex = 0.85, y.intersp = 1.3)
palette(pal4lite) # change back to non-transparent palette (optional)
afs19$QZn <- NULL # to delete quantile column (optional; you can leave it in)


# The resulting percentile bubble map we just made adds value to the 'standard'
# bubble map made above, as it adds some statistical meaning to the bubble
# sizes. A similar map could be created by using the Tukey boxplot thresholds
# instead of percentiles which could show potential outliers (i.e. using the
# `boxplot.stats()` function to generate categories instead of the `quantile()`
# function.)


#-#-#-#-#-#-#-#-#-#-#-#-# **Warning**: #-#-#-#-#-#-#-#-#-#-#-#-# 
# semi-transparent colours (i.e. alpha < 1) are not supported by metafiles in R.
# To use semi-transparent colours, save as `.png` or `.tiff`, or copy as a
# bitmap.

# -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-

## Alternative 2 - Maps in R using the ggmap package ####
  
# The `ggmap` package (Kahle and Wickham 2013) is an extension of `ggplot`, so
# it's easier to use if you are familiar with `ggplot` and the associated family
# of packages. [*You will need a Google maps API key which you can get by
# registering at https://developers.google.com/maps ]

# get-api-key
secret <- read.csv("gmapskey.csv")

# First we make a `ggmap` object, locating the map by its central coordinate
# with the extent controlled by the `zoom` option:

library(ggmap)
register_google(key = secret[1,1]) # you would replace secret[1,1] with your API key
udubua.gg <- get_googlemap(center=c(115.8213,-31.98165), 
                           zoom = 16, maptype = "roadmap", color = "bw")
rm(secret)

# This always gives a **square** map as you'll notice - which we might
# not always want. In theory, the map aspect ratio can be changed using the
# `size` argument in the `get_googlemap` function, but this does not work
# reliably. We recommend leaving the map dimensions and aspect ratio at their
# default values.

# Next we plot the ggmap object using ggplot grammar. It's possible to just plot
# the object (i.e. run `ggmap(udubua.gg)`), but it's good to have more control
# over plot options **and** to plot some data over the base map. In the example
# below, we use the aesthetic in `geom_point()` to plot
# different categories with different shapes and colours, with the categories
# defined by the factor `Type`. A range of map styles is available by selecting
# a combination of one of `maptype = "terrain", "satellite", "roadmap"`, or
# `"hybrid"`, together with `color = "color"` or `"bw"`. **Note**: the `ggsm`
# package is designed to add scale bar and north arrow to ggmaps, but it seems
# buggy and we currently don't recommend it - the north arrow seems OK though.

ggmap(udubua.gg) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_text(aes(x = 115.825, y = -31.98, label = "Swan\nRiver",
                fontface = "italic", family="sans"), 
            size = 4, vjust = 0, hjust = 0, color="gray40") + 
  geom_point(aes(x = Longitude, y = Latitude, col=Type, shape=Type), 
             data = places, size=3) +
  theme(axis.text=element_text(size=9, color="black"),
        axis.title=element_text(size=11,face="bold"))

# There are numerous possibilities with `ggmap` and the features made possible
# by `ggplot2` that are not illustrated by our map.
# For example, we could use `size` as an aesthetic (i.e. include within
# `aes(...)` with `size = variableName`), to generate something like a bubble
# map.

### Other map types using ggmap

# Another option *apparently* available in `ggmap` are some of the *Stamen* map
# tiles, accessible with the `get_stamenmap()` function. **However**, the Stamen
# URLs have been updated, and this function is no longer able to download Stamen
# map tiles.

# The next example uses the simple features information in one of the data
# frames we made at the beginning. The `sf` package introduces a new 'geom',
# `geom_sf()` for plotting in `ggmap.` We need to use the option `inherit.aes =
# FALSE` in `geom_sf()`, to override the default aesthetics from the `ggmap`
# object. We also add `coord_sf` to ensure that all layers use a common CRS
# (Coordinate Reference System). 
# Run `help(geom_sf)` for more information.

# Plotting using `geom_sf()` when the CRS is in degrees adds, by default, a
# °S/°W/°N/°E suffix to the axis values. In the code below we suppress this
# using `scale_x_continuous(label = I)` (same for the y axis; the `limits` and
# `expand` options stop the manual scale adding space around the map tiles). We
# include the information on units and hemisphere in the axis titles.

afr.gg <- get_googlemap(center=c(lon=115.9445, lat = -31.918), 
                        zoom=17, col = "bw", maptype = "terrain")
bb <- as.numeric(unlist(as.vector(attr(afr.gg, "bb"))))
ggmap(afr.gg) + 
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)") + 
  geom_text(aes(x = 115.944, y = -31.918, label = "Ashfield\nFlats",
                fontface = "italic", family="sans"), 
            size = 4, color="gray65", lineheight=0.8) +
  geom_text(aes(x = 115.942, y = -31.9199, label = "Swan River",
                fontface="italic", family="sans"), size=4, color="gray65") +
  geom_path(aes(x = drain_lon, y=drain_lat), data=afr_map, 
            color = "slategray2", size = 1.25) +
  geom_polygon(aes(x=wetland_lon, y=wetland_lat), data = afr_map,
               color = "slategray", fill="azure3") + 
  geom_sf(data = afs21ll, aes(bg=Zn, size=Zn), shape=21, inherit.aes=FALSE) +
  scale_x_continuous(labels = I, limits=c(bb[2],bb[4]), 
                     expand = expansion(mult=c(0,0))) +
  scale_y_continuous(labels = I, limits=c(bb[1],bb[3]), 
                     expand = expansion(mult=c(0,0))) +
  scale_fill_viridis_c(alpha = 0.7) + 
  scale_size_area(breaks = c(30,100,300,1000,3000,5000), max_size = 9) +
  theme_bw() +
  theme(axis.text=element_text(size=9, color="black"),
        axis.title = element_text(size = 12, face = "bold")) +
  coord_sf(crs = st_crs(4326))

  
  ## Other tile-based mapping packages in R ####
  
  The `rosm` package (Dunnington 2022) allows users to produce background maps
from several map tile providers. **We don't currently recommend** `rosm`, since
it's difficult when using this package to produce axes in commonly-used
coordinate reference systems.

The `OpenStreetMap` R package (Fellows, 2019) can make very good tile-based
maps. Unfortunately, however, it can be difficult to use on Apple Mac computers,
and there can also be problems with Windows-based systems due to the use of
`Java` code in the package. So, out of respect for MacOS users, we are 
**not recommending the** `OpenStreetMap` package either.

## Final Words ####

# We recommend using either the `maptiles` or `ggmap` packages to draw maps with
# tiled backgrounds, as they allow use of the state-of-the-art *simple features*
# system *via* the `sf` package.

## Appendix - coordinate conversions ####
  
#   **Converting UTM to LongLat**
#   
# Make a simple features (package `sf`) object containing the UTM coordinates
# 
# Example uses explicit values (as used previously to generate the `maptiles`
# map), but the coordinates could also be obtained from a data frame - edit to
# suit!
  
utm.temp <- st_as_sf(data.frame(x=c(399800,400700),y=c(6467900,6468400)),
                     coords = c("x","y"), crs = UTM50S)

# We then use the `st_transform()` function from the `sf` package to convert to
# long-lat (or another projection), which results in another spatial object:
  
longlat.temp <- st_transform(utm.temp, crs = LongLat)

# We now have two `sf` spatial objects which we can use (for instance) to
# define the map extent for a `maptiles` map:
  
require(sf); require(maptiles) # load packages if not done already
# using the utm object
tiles_utm <- get_tiles(utm.temp, provider = "OpenStreetMap", crop = TRUE)
# using the longitude-latitude object
tiles_longlat <- get_tiles(longlat.temp, provider = "OpenStreetMap",
                           crop = TRUE)
# . . . and so on

# To extract coordinates from a data frame, for example: <br>
#   (**NOTE** - missing coordinates are not allowed!)

afs19 <- afs19[-which(is.na(afs19[,c("Easting")])==1),] # remove rows with NAs
afs19utm <-  st_as_sf(afs19, coords = c("Easting","Northing"), crs = UTM50S)

# We then use the `st_transform()` function from the `sf` package to
# convert to longitude-latitude, which results in another spatial data frame:
  
afs19ll <- st_transform(afs19utm, crs = LongLat)

# To extract just the coordinate values in non-spatial form, we use the function 
# `st_coordinates()`:
  
# extract sf coordinates to console (can assign to object of class "matrix")
st_coordinates(afs19utm)

# extract sf coordinates to data frame
longlat.temp <- as.data.frame(st_coordinates(afs19ll))
colnames(longlat.temp) <- c("Longitude","Latitude")


  ## References and R packages
  
#   Dunnington, Dewey (2017). prettymapr: Scale Bar, North Arrow, and Pretty
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
# Garnier S, Ross N, Rudis R, Camargo AP, Sciaini M, Scherer C (2021). *Rvision - Colorblind-Friendly Color Maps for R*. R package version 0.6.2. (**viridis**)
# 
# Pebesma, E., 2018. Simple Features for R: Standardized Support for
# Spatial VectorData. *The R Journal* **10** (1), 439-446,
# <https://doi.org/10.32614/RJ-2018-009>. (package **sf**)
# 
# Pebesma, E.J., R.S. Bivand, 2005. Classes and methods for spatial data
# in R. *R News* **5** (2), <https://cran.r-project.org/doc/Rnews/>.
# (package **sp**)