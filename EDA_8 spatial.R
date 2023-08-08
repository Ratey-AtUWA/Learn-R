#### -+-+-+-+- EDA WORKFLOW 8: Spatial patterns -+-+-+-+- ####
#              ___  ____ ____ ___    ____ _ ____ _  _ ___                  
#              |__] |__| |__/  |     |___ | | __ |__|  |                   
#              |    |  | |  \  |     |___ | |__] |  |  |                   
#                                                                          
#  ____ ___  ____ ___ _ ____ _       ___  ____ ___ ___ ____ ____ _  _ ____ 
#  [__  |__] |__|  |  | |__| |       |__] |__|  |   |  |___ |__/ |\ | [__  
#  ___] |    |  |  |  | |  | |___    |    |  |  |   |  |___ |  \ | \| ___] 
# 
# Regression against distance ####

# In Part 4 of this EDA Workflow series we created new variables 
# based on distance from a point or linear source. As shown in the 
# examples in Part 4, we can use these new variables as predictors 
# in regression models.

# Here's an example to remind us:
# a reminder of the sampling layout...
with(afs20, plot(Easting, Northing, asp = 1,
                 pch=levels(afs20$Zone)[afs20$Zone],
                 col="grey", cex=0.3))
with(afs20, text(Easting, Northing,
                 labels = Zone, font=3, cex = 0.7,
                 col = c(2,4,6)[Zone]))
lines(c(400107,400355), c(6468020,6468279), lwd = 4, col = "lightskyblue")
text(400250,6468155,labels = "Chapman Street\nMain Drain", pos=4, srt=45, col = "skyblue2")

# ...and then set up a regression based on distance
afs20N <- subset(afs20, Zone=="N")
# Assuming a single input point at 400306 E, 6468230 N. 
points(400306, 6468230, pch = 9, lwd =2, cex = 1.5)
afs20N$dist <- with(afs20N,
                    sqrt((Easting - 400306)^2 + 
                           (Northing - 6468230)^2)
                    )

par(mfrow = c(1,1), mar = c(3,3,1,1), mgp = c(1.6,0.3,0), tcl = 0.3,
    lend = "square", ljoin = "mitre")
palette("default")
with(afs20N,
     plot(Cu ~ dist, pch = 3, lwd = 2, cex = 1.5, col = 4,
          xlab = "Distance from input (m)", ylab = "Cu (mg/kg)")
     )
abline(lm(afs20N$Cu ~ afs20N$dist), col = 8)
summary(lm(afs20N$Cu ~ afs20N$dist))
rm(afs20N)

# Mapping Spatial Patterns ####

# To display spatial patterns in map form, we need a base map.
# To avoid any issue with specific packages (e.g Java problems
# using the OpenStreetMap package), several options are given:
# (1) OpenStreetMap (2) base R map using digitised data, 
# (3) the 'rosm' package, and (4) the 'ggmap' R package. 
# Different representations of spatial data are used in 
# each example that follows.

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Maps using OpenStreetMap ####
library(OpenStreetMap)
library(prettymapr)

# Make a UTM projection map
af_map.osm <- openmap(upperLeft = c(-31.9139,115.9404),
                    lowerRight = c(-31.9203, 115.9504),
                    type = "osm-public-transport",
                    zoom=17)
af_map.utm <- openproj(af_map.osm, 
                     projection = "+proj=utm +zone=50 +south")
rm(af_map.osm)

# Draw the UTM map and include some data
par(mar = c(3,3,1,1), mgp = c(1.6,0.3,0), tcl = 0.3,
    lend = "square", ljoin = "mitre")
plot(af_map.utm, removeMargin = FALSE)
axis(1)
mtext("Easting (UTM Zone 50, m)", 
      side = 1, line = 1.6, font = 2)
axis(2)
mtext("Northing (UTM Zone 50, m)", 
      side = 2, line = 1.6, font = 2)
addnortharrow()
addscalebar(plotepsg = 32750, 
            htin = 0.15, label.cex = 1.4)
box()

# The variable is categorized using the thresholds in the
# Tukey box plot:
# (we use the transformed variable to make the thresholds,
# then back-transform to display the original variable)
# We can quite easily search-and-replace (case-sensitive is 
# best) to change the variable!
cuts5 <- signif(fivenum(afs20$Zn.log, na.rm = TRUE),4)
cuts5[5] <- cuts5[4]+(1.5*(cuts5[4]-cuts5[2]))
cuts5[1] <- cuts5[2]-(1.5*(cuts5[4]-cuts5[2]))
cuts5 <- signif(10^cuts5, 3) # back-transform
palette(c("black","purple","blue3","darkolivegreen4",
          "goldenrod3","tomato","red2","white"))
sf <- 0.85 # adjustable scale factor for plot text & symbols
cex0 <- c(0.85,0.9,1.05,1.2,1.4,1.8)*sf
points(afs20$Northing ~ afs20$Easting, pch=15, 
       col=2, cex=cex0[1], lwd=2,
       subset=afs20$Zn<cuts5[1])
points(afs20$Northing ~ afs20$Easting, pch=0, col=3, cex=cex0[2], lwd=2,
       subset=afs20$Zn>cuts5[1]&afs20$Zn<cuts5[2])
points(afs20$Northing ~ afs20$Easting, pch=3, col=4, cex=cex0[3], lwd=2,
       subset=afs20$Zn>cuts5[2]&afs20$Zn<cuts5[3])
points(afs20$Northing ~ afs20$Easting, pch=4, col=5, cex=cex0[4], lwd=2,
       subset=afs20$Zn>cuts5[3]&afs20$Zn<cuts5[4])
points(afs20$Northing ~ afs20$Easting, pch=1, col=6, cex=cex0[5], lwd=2,
       subset=afs20$Zn>cuts5[4]&afs20$Zn<cuts5[5])
points(afs20$Northing ~ afs20$Easting, pch=16, col=7, cex=cex0[6], lwd=2,
       subset=afs20$Zn>cuts5[5])
min0 <- signif(min(afs20$Zn, na.rm=T),4)
max0 <- signif(max(afs20$Zn, na.rm=T),4)
legend("topleft", bty="o", pch=c(15,0,3,4,1,19), inset=0.025, 
       cex=1 * sf, pt.cex=cex0, pt.lwd=2, y.intersp=1.25, 
       title="Zn by Tukey boxplot thresholds (range):", 
       legend=c(paste0("Lower outliers (",min0,"-",cuts5[1]," mg/kg)"),
                paste0("Below Q1 (",cuts5[1],"-",cuts5[2]," mg/kg)"),
                paste0("Q1 to median (",cuts5[2],"-",cuts5[3]," mg/kg)"),
                paste0("Median to Q3 (",cuts5[3],"-",cuts5[4]," mg/kg)"),
                paste0("Above Q3 (",cuts5[4],"-",cuts5[5]," mg/kg)"),
                paste0("Upper outliers (",cuts5[5],"-",max0," mg/kg)")), 
       col=seq(2,7))
rm(list = c("sf","cex0","cuts5","min0","max0"))

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Map using base R and coordinate data ####

afr_map <- read.csv("ashfield_map.csv")
require(prettymapr)
par(mar = c(3,3,1,1.5), mgp = c(1.6, 0.3, 0), 
    tcl = 0.3, ljoin = "mitre", lend = "square")
# change values of xlim, ylim to suit desired map area
with(afr_map, plot(street_E, street_N, asp=1,
          type = "l", lwd = 6, col = "grey90",
          xlab = "", ylab = "", 
          xlim=c(399910,400670), 
          ylim=c(6467940,6468540)))
mtext("Easting (UTM Zone 50, m)", side=1, 
      line=1.6, cex=1.2, font=2) 
mtext("Northing (UTM Zone 50, m)", side=2, 
      line=1.6, cex=1.2, font=2) 
with(afr_map, polygon(veg_E, veg_N, 
             border="darkseagreen",
             col = "darkseagreen", 
             lwd=1))
with(afr_map, lines(bound_E, bound_N, type="l", 
           col = "darkseagreen", 
           lty = 5, lwd = 2))
with(afr_map, lines(drain_E, drain_N, 
                    col="royalblue4", lwd=2))
with(afr_map, polygon(wetland_E, wetland_N, 
             border="darkcyan",
             col = "lightblue2", lwd=1))
with(afr_map, polygon(swan_E, swan_N, 
             border="transparent",
             col = "#00008040", lwd=1))
with(afr_map, lines(path_E, path_N, 
                    col="ivory4", lwd=2, lty=3))
text(400200, 6467820, labels = "Swan River", 
     col = "royalblue4", font = 3, cex = 1.2, 
     srt = 330)
text(400200, 6468135, labels = "Chapman St Drain", 
     col = "royalblue4", srt = 45)
text(399970, 6468100, labels = "Kitchener St Drain", 
     col = "royalblue4", srt = 300)
text(399900, 6468230, labels = "Hardy Road", 
     col = "grey65", srt = 45)
text(400540, 6468490, labels = "Iveson Place", 
     col = "grey65", srt = 50)
text(400450, 6468180, labels = "Ashfield Flats\nReserve", 
     col = "darkolivegreen", font = 3, cex = 1.2)
addnortharrow(padin = c(0.2,2))
addscalebar(plotepsg=32750, htin = 0.15, label.cex = 1.2)
box()

# plot the 'bubbles'
sf <- 20
with(afw20, 
     symbols(Easting, Northing, add = TRUE, 
             circles = sqrt(P)*sf,
             inches = FALSE, fg = "red2", bg = "#C0000080")
     )
# make a bubble legend
bublo <- pretty(afw20$P)[2]/2
bubhi <- pretty(afw20$P)[length(pretty(afw20$P))]
symbols(c(399950,399950), c(6468500,6468450), add = TRUE, 
        circles = sqrt(c(bublo, bubhi))*sf,
        inches = FALSE, fg = "red2", bg = "#C0000080")
text(c(399950,399970,399970), c(6468520,6468500,6468450),
     labels = c("P (mg/L)", bublo, bubhi), pos = c(3,4,4))

# using the rosm package to make maps ####
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#### base maps using rosm and prettymapr ####

# Maps in rosm need Long-Lat coordinates
# The packages 'sp' and 'PBSmapping' have useful functions for
# converting coordinates (e.g. between Long-Lat and UTM)
# (see APPENDIX at end)

# For more information, run   help(package="rosm") 

library(rosm)
# boundaries in makebbox(N, E, S, W)
af.rosm <- makebbox(-31.9139,115.9504,-31.9203,115.9404)
# see osm.types() or bmaps.types() for type options
osm.plot(af.rosm, type="hotstyle", zoomin=0)
osm.points(afw20$Longitude,afw20$Latitude, 
           col="red3", lwd=2, pch=3)

# use osm.plot() or bmaps.plot() function embedded in 
# prettymap() function:
# find choices for type= using osm.types() and bmaps.types() 
#   (some choices need API keys)
prettymap({
  osm.plot(af.rosm, type="hotstyle", zoomin = -1)
  osm.points(afw20$Longitude,afw20$Latitude, 
             col=c(1,2,4,6,7)[afw20$Zone], lwd=2, 
             pch=c(19,15,17,3,0)[afw20$Zone], cex=1.)
  osm.text(115.941, -31.919, 
           labels = "Kitchener\nDrain", 
           font = 3, col = "darkcyan")
  osm.text(115.9465, -31.9175, 
           labels = "Chapman Street\nDrain", 
           pos=4, font = 3, col = "darkgreen")
  },
  oma = c(3,3,1,1), drawarrow = T, arrow.scale = 1.,
  scale.htin = 0.15, scale.label.cex = 1.
)
# legend(par("usr")[1] + 0.75*(par("usr")[2]-par("usr")[1]),
#        par("usr")[3] + 0.45*(par("usr")[4]-par("usr")[3]), 
legend(12907300, -3752480, 
       legend = levels(afw20$Zone), cex = 0.85,
       pch = c(19,15,17,3,0), col = c(1,2,4,6,7))
box()

# map axes are weird in rosm because the internal coordinates are
# 'web mercator'. So we need to do some manipulation with the
# projectMercator() function from the OpenStreetMap package...
# (we could also use the 'sp' package)
require(OpenStreetMap)
xtix <- projectMercator(rep(af.rosm["y","min"],
                            length(pretty(af.rosm[1,])[-1])),
                        pretty(af.rosm[1,])[-1])
axis(1, cex.axis = 1., at = xtix[,1], 
     labels = round(pretty(af.rosm[1,])[-1],3))
mtext("Longitude (\u00B0E)", 1, 1.7, font = 2, cex = 1.2)
ytix <- projectMercator(pretty(af.rosm[2,]),
                        rep(af.rosm["x","min"], length(pretty(af.rosm[2,]))))
axis(2, cex.axis = 1., at = ytix[2:(length(pretty(af.rosm[2,]))-1),2], 
     labels = round(pretty(af.rosm[2,])[2:(length(pretty(af.rosm[2,]))-1)],3))
mtext("Latitude (\u00B0S)", 2, 1.7, font = 2, cex = 1.2)
rm(list = c("xtix", "ytix"))

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Bubble Maps in the ggmap R Package ####

# Maps in ggmap need Long-Lat coordinates
# These have been added to the Google Sheet at
# https://docs.google.com/spreadsheets/d/1e4M5OIs7Gedqv7vFIQaqt4LmfCDnWoDTEyMLpvVFHiM/edit?usp=sharing
# Otherwise the package 'PBSmapping' has useful functions for
# converting coordinates between Long-Lat and UTM
# (see APPENDIX at end)

library(ggmap)
library(ggsn)
# make a map
# this is my Google Maps API key
register_google(key = "AIzaSyDU7QiTWE4RGFFQNmhWy51n7e4RBeHKjc0")
af_gg <- get_googlemap(center = c(lon = 115.9445, lat = -31.9182),
                       maptype = "hybrid", 
                       zoom = 16, scale = 2)

ggmap(af_gg) + 
  geom_point(data = afw20, aes(x=Longitude, y=Latitude, size = EC), 
             shape = 19, stroke=2, col = "cyan", alpha=0.7) 

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- #
#  ____ ___  ___  ____ _  _ ___  _ _  _ 
#  |__| |__] |__] |___ |\ | |  \ |  \/  
#  |  | |    |    |___ | \| |__/ | _/\_ 
#
## APPENDIX: Coordinate conversion ####

require(PBSmapping)
# examples of coordinate conversions
# (read help entries for PBSmapping after installing)
TEMP.utm <- as.matrix(cbind(afw20$Easting, afw20$Northing))
colnames(TEMP.utm) <- c("X","Y")
attr(TEMP.utm, "projection") <- "UTM"
attr(TEMP.utm, "zone") <- 50
TEMP.LL <- convUL(TEMP.utm, km=FALSE, southern=TRUE)
afw20$Longitude <- TEMP.LL[,1]
afw20$Latitude <- TEMP.LL[,2]
afw20[,c("Longitude","Latitude")]

# subset data to not include NA coordinates
afs20temp <- subset(afs20, afs20$Easting > 0 & afs20$Northing > 0) 
TEMP.utm=as.matrix(cbind(afs20temp$Easting, afs20temp$Northing))
colnames(TEMP.utm) <- c("X","Y")
attr(TEMP.utm, "projection") <- "UTM"
attr(TEMP.utm, "zone") <- 50
TEMP.LL <- convUL(TEMP.utm, km=FALSE, southern=TRUE)
afs20temp$Longitude <- TEMP.LL[,1]
afs20temp$Latitude <- TEMP.LL[,2]
print(afs20temp[,c("Longitude","Latitude")], digits = 9)

rm(list = c("TEMP.utm", "TEMP.LL"))

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# or use "sp" & "rgdal" packages to convert coordinates,
# for example: (see help(package = "sp"))
require(rgdal)
require(sp)
# first subset the data since there are missing coordinates!
afs20temp <- subset(afs20, afs20$Easting > 0 & afs20$Northing > 0) 
d <- SpatialPoints(cbind(afs20temp[,c("Easting","Northing")]), 
                     proj4string = CRS("+init=epsg:32750"))
# proj4string(d) <- CRS("+init=epsg:32750") # UTM Zone 50, WGS 84
# define new coordinate reference system to be (for example) web mercator
CRS.osm <- CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs")
d.WM <- spTransform(d, CRS.osm)
# display the converted coordinates
print(unclass(d.WM))
# include converted coordinates in our data subset
afs20temp$webMerc_E <- d.WM@coords[,1]
afs20temp$webMerc_N <- d.WM@coords[,2]


# to replot rosm maps with UTM axes, ####
# we can use this type of conversion...
require(sp)
# define coordinate reference systems to be used
webMercator <- CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +units=m +no_defs")
UTM <- CRS("+proj=utm +zone=50 +south")
# get axis data from existing plot, where
# par("usr") contains axis limits for [map] plot
osm.plot(af.rosm)
xybox <- 
  SpatialPoints(rbind(c(par("usr")[1],par("usr")[4]),
                      c(par("usr")[2],par("usr")[3])),
                proj4string = webMercator)

xyboxUTM <- spTransform(xybox, UTM)
newUTM <- xyboxUTM@coords

xUTM <- cbind(pretty(xyboxUTM@coords[,1], n=8),
              rep(xyboxUTM@coords[1,2], 
                  length(pretty(xyboxUTM@coords[,1], n=8))))
row.names(xUTM) <- NULL
xUTM <- SpatialPoints(xUTM, proj4string = UTM)
yUTM <- cbind(rep(xyboxUTM@coords[1,1], 
                  length(pretty(xyboxUTM@coords[,2]))),
              pretty(xyboxUTM@coords[,2]))
row.names(yUTM) <- NULL
yUTM <- SpatialPoints(yUTM, proj4string = UTM)

# transform the coordinates
xWM <- spTransform(xUTM, webMercator)
yWM <- spTransform(yUTM, webMercator)

par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), 
    lend=2, ljoin=1, tcl=0.25)
palette(c("black","red3","forestgreen","blue","darkcyan",
          "darkmagenta","gold3","sienna","grey50","white"))
osm.plot(af.rosm, type="osm", zoomin=0)
with(afs20temp, osm.points(Longitude, Latitude, 
                       col=c(2,4,6)[Zone], 
                       pch=c(1,2,3)[Zone], 
                       lwd=2, cex=1.5))
legend("bottomleft", inset=0.02, bg = "grey92",
       legend=levels(afs20temp$Zone),
       title = "Sampling Zone",
       col=c(2,4,6), pch=c(1,2,3), 
       pt.lwd=2, pt.cex=1.5)
axis(1, cex.axis = 1.2, 
     at = xWM@coords[,1], 
     labels = as.numeric(xUTM@coords[,1]))
mtext("Easting (m, UTM Zone 50)", 1, 1.7, font = 2, cex = 1.5)
axis(2, cex.axis = 1.2, 
     at = yWM@coords[,2], labels = yUTM@coords[,2])
mtext("Northing (m, UTM Zone 50)", 2, 1.7, font = 2, cex = 1.5)
# need to use this code structure to plot isolated subsets
afs20tempN <- subset(afs20temp, Zone=="N")
with(afs20tempN, osm.points(Longitude, Latitude, pch=3, lwd=2, cex=1.5))
rm(afs20tempN)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ASCII big lettering from https://patorjk.com/software/taag/
# using the Cybermedium font