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
library(sf)
library(maptiles)
library(prettymapr)

# Make a UTM projection map
af.extent <- st_as_sf(data.frame(x=c(399900,400600),y=c(6467900,6468400)),
                      coords = c("x","y"), crs=st_crs(32750))
af_map.utm <- get_tiles(af.extent,
                    provider = "OpenStreetMap.HOT",
                    zoom=17, crop=TRUE)

# Draw the UTM map and include some data
par(mar = c(3,3,1,1), mgp = c(1.6,0.3,0), tcl = 0.3,
    lend = "square", ljoin = "mitre", xpd=T, las=0)
plot_tiles(af_map.utm, mar=c(3,3,1,1), adjust = FALSE, axes=TRUE)
mtext("Easting (UTM Zone 50, m)", 
      side = 1, line = 1.6, font = 2)
mtext("Northing (UTM Zone 50, m)", 
      side = 2, line = 1.6, font = 2)
addnortharrow()
addscalebar(plotepsg = 32750, 
            htin = 0.15, label.cex = 1.4)

# The variable is categorized using the thresholds in the
# Tukey box plot:
# (we use the transformed variable to make the thresholds,
# then back-transform to display the original variable)
# We can quite easily search-and-replace (case-sensitive is 
# best) to change the variable!
afs20$Zn.log <- log10(afs20$Zn)
bpstats <- boxplot.stats(afs20$Zn.log)
cuts5 <- signif(10^bpstats$stats, 3) # back-transform

# make new column based on cuts
afs20$Zn_cuts <- cut(afs20$Zn, breaks = c(0,cuts5,999999))

palette(c("black","#a000c0a0","#0000c0a0","#008080a0",
          "#c0a000a0","#e0800060","#e00000a0","white"))
s0 <- 1 # adjustable scale factor for plot text & symbols
cex0 <- c(0.7,0.9,1,1.4,1.8,2.4)*s0
with(afs20, points(Northing ~ Easting, pch=c(15,15,0,1,19,19)[Zn_cuts], 
       col=c(2,3,4,5,6,7)[Zn_cuts], cex=cex0[Zn_cuts], lwd=2))
min0 <- signif(min(afs20$Zn, na.rm=T),4)
max0 <- signif(max(afs20$Zn, na.rm=T),4)
legend("topleft", bty="o", pch=c(15,15,0,1,19,19), inset=0.025, 
       cex=1 * s0, pt.cex=cex0, pt.lwd=2, y.intersp=1.25, 
       title="Zn by Tukey boxplot thresholds (range):", 
       legend=c(paste0("Lower outliers (",min0,"-",cuts5[1]," mg/kg)"),
                paste0("Below Q1 (",cuts5[1],"-",cuts5[2]," mg/kg)"),
                paste0("Q1 to median (",cuts5[2],"-",cuts5[3]," mg/kg)"),
                paste0("Median to Q3 (",cuts5[3],"-",cuts5[4]," mg/kg)"),
                paste0("Above Q3 (",cuts5[4],"-",cuts5[5]," mg/kg)"),
                paste0("Upper outliers (",cuts5[5],"-",max0," mg/kg)")), 
       col=seq(2,7))
rm(list = c("s0","cex0","cuts5","min0","max0"))

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Map using base R and coordinate data ####

git <- "https://raw.githubusercontent.com/Ratey-AtUWA/"
afr_map <- read.csv(file=paste0(git,"spatial/main/afr_map_v2.csv"), 
                    stringsAsFactors = TRUE)
require(prettymapr)
par(mar = c(3,3,1,1.5), mgp = c(1.6, 0.3, 0), 
    tcl = 0.3, ljoin = "mitre", lend = "square", xpd=F)
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
register_google(key = secret)
af_gg <- get_googlemap(center = c(lon = 115.9445, lat = -31.9182),
                       maptype = "hybrid", 
                       zoom = 16, scale = 2)

ggmap(af_gg) + 
  geom_point(data = afw20, aes(x=Longitude, y=Latitude, size = EC), 
             shape = 19, stroke=2, col = "cyan", alpha=0.7) 

## APPENDIX: Coordinate conversion ####

# see https://ratey-atuwa.github.io/Learn-R-web/maps.html 

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# ASCII big lettering from https://patorjk.com/software/taag/
# using the Cybermedium font