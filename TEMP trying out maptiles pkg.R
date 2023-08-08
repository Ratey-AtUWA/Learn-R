# trying out maptiles package
library(sp)
library(sf)
library(maptiles)
library(prettymapr)

LongLat <- CRS("+proj=longlat +ellps=WGS84 
           +datum=WGS84 +no_defs") # uses Earth ellipsis specs from WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50, S hemisphere!

# make a temporary object containing the UTM coordinates
# Example uses data frame called data1 - edit to suit!
extent <- 
  st_as_sf(SpatialPoints(coords = 
                           data.frame(Latitude = c(115.94,115.951),
                                      Longitude = c(-31.9225,-31.915)), 
                         proj4string = LongLat))
extent <- 
  st_as_sf(SpatialPoints(coords = 
                           data.frame(Latitude = c(399800,400700),
                                      Longitude = c(6467700,6468400)), 
                         proj4string = UTM50))

# these tile providers work:
# OpenStreetMap, OpenStreetMap.HOT, Esri.WorldStreetMap, Esri.WorldTopoMap,
# Esri.WorldImagery, Esri.WorldGrayCanvas, Stamen.Toner, Stamen.TonerBackground,
# Stamen.TonerHybrid, Stamen.TonerLines, Stamen.TonerLabels, Stamen.TonerLite,
# CartoDB.Positron, CartoDB.DarkMatter, CartoDB.Voyager, * OpenTopoMap *, 
# Wikimedia
par(oma=c(3,3,1,1))
pal4lite <- c("black", "red3", "darkorange", "gold3", "forestgreen", "cyan4", 
              "blue2", "purple2", "gray56", "white")
pal4dark <- c("white", "pink1", "orange", "khaki1", "lightgreen", "cadetblue1", 
              "deepskyblue", "plum", "gray80", "black")
palette(pal4lite)
aftiles <- get_tiles(extent, provider = "OpenTopoMap")

plot(aftiles) # par("usr")
# with(afs22, points(Longitude, Latitude))
with(afs22, points(Easting, Northing, pch=19, col=8))
box(lwd=6,col="white")
axis(1, tcl=-0.2); mtext("Easting (UTM Zone 50, m)", 
               side = 1, line = 1.5, font=2)
axis(2, tcl=-0.2); mtext("Northing (UTM Zone 50, m)", 
               side = 2, line = 1.5, font=2)
box()
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.3)
legend('topleft', legend = "Sediment samples", pch=19, col=8, bty = "o",
       text.col = 1, bg = 10, inset = 0.02)
