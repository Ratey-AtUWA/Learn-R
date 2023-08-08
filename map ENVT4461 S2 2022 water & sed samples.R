## load packages ##
library(sp)
library(sf)
library(maptiles)
library(prettymapr)
library(TeachingDemos)

## load maps data and do setups ####
git <- "https://raw.githubusercontent.com/Ratey-AtUWA/"
afr_map <- read.csv(file=paste0(git,"spatial/main/afr_map_v2.csv"), 
                      stringsAsFactors = TRUE)
pal4lite <- c("black", "purple2", "blue2", "cyan4", "forestgreen", 
              "darkorange", "gold3", "red3", "gray80", "white")
pal4liteTransp <- c("#000000","#912CEEb0","#0000EEb0","#008B8Bb0","#228B22b0",
                    "#CDAD00b0", "#FF8C00b0", "#CD0000b0", "#CCCCCC", "#FFFFFF")
pal4dark <- c("white", "pink1", "orange", "khaki1", "lightgreen", "cadetblue1", 
              "deepskyblue", "plum", "gray80", "black")
windowsFonts(nar=windowsFont("Arial Narrow"), monosans=windowsFont("Consolas"))

## define CRS ####
LongLat <- CRS("+proj=longlat +ellps=WGS84 
           +datum=WGS84 +no_defs") # uses Earth ellipsis specs from WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50, S hemisphere!

## make map extent object ####
afr_utm <- SpatialPoints(coords = data.frame(x = c(399900,400500),
                         y = c(6467950,6468350)), proj4string = UTM50)
extent <- st_as_sf(afr_utm)

# **NOTE**: The projection we specify here will be the one the map plots in!

## Getting the map tile data ####

# We now need some functions from the 'maptiles' package (Giraud 2021).
# We're using one of the OpenStreetMap tile options, but the following
# tile providers also work:  
# OpenStreetMap, OpenStreetMap.HOT, Esri.WorldStreetMap, Esri.WorldTopoMap,
# Esri.WorldImagery, Esri.WorldGrayCanvas, Stamen.Toner, Stamen.TonerBackground,
# Stamen.TonerHybrid, Stamen.TonerLines, Stamen.TonerLabels, Stamen.TonerLite,
# CartoDB.Positron, CartoDB.DarkMatter, CartoDB.Voyager (all CartoDB... tiles
# have variants which work), OpenTopoMap
# The option {crop = TRUE} is included to crop the tiles to our defined area in
# the {extent} object. If we leave it out, the map may change shape as it will
# use only square (uncropped) map tiles.

## get map tiles
aftiles <- get_tiles(extent, provider = "Esri.WorldImagery", crop = TRUE, zoom = 16)

## Plotting the map ####

# The 'aftiles' object we created is a 'SpatRaster' object which needs the
# maptiles package loaded to be able to plot it. We need to set the outer
# margins of the plot area (by default they are zeros) to allow plotting
# of axes etc.

## plot map and axes
palette(pal4dark)
par(oma=c(3,3,1,1), lend="square")
plot(aftiles)
# box(lwd=6,col="white") # tidies up skewness of UTM tiles a bit
axis(1, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.5, font=2)
axis(2, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.5, font=2)
box()
# . . . then . . .
addnortharrow(text.col=1, border=1, pos = "bottomright", padin = c(0.6,0.5))
addscalebar(plotepsg = 32750, pos = "bottomright", label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.2)
# . . . then add points for water . . .
with(afr_coords_S2_2022, points(Northing ~ Easting, lwd = 2, 
                                pch = c(0,1,2,5)[Group], 
                                col = c(2,4,6,8)[Group], cex = 1.5,
                                subset = Type == "Water"))
with(afr_coords_S2_2022, shadowtext(Northing ~ Easting, cex = 0.8, pos = c(4,2), 
                              labels = paste0("(",Easting,",\n",Northing,")"), 
                              col = c(2,4,6,8)[Group], 
                              subset = Type == "Water"))
points(400024,6468196, lwd = 2, pch = 2, col = "dodgerblue", cex = 1.5)
text(400024,6468196, pos = 3, labels = "(400024,\n6468196)", col = "dodgerblue", cex = 0.9)
points(400224,6468196, lwd = 2, pch = 2, col = "grey40", cex = 1.5)
text(400224,6468196, pos = 4, labels = "(400224,\n6468196)", col = "grey40", cex = 0.8)
arrows(400218,6468196,400030,6468196, col="dodgerblue", length = 0.15, angle = 15)
legend("bottom", legend=levels(as.factor(afwS2_2022$Group)), y.intersp = 1.2,
       pch = c(0,1,2,5), col = c(2,4,6,8), cex = 1.4, text.col = 1, bty = "o",
       pt.lwd = 2, inset = 0.1, ncol = 1, pt.cex = 1.5, bg = "#00000080", 
       title = expression(italic("Group")), title.col = "khaki")

plot(aftiles)
axis(1, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.5, font=2)
axis(2, tcl=-0.2, mgp = c(1.6,0.3,0))
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.5, font=2)
box()
# . . . then . . .
addnortharrow(text.col=1, border=1, pos = "bottomright", padin = c(0.6,0.5))
addscalebar(plotepsg = 32750, pos = "bottomright", label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.2)
# . . . then add points for sediment . . .
with(afr_coords_S2_2022, points(Northing ~ Easting, lwd = 2, 
                                pch = c(0,1,2,5)[Group], 
                                col = c(2,4,6,8)[Group], cex = 1.5,
                                subset = Type == "Sediment"))
with(afr_coords_S2_2022, text(Northing ~ Easting, cex = 0.8, pos = c(4,2), 
                              labels = paste0("(",Easting,",\n",Northing,")"), 
                              col = c(2,4,6,8)[Group], 
                              subset = Type == "Water"))
points(400024,6468196, lwd = 2, pch = 2, col = "cyan", cex = 1.5)
shadowtext(400024,6468196, pos = 3, labels ="(400024,\n6468196)", 
                          font=2, col = "cyan", cex = 0.9)
points(400224,6468196, lwd = 2, pch = 2, col = "grey40", cex = 1.5)
text(400224,6468196, pos = 4, labels = "(400224,\n6468196)", col = "grey40", cex = 0.8)
arrows(400218,6468196,400030,6468196, col="cyan", length = 0.15, angle = 15)
legend("bottom", legend=levels(as.factor(afwS2_2022$Group)), y.intersp = 1.2,
       pch = c(0,1,2,5), col = c(2,4,6,8), cex = 1.4, text.col = 1, bty = "o",
       pt.lwd = 2, inset = 0.1, ncol = 1, pt.cex = 1.5, bg = "#00000080", 
       title = expression(italic("Group")), title.col = "khaki")

# or with ggmap ####

utmpts <- SpatialPoints(afr_coords_S2_2022[,c("Easting","Northing")], proj4string = UTM50)
llpts <- spTransform(utmpts, CRSobj = LongLat)
afr_coords_S2_2022[,c("Longitude","Latitude")] <- llpts@coords[,1:2]

library(ggmap)

# Group 1 samples ####
afr_Group1 <- get_googlemap(center=c(115.9464,-31.91693), 
                             zoom = 18, maptype = "satellite", color = "col")
ggmap(afr_Group1) + 
  geom_point(aes(x = Longitude, y =Latitude), col="cyan", shape=3, 
             alpha=0.75, size=3, stroke=1, 
             data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Water"),], 
             na.rm = TRUE) +
  geom_text(aes(x = Longitude, y = Latitude+ 0.00006, label=Seq, hjust=0), 
            data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Water"),], 
            col = "cyan", na.rm = TRUE) +
  geom_text(aes(x = 115.948, y = -31.917, label="Group 1 Water", hjust=1), 
            col = "cyan", size = 6) +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)")

ggmap(afr_Group1) + 
  geom_point(aes(x = Longitude, y =Latitude), col="gold", shape=3, 
             alpha=0.75, size=3, stroke=1, 
             data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Sediment"),], 
             na.rm = TRUE) +
  geom_text(aes(x = Longitude, y = Latitude+ 0.00006, label=Seq, hjust=0), 
            data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Sediment"),], 
            col = "gold", na.rm = TRUE) +
  geom_text(aes(x = 115.948, y = -31.917, label="Group 1 Sediment", hjust=1), 
            col = "gold", size = 6) +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)")

# Group 2 samples ####
afr_Group2 <- get_googlemap(center=c(115.9441,-31.91674), 
                            zoom = 18, maptype = "satellite", color = "col")
ggmap(afr_Group2) + 
  geom_point(aes(x = Longitude, y =Latitude, col=Group), shape=3, 
             size=3, stroke=1, 
             data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Water"),], 
             na.rm = TRUE) + 
  scale_color_manual(values = c("grey30","cyan","grey30","grey30")) +
  geom_text(aes(x = Longitude, y = Latitude+ 0.00006, label=Seq, hjust=0, col = Group),
            data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Water"),], 
            na.rm = TRUE) +
  geom_text(aes(x = 115.9455, y = -31.917, label="Group 2 Water", hjust=1), 
            col = "cyan", size = 6) + theme(legend.position = "none") +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)")

ggmap(afr_Group2) + 
  geom_point(aes(x = Longitude, y =Latitude, col=Group), shape=3, 
             size=3, stroke=1, 
             data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Sediment"),], 
             na.rm = TRUE) +
  scale_color_manual(values = c("grey30","gold","grey30","grey30")) +
  geom_text(aes(x = Longitude, y = Latitude+ 0.00006, label=Seq, hjust=0, col=Group), 
            data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Sediment"),], 
            na.rm = TRUE) +
  geom_text(aes(x = 115.9455, y = -31.917, label="Group 2 Sediment", hjust=1), 
            col = "gold", size = 6) + theme(legend.position = "none") +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)")

# Group 3 samples ####
afr_Group3 <- get_googlemap(center=c(115.9431,-31.91747), 
                            zoom = 18, maptype = "satellite", color = "col")
ggmap(afr_Group3) + 
  geom_point(aes(x = Longitude, y =Latitude, col=Group), shape=3, 
             size=3, stroke=1, 
             data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Water"),], 
             na.rm = TRUE) + 
  scale_color_manual(values = c("grey30","grey30","cyan","grey30")) +
  geom_text(aes(x = Longitude, y = Latitude+ 0.00006, label=Seq, hjust=0, col = Group),
            data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Water"),], 
            na.rm = TRUE) +
  geom_text(aes(x = 115.9445, y = -31.917, label="Group 3 Water", hjust=1), 
            col = "cyan", size = 6) + theme(legend.position = "none") +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)")

ggmap(afr_Group3) + 
  geom_point(aes(x = Longitude, y =Latitude, col=Group), shape=3, 
             size=3, stroke=1, 
             data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Sediment"),], 
             na.rm = TRUE) +
  scale_color_manual(values = c("grey30","grey30","gold","grey30")) +
  geom_text(aes(x = Longitude, y = Latitude+ 0.00006, label=Seq, hjust=0, col=Group), 
            data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Sediment"),], 
            na.rm = TRUE) +
  geom_text(aes(x = 115.9445, y = -31.917, label="Group 3 Sediment", hjust=1), 
            col = "gold", size = 6) + theme(legend.position = "none") +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)")

# Group 4 samples ####
afr_Group4 <- get_googlemap(center=c(115.9418,-31.91864), 
                            zoom = 18, maptype = "satellite", color = "col")
ggmap(afr_Group4) + 
  geom_point(aes(x = Longitude, y =Latitude, col=Group), shape=3, 
             size=3, stroke=1, 
             data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Water"),], 
             na.rm = TRUE) + 
  scale_color_manual(values = c("grey30","grey30","grey30","cyan")) +
  geom_text(aes(x = Longitude, y = Latitude+ 0.00006, label=Seq, hjust=0, col = Group),
            data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Water"),], 
            na.rm = TRUE) +
  geom_text(aes(x = 115.9430, y = -31.918, label="Group 4 Water", hjust=1), 
            col = "cyan", size = 6) + theme(legend.position = "none") +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)")

ggmap(afr_Group4) + 
  geom_point(aes(x = Longitude, y =Latitude, col=Group), shape=3, 
             size=3, stroke=1, 
             data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Sediment"),], 
             na.rm = TRUE) +
  scale_color_manual(values = c("grey30","grey30","grey30","gold")) +
  geom_text(aes(x = Longitude, y = Latitude+ 0.00006, label=Seq, hjust=0, col=Group), 
            data=afr_coords_S2_2022[which(afr_coords_S2_2022$Type=="Sediment"),], 
            na.rm = TRUE) +
  geom_text(aes(x = 115.9430, y = -31.918, label="Group 4 Sediment", hjust=1), 
            col = "gold", size = 6) + theme(legend.position = "none") +
  labs(y="Latitude (\u00B0S)", x = "Longitude (\u00B0E)")

# find centre coordinates ####
cbind(tapply(afr_coords_S2_2022$Longitude,afr_coords_S2_2022$Group,mean),
      tapply(afr_coords_S2_2022$Latitude,afr_coords_S2_2022$Group,mean))
