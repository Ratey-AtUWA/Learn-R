# afs19 <- read.csv(file = "afs19.csv", stringsAsFactors = TRUE) # un-comment me
library(sp)
library(sf)
library(maptiles)
library(prettymapr)
pal4lite <- c("black", viridis::plasma(8), "white")
pal4liteTransp <- 
  c("black", scico::scico(8, alpha=0.7, palette="hawaii", beg=0.1), "white")
pal4dark <- c("white", viridis::turbo(8, beg=0.2, end=0.9,dir=1), "black")
                           
LongLat <- CRS("+proj=longlat +ellps=WGS84 
           +datum=WGS84 +no_defs") # uses Earth ellipsis specs from WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50, S hemisphere!
extent <- 
  st_as_sf(SpatialPoints(coords = 
                           data.frame(x = c(399800,400700),
                                      y = c(6467900,6468400)), 
                         proj4string = UTM50))
aftiles <- get_tiles(extent, provider = "OpenStreetMap.HOT", crop = TRUE)
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
     pos = c(2,2,4), cex = 0.8, font = 3, col = "cadetblue3")
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.15)
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

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

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
palette(pal4liteTransp)
with(afs19, 
     points(Easting, Northing, 
            pch = c(22,22,22,3,4,21,21,21)[QZn], 
            col = c(1,1,1,4,5,1,1,1)[QZn],
            lwd = c(1,1,1,2,2,1,1)[QZn], 
            bg = c(1:8)[QZn], 
            cex = c(0.5,0.65,0.8,1,1,1.5,2.2,3)[QZn])
     )
legend("bottomright", legend = levels(afs19$QZn), title="Zn",
       pch = c(22,22,22,3,4,21,21,21), col = c(1,1,1,4,5,1,1,1),
       pt.lwd = c(1,1,1,2,2,1,1), pt.bg = c(1:8), 
       pt.cex = c(0.5,0.65,0.8,1,1,1.5,2.2,3),
       bty = "n", inset = 0.02, cex = 0.85, y.intersp = 1.2)
palette(pal4lite)
afs19$QZn <- NULL # to delete quantile column (optional; you can leave it in)
