# function to draw map based on digitized data from
# '../spatial/main/afr_map_v2.csv'
afrMap <- function(
    proj = "utm",
    xlim = NULL,
    ylim = NULL,
    maplabels = TRUE,
    pos.north = "topleft",
    pos.scale = "bottomleft"
)
{ require(prettymapr)
  if (proj != "utm"){
    z <- 0
    axlabs <- c("Longitude (\u00B0E)","Latitude (\u00B0S)")
    aspct <- 1.2
    epsg <- 4326
  } else {
    z <- 2
    axlabs <- c("UTM Easting (m)","UTM Northing (m)")
    aspct <- 1
    epsg <- 32750
  }
  
  afr_map <- read.csv(file="https://raw.githubusercontent.com/Ratey-AtUWA/spatial/main/afr_map_v2.csv", 
                      stringsAsFactors = TRUE)
  par(mar = c(3.5, 3.5, 1, 1), mgp = c(1.7, 0.3, 0), tcl = 0.25, font.lab = 2,
      lend = "round", ljoin = "mitre")
  plot(afr_map[,10+z], afr_map[,11+z], 
       lwd=1, col = "darkkhaki", lty = 3, asp=aspct, type="l",
       xlab = axlabs[1], ylab = axlabs[2], xlim = xlim, ylim = ylim)
  polygon(afr_map[,6+z], afr_map[,7+z], 
          border = "#9AB79A", col = "#9AB79A")
  polygon(afr_map[,20+z], afr_map[,21+z], 
          border = "dodgerblue", col = "lightblue")
  polygon(afr_map[1:67, 24+z], afr_map[1:67, 25+z], 
          border = "dodgerblue", col = "lightblue")
  polygon(afr_map[69:92, 24+z], afr_map[69:92, 25+z], 
          border = "dodgerblue", col = "white")
  lines(afr_map[,2+z],afr_map[,3+z], 
        lwd=5, col = "grey")
  lines(afr_map[,15+z],afr_map[,16+z], 
        lwd = 2, col = "royalblue3")
  lines(afr_map[,29+z],afr_map[,30+z], 
        lty = 2, col = "grey67")
  
  addnortharrow(pos = pos.north)
  addscalebar(plotepsg = epsg, htin = 0.15, 
              label.cex = 1.2, pos = pos.scale)
  
  afrLabels <- 
    read.csv(file="https://raw.githubusercontent.com/Ratey-AtUWA/spatial/main/afr_labels.csv")
  if(isTRUE(maplabels)) {
    text(afrLabels[1,4-z], afrLabels[1,5-z], 
         labels = afrLabels[1,1], 
         col = "royalblue4", font = 3, cex = 1.2, 
         srt = 330)
    text(afrLabels[2,4-z], afrLabels[2,5-z], 
         labels = afrLabels[2,1], 
         col = "royalblue3", srt = 45)
    text(afrLabels[3,4-z], afrLabels[3,5-z], 
         labels = afrLabels[3,1], 
         col = "royalblue3", srt = 300)
    text(afrLabels[4,4-z], afrLabels[4,5-z], 
         labels = "Woolcock\nCt Drain", 
         col = "royalblue3", pos = 4)
    text(afrLabels[5,4-z], afrLabels[5,5-z], 
         labels = afrLabels[5,1], 
         col = "grey65", srt = 45)
    text(afrLabels[6,4-z], afrLabels[6,5-z], 
         labels = afrLabels[6,1], 
         col = "grey65", srt = 50)
    text(afrLabels[7,4-z], afrLabels[7,5-z], 
         labels = afrLabels[7,1], 
         col = "grey65", srt = 59)
    text(afrLabels[8,4-z]+50, afrLabels[8,5-z]-50, 
         labels = "Ashfield\nFlats\nReserve", 
         col = "darkolivegreen", font = 3, cex = 1.2)
    points(afrLabels[9,4-z], afrLabels[9,5-z], 
           pch = 19, cex = 0.75, 
           col = "black")
    text(afrLabels[10,4-z], afrLabels[10,5-z], 
         labels = afrLabels[10,1], 
         col = "grey65", pos = 2)
    box()
  } else {
    box()
  }
}