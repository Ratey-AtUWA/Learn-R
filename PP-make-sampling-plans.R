# check packages
packages <- c("sf", "maptiles","prettymapr","TeachingDemos")
install.packages(setdiff(packages, installed.packages()[,1]))

# load packages
library(sf)
library(maptiles)
library(prettymapr)
library(TeachingDemos)

sampgrid <- 
  st_make_grid(pp_samparea,
               cellsize=c(input$xysp, input$xysp),
               offset = st_bbox(pp_samparea)[c("xmin", "ymin")]+input$off0,
               what="centers", square=as.logical(input$ifsq))

par(oma=c(3,3,0.5,0.5), mgp=c(1.6,0.2,0), tcl=0.25)
plot_tiles(ppmap)
axis(1);mtext("Easting (m)",1,1.6,font=2) 
axis(2);mtext("Northing (m)",2,1.6,font=2);box() 

polygon(sa1, col="#e0e08080", border="#d0d070")
polygon(sah1, col="#80c0ffa0", border="#70b0f0")
polygon(sah2, col="#90b09080", border="#80a080")
polygon(sah3, col="#c0c0c0", border="#b0b0b0")

points(st_coordinates(st_as_sf(st_jitter(st_intersection(sampgrid, samppoly),
                                  amount=input$rand0))),
          pch=10, col="blue3", cex=1.2, lwd=1) 
addnortharrow()
addscalebar(plotepsg=32750, pos="bottomright", label.cex=1.4, htin=0.2, 
            padin=c(0.15,0.4))
mtext("EPSG: 32750 \u2022 Datum: WGS84 \u2022 (UTM Zone 50S)"
      , side=1, line=-1.5, adj=0.98, cex=0.85)
legend("bottomleft", bty="n", inset=0.03, y.intersp=1.2, cex=1.4,
       legend=c("Study area","Proposed soil sample locations"), pt.cex=c(4,1.2),
       pch=c(22,10), pt.bg=c("#e0e08080","#00000000"), col=c("#507070","blue3"))
shadowtext(c(388900,389000,389000,388600), c(6460320,6460390,6460690,6460580), 
     labels=c("Swan River","Pelican Point\nReserve","Matilda\nBay","UWA"),
     col=c("cadetblue","olivedrab","cadetblue","navy"), font=c(3,3,3,4), 
     cex=1.5,bg="white")
text(x=c(388745,388960,388978), y=c(6460427,6460472,6460440), 
     labels=c("Inaccessible","Inaccessible","DBCA\nDepot"),
     font=3, col=c("#ffffd0","white","grey15"), cex=0.8)
