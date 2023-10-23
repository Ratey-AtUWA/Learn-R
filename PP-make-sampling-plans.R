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

par(oma=c(3,3,0.5,0.5))
plot_tiles(ppmap)
axis(1);mtext("Easting (m, UTM Zone 50S)",1,1.6,font=2) 
axis(2);mtext("Northing (m, UTM Zone 50S)",2,1.6,font=2);box() 

polygon(sa1, col="#e0e08080", border="#d0d070")
polygon(sah1, col="#80c0ffa0", border="#70b0f0")
polygon(sah2, col="#90b09080", border="#80a080")
polygon(sah3, col="#c0c0c0", border="#b0b0b0")

points(st_coordinates(st_as_sf(st_jitter(st_intersection(sampgrid, samppoly),
                                  amount=input$rand0))),
          pch=10, col="blue3", cex=1.2, lwd=1.5) 
legend("bottomleft", bg="grey95", inset=0.02, y.intersp=1.2, cex=1.2,
       legend=c("Study area","Proposed soil sample locations"),
       pch=22, pt.bg="#e0e08080", col="#d0d070",
       pt.cex=c(3,3))
legend("bottomleft", bty="n", inset=0.02, y.intersp=1.2, cex=1.2,
       legend=c("Study area","Proposed soil sample locations"),
       pch=c(NA,10), col=c("#507070","blue3"), text.col="#00000000")
# coord_sf(xlim = c(388250,389260), ylim = c(6460140,6461000), expand = F) +
shadowtext(c(388900,389000,389000,388600), c(6460320,6460390,6460690,6460580), 
     labels=c("Swan River","Pelican Point\nReserve","Matilda\nBay","UWA"),
     col=c("cadetblue","olivedrab","cadetblue","navy"), font=c(3,3,3,4), 
     cex=1.5,bg="white")
text(x=c(388745,388960,388978), y=c(6460427,6460472,6460440), 
     labels=c("Inaccessible","Inaccessible","DBCA\nDepot"),
     font=3, col=c("#ffffd0","white","grey15"), cex=0.8)
