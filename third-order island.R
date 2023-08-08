library(sp)
library(sf)
library(maptiles)
library(prettymapr)
library(viridis)
library(rosm)
library(ggmap)
library(ggsn)

LongLat <- st_crs(4326) # uses Earth ellipsis specs from WGS84 datum
UTM50S <- st_crs(32750) # just for Zone 50, S hemisphere!

library(png)
addImg <- function(obj, x = NULL, y = NULL, width = NULL, interpolate = TRUE){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr ; PIN <- par()$pin ; DIM <- dim(obj) ; ARp <- DIM[1]/DIM[2]
  WIDi <- width/(USR[2]-USR[1])*PIN[1] ;   HEIi <- WIDi * ARp 
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) 
  rasterImage(image = obj, xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), interpolate = interpolate)
}

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

wider <- st_as_sf(data.frame(x=c(120.882, 121.164),y=c(13.86,14.106)),
                  coords = c("x","y"), crs = LongLat)
island3wider <- get_tiles(wider, provider = "Esri.WorldImagery", 
                          crop = TRUE, zoom=11)

png(filename="wider.png", width = 789, height = 720)
par(oma=c(0,0,0,0), ljoin=1, lend=2)
plot_tiles(island3wider)
addscalebar(label.col = "white", linecol = "white", label.cex = 3, htin = 0.35, 
            widthhint = 0.35, lwd = 3, pos = "topright",padin=c(0.5,0.25))
axis(1,col = "white",col.ticks = "white",mgp=c(2,0,0), 
     labels = T, tcl=0.8,lwd.ticks = 2)
text(pretty(par("usr")[1:2],3),
     rep(par("usr")[3],length(pretty(par("usr")[1:2],3))),
     labels=pretty(par("usr")[1:2],3),
     col="white", cex=2, pos=3, offset=1)
axis(2,col = "white",col.ticks = "white",mgp=c(2,0,0), 
     labels = F, tcl=1,lwd.ticks = 2)
text(rep(par("usr")[1],length(pretty(par("usr")[3:4],3))),
     pretty(par("usr")[3:4],3),
     labels=pretty(par("usr")[3:4],3),
     col="white", cex=2, pos=4, offset=1)
rect(120.985, 14.004,121.01,14.016,border="white",lwd=3)
box(lwd=4, col="white")
dev.off()
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

widest <- st_as_sf(data.frame(x=c(119.6, 124.54),y=c(12.49,18.88)),
                  coords = c("x","y"), crs = LongLat)
island3widest <- get_tiles(widest, provider = "Esri.WorldImagery", 
                          crop = TRUE, zoom=7)

png(filename="widest.png", width = 548, height = 720)
par(oma=c(0,0,0,0), ljoin=1, lend=2, mgp=c(2,0,0))
plot_tiles(island3widest)
addscalebar(label.col = "white", linecol = "white", label.cex = 3, htin = 0.35, 
            widthhint = 0.35, lwd = 3, pos = "topright",padin=c(0.5,0.25))
axis(1,col = "white",col.ticks = "white",mgp=c(2,0,0), 
     labels = T, tcl=0.8,lwd.ticks = 2)
text(120:124,rep(par("usr")[3]+0.1,5),labels=120:124,col="white",cex=2,pos=3)
axis(2,col = "white",col.ticks = "white",mgp=c(2,0,0), 
     labels = F, tcl=1,lwd.ticks = 2)
text(rep(par("usr")[1]+0.1,7),13:19,labels=13:19,col="white",cex=2,pos=4)
rect(120.882,13.86,121.164,14.106,border="white",lwd=2)
box(lwd=3, col="white")
dev.off()
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

extent <- st_as_sf(data.frame(x=c(120.975, 121.004),y=c(14.004,14.017)),
                   coords = c("x","y"), crs = LongLat)

island3tiles <- get_tiles(extent, provider = "Esri.WorldImagery", 
                          crop = TRUE, zoom=16)

par(oma=c(4,4,1,1), ljoin=1, lend=2)
plot_tiles(island3tiles)
addscalebar(label.col = "white", linecol = "white", htin = 0.25, 
            label.cex = 1.5, lwd = 2, pos="topright", widthhint = 0.17,
            padin=c(1.3,0.3))
axis(1);axis(2)
inset1 <- readPNG("wider.png")
rasterImage(inset1,120.984,14.0085,120.991,14.015)
inset2 <- readPNG("widest.png")
rasterImage(inset2,120.975,14.007,120.982,14.016)
lines(c(120.977,120.9841),c(14.0094,14.015), lty=2, col="white")
lines(c(120.977,120.9841),c(14.0091,14.0085), lty=2, col="white")
lines(c(120.9867,120.995),c(14.01255,par("usr")[4]), lty=2, col="white")
lines(c(120.9867,120.995),c(14.01225,par("usr")[3]), lty=2, col="white")
arrows(120.9975, 14.0093,120.9965,14.0093,col="white",length=0.15)
text(120.9975,14.0093,pos=4,labels="An island in a lake\non an island in a lake\non an island",col="white")
box()
# addImg(inset1,x=0.5,y=0.5,width = 0.3)


# island3.gg <- get_googlemap(center=c(-108.2378,69.79315), 
#                            zoom = 16, maptype = "satellite", color = "color")
# ggmap(island3.gg)
