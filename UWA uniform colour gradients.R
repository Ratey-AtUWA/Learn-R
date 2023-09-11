# Grayscale = 0.299R + 0.587G + 0.114B
# (https://www.dynamsoft.com/blog/insights/image-processing/image-processing-101-color-space-conversion/)

# White WHITE   # HEX #FFFFFF, R255 G255 B255
# 
# Black         # HEX #000000 greys #636363 #A0A0A0
# 
# UWA blue (Pantone 287C)
# C100 M90 Y0 K0, R0 G48 B135
# HEX #003087 lighter #5560A4 darker #152968
# 
# UWA gold (Pantone 110C)
# C0 M20 Y100 K15, R218 G170 B0
# HEX #DAAA00 darker #997500 lighter #E4BB5E
# 
# Purple R165 G24 B144 #A51890
# Red R197 G0 B62 #C5003E
# Orange R255 G130 B0 #FF8200
# Yellow R255 G209 B0 #FFD100
# Green R108 G194 B74 #6CC24A
# Teal R0 G178 B169 #00B2A9
# Mid-blue R0 G134 B214 #0086D6
# Cyan R72 G190 B216 #48BED8
# Tan R163 G147 B130 #A39382
# 
# https://uniwa.sharepoint.com/sites/bmr/brand/SitePages/Colour.aspx

UWAgrad <- colorRampPalette(c("#000000","#003087","#DAAA00","#a0a0a0"))(np)
np <- 16
# UWAgrad0 <- colorRampPalette(c("#000F2A","#003087","#DAAA00","#E4BB5E","#ffffff"))(np)
UWAgrad <- colorRampPalette(c("#000b1f","#103f8f","#DAAA00","#E4BB5E","#ffffff"))(np)
# UWAgrad <- colorRampPalette(c("#00091A","#31517d","#ac8e4b","#E6BF68","#ffffff"))(np)
rgbTable <- col2rgb(UWAgrad)
palette(UWAgrad)
greyWt <- ((rgbTable["red",]*0.299) + (rgbTable["green",]*0.587) + (rgbTable["blue",]*0.114))/255
greyAv <- ((rgbTable["red",]/3) + (rgbTable["green",]/3) + (rgbTable["blue",]/3))/255

par(mar=rep(0.5,4),mfrow=c(2,1),mar=c(3,3,.5,.5),mgp=c(1.5,0.2,0), tcl=0.2, font.lab=2)
plot(0:1, 0:1, ann=F, axes = F, type="n")
points(seq(0,1,l=np), rep(0.85,np), pch=22, bg=UWAgrad, cex=6) #, col=UWAgrad
text(0.5, 0.8, labels=paste("Full colour ramp, n =",np), pos=1, cex=2)
points(seq(0,1,l=np), rep(0.55,np), pch = 22,
       bg=rgb(greyWt, greyWt, greyWt), cex = 6)
text(0.5, 0.5, labels=paste("Weighted grey ramp, n =",np), pos=1, cex=2)
points(seq(0,1,l=np), rep(0.25,np), pch = 22,
       bg=rgb(greyAv, greyAv, greyAv), cex = 6)
text(0.5, 0.2, labels=paste("Average grey ramp, n =",np), pos=1, cex=2)
# points(seq(0,1,l=np), rep(0.25,np), pch = 22,
#        bg=UWAgrad0, cex = 6)
# text(0.5, 0.2, labels=paste("Saturated ramp, n =",np), pos=1, cex=2)
rbind(round(greyWt,2),c(NA,round(diff(greyWt),3)))
plot(1:np, round(greyWt,2), ylim=c(0,1), xlab="Gradient sequence number", 
     ylab="Weighted greyscale value", pch=21, cex=4, bg=UWAgrad)
abline(lm(greyWt ~ c(1:np)), col=as.integer(np*0.6),lty=2)

# source("hsv2hex.R");source("hsv2rgb.R") # makes 2 functions hsv2hex & hsv2rgb
(hsvTable <- rgb2hsv(rgbTable))

lmGrad <- lm(greyWt~c(1:np))
summary(lmGrad)

