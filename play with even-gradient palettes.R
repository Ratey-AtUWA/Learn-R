library(viridis)
library(scico)

np <- 20
par(mar=rep(0.5,4))
plot(0:1, 0:1, ann=F, axes = F, type="n")
points(seq(0,1,l=np),rep(1,np),pch=22,bg=viridis(np),cex=3)
text(0.5,0.99,labels=paste0("viridis::viridis(",np,")"),pos=1, cex=2, family="mono")
points(seq(0,1,l=np),rep(0.8,np),pch=22,bg=plasma(np),cex=3)
text(0.5,0.79,labels=paste0("viridis::plasma(",np,")"),pos=1, cex=2, family="mono")
points(seq(0,1,l=np),rep(0.6,np),pch=22,bg=scico(np, palette = "hawaii"),cex=3)
text(0.5,0.59,labels=paste0("scico(",np,", palette='hawaii')"),pos=1, cex=2, family="mono")
points(seq(0,1,l=np),rep(0.4,np),pch=22,bg=scico(np, palette = "lajolla"),cex=3)
text(0.5,0.39,labels=paste0("scico(",np,", palette='lajolla')"),pos=1, cex=2, family="mono")
points(seq(0,1,l=np),rep(0.2,np),pch=22,bg=scico(np, palette = "batlow"),cex=3)
text(0.5,0.19,labels=paste0("scico(",np,", palette='batlow')"),pos=1, cex=2, family="mono")


np <- 13
c# [c(13,6,12,5,11,4,10,3,9,2,8,1,7)]
points(seq(0,1,l=np),rep(0.55,np),pch=22,
       bg=colorRampPalette(c("#004020","royalblue","#FFD3FF"))(np)[c(13,6,12,5,11,4,10,3,9,2,8,1,7)],cex=6)
points(seq(0,1,l=np),rep(0.35,np),pch=22,
       bg=colorRampPalette(c("#400020","royalblue","#c0FFc0"))(np)[c(13,6,12,5,11,4,10,3,9,2,8,1,7)],cex=6)

# _.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__.__._

# Grayscale = 0.299R + 0.587G + 0.114B
# (https://www.dynamsoft.com/blog/insights/image-processing/image-processing-101-color-space-conversion/)
np <- 30
# hexRamp <- colorRampPalette(c("#6D352C","#a0a0a0","#d0ffd0"))(np)
hexRamp <- scico(np, pal="hawaii")
rgbTable <- col2rgb(hexRamp)

greyWt <- ((rgbTable["red",]*0.299) + (rgbTable["green",]*0.587) + (rgbTable["blue",]*0.114))/255
rbind(round(greyWt,1),c(NA,round(diff(greyWt),1)))
greyAv <- ((rgbTable["red",]/3) + (rgbTable["green",]/3) + (rgbTable["blue",]/3))/255
rbind(round(greyAv,1),c(NA,round(diff(greyAv),1)))

par(mar=rep(0.5,4))
plot(0:1, 0:1, ann=F, axes = F, type="n")
points(seq(0,1,l=np), rep(0.85,np), pch=22, bg=hexRamp, cex=6) #, col=hexRamp
text(0.5, 0.82, labels=paste("Full colour ramp, n =",np), pos=1, cex=2)
points(seq(0,1,l=np), rep(0.55,np), pch = 22,
       bg=rgb(greyWt, greyWt, greyWt), cex = 6)
text(0.5, 0.52, labels=paste("Weighted grey ramp, n =",np), pos=1, cex=2)
points(seq(0,1,l=np), rep(0.25,np), pch = 22,
       bg=rgb(greyAv, greyAv, greyAv), cex = 6)
text(0.5, 0.22, labels=paste("Average grey ramp, n =",np), pos=1, cex=2)
