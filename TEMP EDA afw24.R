library(car)
library(sf)
library(maptiles)
library(prettymapr)
library(viridis)
library(TeachingDemos)
afw24 <- read.csv("afw24.csv")
# str(afw24)
afw24$Group <- as.factor(afw24$Group)
afw24$Group <- factor(afw24$Group, 
                      levels=c("1","2","3","4","5","6",
                               "7","8","9","10","11","12",
                               "A","B","C","D","E","F","G","H"))
afw24$Location <- as.factor(afw24$Location)
afw24$Lab.Field.Group <- as.factor(afw24$Lab.Field.Group)
afw24$PO4.P <- as.numeric(afw24$PO4.P)

# ~.~.~.~.~.~.~.~.~.~.~.~.~.~. SCATTER PLOT MATRIX .~.~.~.~.~.~.~.~.~.~.~.~ ####

spm(~pH+Al+Fe+Ca+K+Mg+Na+S | Group, data=afw24, smooth=F, regLine=F, log="xy",
    use="pairwise.complete.obs", col=plasma(20, end=0.7), pch=rep(c(15,17,18,19),5), 
    legend=F)

sp(S~Na | Group, data=afw24, smooth=F, legend=list(coords="bottomright", cex=1.2),
   log="xy", cex=1.2, col=plasma(20, end=0.7, alp=0.7), pch=rep(c(15,17,18,19),5))

par(mar=c(3,3,1,1), oma=c(0,0,0,0), mgp=c(1.7,0.3,0), tcl=0.25, font.lab=2, 
    cex.lab=1.2, lend="square", ljoin="mitre")
with(afw24, plot(S ~ Na, log="xy", pch=rep(c(21:25),4)[Group], 
                 bg=plasma(20)[Group], cex=1.5))
legend("bottomright", legend=c(levels(afw24$Group),"","","",""), title="Group", 
       ncol=2, pch=c(rep(c(21:25),4),NA,NA,NA,NA), 
       pt.bg=plasma(20), pt.cex=1.5, inset=0.02, bty="n")

# ~.~.~.~.~.~.~.~.~.~.~.~.~.~.~. BUBBLE MAP .~.~.~.~.~.~.~.~.~.~.~.~.~.~.~ ####

# make background map raster
afext <- st_as_sf(data.frame(x=c(399800,400600), y=c(6467900,6468400)),
                  coords = c("x","y"), crs=st_crs(32750))
afUTM <- get_tiles(afext, provider="Thunderforest.Outdoors",
                   apikey="03b0375e8e7c47d0a03018a9001de439",
                   crop=TRUE, zoom=17)

# choose variable and guideline value + units
# ==================================================#
v0 <- "Zn"    # make sure this variable is correct  |
GV <- 0.008   # make sure its guideline is correct  |
GU <- "mg/L" # make sure guideline unit is correct  |
# ==================================================#

# plot the map
par(oma=c(3,3,1,1), mar=c(0,0,0,0), mgp=c(1.7,0.3,0), font.lab=2, tcl=-0.2)
plot_tiles(afUTM, axes=T)
# axis(1);axis(2);box()
mtext("Easting",1,1.7,font=2,cex=1.2)
mtext("Northing",2,1.7,font=2,cex=1.2)
legend("bottomleft", bty="o", cex=0.65, y.int=0.6, inset=c(0.02,0.005),
       legend="Map tiles: Thunderforest Outdoors; CRS: UTM Zone 50S, WGS84 (EPSG:32750)",
       box.col="#00000000", bg="#ffffff80")
with(afr_map, polygon(wetland_E, wetland_N, border="steelblue", col="#50b0e080"))
with(afr_map, lines(drain_E, drain_N, col="steelblue", lwd=2))
addnortharrow()
addscalebar(plotepsg=32750, htin=0.12, label.cex=1.2, padin=c(4.5,0.1),widthhint=0.12)
with(afw24, points(E.approx, N.approx, lwd=0.7,
                   pch=rep(21:25,4)[Group], cex=1.2,
                   bg=viridis(20, alpha=0.7)[Group]))
shadowtext(tapply(afw24$Easting, afw24$Group, function(x){mean(x, trim=0.5, na.rm=T)}),
           tapply(afw24$Northing, afw24$Group, function(x){mean(x, trim=0.5, na.rm=T)}),
           labels=levels(afw24$Group),pos=1,col="black",bg="white")
legend("bottomright", legend=paste(levels(afw24$Group),"  "), title="Group", cex=0.8,
       pch=rep(21:25,4), pt.bg=viridis(20, alpha=0.7), pt.lwd=0.7,
       ncol=3, inset=c(0.02,0.01), pt.cex=1.2, box.col="steelblue", bg="#ffffff80")

# make a scaling factor for bubble sizes for data and legend
ff <- signif((0.033*(par("usr")[4]-par("usr")[3]))/sqrt(max(afw24$Zn,na.rm=T)),3)
# plot the data as area-proportional bubbles
with(afw24, symbols(E.approx, N.approx, add=TRUE, circles = ff*sqrt(Zn), inches=F,
                        bg="#ffe00080", fg="sienna"))
# plot symbol to represnt samples exceeding guideline
with(afw24[which(afw24$Zn > GV),], points(Easting, Northing,
                                                 pch=19, col="red"))
# set up values to plot legend
xy <- par("usr")
if(pretty(afw24$Zn)[1]==0){
  bublo <- pretty(afw24$Zn)[2]/2
} else {
  bublo <- pretty(afw24$Zn)[1]
}
bubmd <- median(pretty(afw24$Zn))
bubhi <- tail(pretty(afw24$Zn),1)

# define dimensions of legend box as c(x1, x2, y1, y2) in axis proportions
bxy <- c(0.02,0.25,0.7,0.99)
# draw legend box
rect(xy[1]+bxy[1]*(xy[2]-xy[1]), xy[3]+bxy[3]*(xy[4]-xy[3]),
     xy[1]+bxy[2]*(xy[2]-xy[1]), xy[3]+bxy[4]*(xy[4]-xy[3]),
     col="#ffffffa0", border = "grey50")
# draw legend bubbles
symbols(rep(xy[1]+0.08*(xy[2]-xy[1]), 3),
        c(xy[3]+0.9*(xy[4]-xy[3]), xy[3]+0.83*(xy[4]-xy[3]), xy[3]+0.78*(xy[4]-xy[3])),
        add=T, inches=F, circles=ff*sqrt(c(bubhi,bubmd,bublo)),
        bg="#ffe00080", fg="sienna")
# draw legend 'exceed guideline' symbol
points(xy[1]+0.05*(xy[2]-xy[1]), xy[3]+0.73*(xy[4]-xy[3]), pch=19, col="red")
# add legend text
text(c(rep(xy[1]+0.1*(xy[2]-xy[1]), 3), xy[1]+0.05*(xy[2]-xy[1])),
     c(xy[3]+0.9*(xy[4]-xy[3]), xy[3]+0.83*(xy[4]-xy[3]),
       xy[3]+0.78*(xy[4]-xy[3]), xy[3]+0.73*(xy[4]-xy[3])),
     labels=c(bubhi,bubmd,bublo,paste0("Exceeds ",GV,"\n",GU," guideline")),
     pos=4, offset=1.2, cex=0.85)
# add legend title
text(xy[1]+0.12*(xy[2]-xy[1]), xy[3]+0.96*(xy[4]-xy[3]),
     labels=paste0("Zn (",GU,")"), font=2, cex=1.2)
RcmdrMisc::numSummary(afw24[,c(10,12:14,16:44)])

# ~.~.~.~.~.~.~.~.~.~.~.~. STRIPCHARTS .~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~ ####

afw24$SO4_Cl <- with(afw24, (S*(96/32))/(Na*1.799))

par(mar=c(3,10,1,1), oma=c(0,0,0,0), mgp=c(1.7,0.3,0), tcl=0.25, font.lab=2, 
    cex.lab=1.2, lend="square", ljoin="mitre")
with(afw24, stripchart(Al ~ Location, las=1, log="x", method="jitter", pch=10, 
                       lwd=2, ylim=c(20,1), cex=1.5, 
                       col=plasma(20, end=.8, alp=0.5, dir=-1)))
abline(h=seq(1.5,19.5,1), col="#00000040", lty=2); box(lwd=2)

with(afw24, stripchart(log10(SO4_Cl) ~ Location, las=1, method="jitter", pch=19, 
                       lwd=2, ylim=c(20,1), cex=1.8, xaxt="n", xlab="", 
                       col="#00000000")); box(lwd=2)
axis(1, at=log10(c(.01,.02,.05,.1,.2,.5,1,2,5,10,20,50,100,200,500,1e3,2e3,5e3)),
     labels=c(.01,.02,.05,.1,.2,.5,1,2,5,10,20,50,100,200,500,1e3,2e3,5e3))
mtext(expression(bold(paste(log[10],"(SO"[4],"/Cl)"))),1,1.7,cex=1.2)
for (i in seq(1,19,2)){
  rect(par("usr")[1],i+0.5,par("usr")[2],i-0.5, col="#00000020", border="transparent")
}
abline(v=log10(0.14), col="#00a0a040", lwd=25)
text(log10(0.14),3.5,labels="Sea water 0.14", srt=90, col="#008080")
text(log10(0.14),17.5,labels="Sea water 0.14", srt=-90, col="#008080")
abline(v=log10(0.5), col="#f0400040", lwd=25)
text(log10(0.5),4,labels="Acid sulfate trigger 0.5", srt=90, col="#a00000")
text(log10(0.5),17,labels="Acid sulfate trigger 0.5", srt=-90, col="#a00000")
text(c(log10(0.015),log10(1.3)),c(15,7),
     labels=c("Sulfate-\ndepleted","Excess\nsulfate"),
     cex=2.4, font=3, col="#00000060", pos=c(4,2), offset=0)
with(afw24, stripchart(log10(SO4_Cl) ~ Location, las=1, method="jitter", pch=19, 
                       lwd=2, ylim=c(20,1), cex=1.8, xaxt="n", xlab="", 
                       col=plasma(20, end=.8, alp=0.5, dir=-1), add=TRUE))

# ~.~.~.~.~.~.~.~.~.~.~.~. BOXPLOTS .~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~.~ ####

par(mar=c(3,10,1,1), oma=c(0,0,0,0), mgp=c(1.7,0.3,0), tcl=0.25, font.lab=2, 
    cex.lab=1.2, lend="square", ljoin="mitre")
with(afw24, 
     boxplot(log10(P) ~ Location, las=1, lwd=1, xlim=c(20,1), horizontal=T, 
             col=mako(20, end=.8, alp=0.5, dir=-1),ylab="", xaxt="n", xlab=""))
axis(1, at=log10(c(.01,.02,.05,.1,.2,.5,1,2,5,10,20,50,100,200,500,1e3,2e3,5e3)),
     labels=c(.01,.02,.05,.1,.2,.5,1,2,5,10,20,50,100,200,500,1e3,2e3,5e3))
mtext(expression(bold(paste(log[10],"(P)"))),1,1.7,cex=1.2)
abline(h=seq(1.5,19.5,1), col="#00000040", lty=2); box(lwd=1)

with(afw24, 
     boxplot(log10(SO4_Cl) ~ Location, las=1, lwd=0.7, xlim=c(20,1), horizontal=T, 
             col="transparent", border="transparent", ylab="", xaxt="n", xlab=""))
axis(1, at=log10(c(.01,.02,.05,.1,.2,.5,1,2,5,10,20,50,100,200,500,1e3,2e3,5e3)),
     labels=c(.01,.02,.05,.1,.2,.5,1,2,5,10,20,50,100,200,500,1e3,2e3,5e3))
mtext(expression(bold(paste(log[10],"(SO"[4],"/Cl)"))),1,1.7,cex=1.2)
abline(h=seq(1.5,19.5,1), col="#00000040", lty=2); box(lwd=1)
abline(v=log10(0.14), col="#00a0a040", lwd=25)
text(log10(0.14),2.5,labels="Sea water 0.14", srt=90, col="#008080")
text(log10(0.14),17.5,labels="Sea water 0.14", srt=-90, col="#008080")
abline(v=log10(0.5), col="#f0400040", lwd=25)
text(log10(0.5),3,labels="Acid sulfate trigger 0.5", srt=90, col="#a00000")
text(log10(0.5),17,labels="Acid sulfate trigger 0.5", srt=-90, col="#a00000")
with(afw24, 
     boxplot(log10(SO4_Cl) ~ Location, las=1, lwd=0.7, xlim=c(20,1), horizontal=T, 
             col=mako(20, end=.8, alp=0.5, dir=-1),ylab="", xaxt="n", xlab="",
             add=T))
