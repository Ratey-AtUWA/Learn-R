palette("default")
par(mfrow=c(1,2), mar=c(3,0,1,0),oma=c(0,12,0,1), mgp=c(1.7,0.3,0),
    lend="square",tcl=0, font.lab=2)
with(droplevels(subset(afw23, subset=afw23$Drain2=="Drain")),
     boxplot(K~Site.Location, horizontal = T,las=1,log="x",ylim=c(5,3000),
             xlab="Concentration (mg/L)", ylab="",  col="#c0c0c040"))
axis(1, tcl=0.3)
axis(2,at=c(1:nlevels(afw23$Site.Location)),labels = NA, tcl=0.3)
with(droplevels(subset(afw23, subset=afw23$Drain2=="Drain")),
     boxplot(Ca~Site.Location, horizontal = T,las=1,add=T,
             col="#00FFFF40",names=NA,
             pars=list(tcl=0, boxcol=4, medcol=4, staplecol=4),
             at=c(1:nlevels(Site.Location))+0.1))
with(droplevels(subset(afw23, subset=afw23$Drain2=="Drain")),
     boxplot(Mg~Site.Location, horizontal = T,las=1,add=T,
             col="#FFFF0040",names=NA,
             pars=list(tcl=0, boxcol=7, medcol=7, staplecol=7),
             at=c(1:nlevels(Site.Location))-0.1))
legend("topright", bty="n", inset=0.0, legend=c("Mg","K","Ca"),
       pch=22, col=c(7,1,4), pt.bg=c("#FFFF0040","#c0c0c040","#00FFFF40"), 
       pt.lwd=2, pt.cex=4, cex=1.8, y.intersp = 1.)

with(droplevels(subset(afw23, subset=afw23$Drain2=="Drain")),
     boxplot(Chloride~Site.Location, horizontal = T,las=1,log="x",
             ylim=c(100,30000), names=NA,
             pars=list(tcl=0, outcol=4, boxcol=4, medcol=4, staplecol=4),
             xlab="Concentration (mg/L)", ylab="",  col="#00FFFF40"))
axis(1, tcl=0.3)
axis(2,at=c(1:nlevels(afw23$Site.Location)),labels = NA, tcl=0.3)
with(droplevels(subset(afw23, subset=afw23$Drain2=="Drain")),
     boxplot(Na~Site.Location, horizontal = T,las=1,add=T,
             col="#FF00FF40",names=NA,
             pars=list(tcl=0, outcol=2, boxcol=2, medcol=2, staplecol=2),
             at=c(1:nlevels(Site.Location))+0.1))
legend("topright", bty="n", inset=0.0, legend=c("Na","Cl"),
       pch=22, col=c(4,2), pt.bg=c("#00FFFF40","#FF00FF40"), pt.lwd=2,
       pt.cex=4, cex=1.8, y.intersp = 1.)