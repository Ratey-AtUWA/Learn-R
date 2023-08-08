#### FIRST RUN CODE IN eDNA DataAnal_R_17.Rmd OR eDNA-DataAnal_R_noMarkdown.R !!
# ~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^

par(mar=c(3,3,1,1), mgp=c(1.7,0.3,0), tcl=0.25, lend=2, ljoin=1, font.lab=2,
    mfrow=c(2,2))
palette(c("black", inferno(8)[2:6],"white"))
plot(AF_nmds_all, display="sites", cex=1.4, xlim=c(-1.8,1.6), ylim=c(-1.8,1.1))
mtext("(a)", side=3, line=-1.4, adj=0.03, cex=1.4)
points(AF_nmds_all$points, col=c(1,3:6)[eDNA2023$Zone], pch=19, cex=1.4)
ordiellipse(AF_nmds_all, groups=eDNA2023$Zone, col=2:6, lwd=2, 
            kind = "sd", conf=0.75)
shadowtext(tapply(AF_nmds_all$points[,1], eDNA2023$Zone, mean),
           tapply(AF_nmds_all$points[,2], eDNA2023$Zone, mean),
           labels=levels(eDNA2023$Zone), col=2:6, bg=7, r=0.2)
# legend("bottomright", legend=levels(eDNA2023$Zone),col=2:6,lty=1,lwd=2,bty="n", 
#        text.col = 2:6)
# legend("bottomright", bty="n", legend=levels(eDNA2023$Zone), col=1, pt.bg=2:6,
#        lty=1, lwd=2, pch=21, pt.cex=1.5,text.col = 2:6)
legend("bottomright", bty="n", legend=levels(eDNA2023$Zone), title="Zone",
       col=c(1,3:6), pt.bg=2:6, lty=1, lwd=2, pch=NA, pt.cex=1.5)
legend("bottomright", bty="n", legend=levels(eDNA2023$Zone), col=7, 
       pt.bg=c(1,3:6), lty=NA, lwd=2, pch=21, pt.cex=2.)

# plot (b) convex hulls
par(mar=c(3,3,1,1), mgp=c(1.7,0.3,0), tcl=0.25, lend=2, ljoin=1, font.lab=2)
palette(c("black", rocket(19)[2:18],"white"))
plot(AF_nmds_all, display="sites", cex=1.4, xlim=c(-1.8,1.6), ylim=c(-1.8,1.1))
points(AF_nmds_all$points, bg=c(2:18)[eDNA2023$Site], 
       pch=c(rep(21:25,3),21,22)[eDNA2023$Site], cex=1.4)
ordihull(AF_nmds_all, groups=eDNA2023$Site, 
         col="blue3", lwd=2, label=TRUE)
ordihull(AF_nmds_all, groups=eDNA2023$Site, 
         col=2:18, lwd=2, label=F)
mtext("(b)", side=3, line=-1.4, adj=0.03, cex=1.4)
legend("bottomright", bty="n", legend=levels(eDNA2023$Site), title="Site",
        ncol=6, col=2:18, pt.bg=2:18, lty=1, lwd=2, pch=NA, pt.cex=1.5)
legend("bottomright", bty="n", legend=levels(eDNA2023$Site), ncol=6, col=1, 
     pt.bg=2:18, lty=NA, lwd=2, pch=c(rep(21:25,3),21,22), pt.cex=1.5, pt.lwd=1)

# (c) spiders
palette(c("black", plasma(8)[2:6],"white"))
plot(AF_nmds_all, display="sites", cex=1.4, xlim=c(-1.8,1.6), ylim=c(-1.8,1.1))
mtext("(c)", side=3, line=-1.4, adj=0.03, cex=1.4)
points(AF_nmds_all$points, col=c(1,3:6)[eDNA2023$Zone], pch=19, cex=1.4)
ordispider(AF_nmds_all, groups=eDNA2023$Zone, col=2:6, lwd=1)
shadowtext(tapply(AF_nmds_all$points[,1], eDNA2023$Zone, mean),
           tapply(AF_nmds_all$points[,2], eDNA2023$Zone, mean),
           labels=levels(eDNA2023$Zone), col=2:6, bg=7, r=0.2)
# legend("bottomright", legend=levels(eDNA2023$Zone),col=2:6,lty=1,lwd=2,bty="n", 
#        text.col = 2:6)
# legend("bottomright", bty="n", legend=levels(eDNA2023$Zone), col=1, pt.bg=2:6,
#        lty=1, lwd=2, pch=21, pt.cex=1.5,text.col = 2:6)
legend("bottomright", bty="n", legend=levels(eDNA2023$Zone), title="Zone",
       col=c(1,3:6), pt.bg=2:6, lty=1, lwd=2, pch=NA, pt.cex=1.5)
legend("bottomright", bty="n", legend=levels(eDNA2023$Zone), col=7, 
       pt.bg=c(1,3:6), lty=NA, lwd=2, pch=21, pt.cex=2.)

plot(c(0,1),c(0,1),ann=F,type="n",xaxt="n",yaxt="n", bty="n")
text(0,0.5,pos=4, cex=1.5,labels="These plots show grouping of\nmMDS plots for eDNA data by\n'Zone' or Site.\n\nCan we group eDNA observations \nusing a different factor?\ne.g. based on categories derived\nfrom a variable?")

# plot (d) bars
# par(mar=c(3,3,1,1), mgp=c(1.7,0.3,0), tcl=0.25, lend=2, ljoin=1, font.lab=2)
# palette(c("black", rocket(8)[2:6],"white"))
# plot(AF_nmds_all, display="sites", cex=1.4, xlim=c(-1.8,1.6), ylim=c(-1.8,1.1))
# points(AF_nmds_all$points, col=c(2:6)[eDNA2023$Zone], 
#        pch=19, cex=1.4)
# ordibar(AF_nmds_all, groups=eDNA2023$Zone, length=1, 
#         kind = "se", conf=0.95, col=2:6, lwd=2, label=F)
# shadowtext(tapply(AF_nmds_all$points[,1], eDNA2023$Zone, mean),
#            tapply(AF_nmds_all$points[,2], eDNA2023$Zone, mean),
#            labels=levels(eDNA2023$Zone), col=2:6, bg=7, r=0.15)
# mtext("(d)", side=3, line=-1.4, adj=0.03, cex=1.4)
# legend("bottomright", bty="n", legend=levels(eDNA2023$Zone), title="Zone",
#        col=c(1,3:6), pt.bg=2:6, lty=1, lwd=2, pch=NA, pt.cex=1.5)
# legend("bottomright", bty="n", legend=levels(eDNA2023$Zone), col=7, 
#        pt.bg=c(1,3:6), lty=NA, lwd=2, pch=21, pt.cex=2.)

