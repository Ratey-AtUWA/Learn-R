par(mar=c(4,4,1,1), oma=c(0,0,1,0), mfrow=c(2,2), mgp=c(1.7,0.3,0), tcl=0.25, 
    font.lab=2, cex=1.2)
with(datasets::anscombe, plot(x1,y1, pch=19, xlim=c(4,20), ylim = c(3,13),
                              col="#800000", col.axis="#800000", col.lab="#800000"))
mtext("Anscombe's Quartet",3,1,adj=1,cex=1.6,font=2)
lm1 <- with(datasets::anscombe, lm(y1~x1))
text(4,12.5, pos=4, col="#800000",
     labels=paste0("y = ",signif(lm1$coef[2],2),"x + ",signif(lm1$coef[1],2)))
text(4,11.5, pos=4, col="#800000",
     labels=paste0("R\u00B2 = ",signif(summary(lm1)$r.sq,2)))
abline(lm1, lwd = 2, col = "#80000080")
with(datasets::anscombe, plot(x2,y2, pch=19, xlim=c(4,20), ylim = c(3,13),
                              col="#000080", col.axis="#000080", col.lab="#000080"))
lm2 <- with(datasets::anscombe, lm(y2~x2))
text(4,12.5, pos=4, col="#000080",
     labels=paste0("y = ",signif(lm2$coef[2],2),"x + ",signif(lm2$coef[1],2)))
text(4,11.5, pos=4, col="#000080",
     labels=paste0("R\u00B2 = ",signif(summary(lm2)$r.sq,2)))
abline(lm2, lwd = 2, col = "#00008080")
with(datasets::anscombe, plot(x3,y3, pch=19, xlim=c(4,20), ylim = c(3,13),
     col="#500090", col.axis="#500090", col.lab="#500090"))
lm3 <- with(datasets::anscombe, lm(y3~x3))
text(4,12.5, pos=4, col="#500090",
     labels=paste0("y = ",signif(lm3$coef[2],2),"x + ",signif(lm3$coef[1],2)))
text(4,11.5, pos=4, col="#500090",
     labels=paste0("R\u00B2 = ",signif(summary(lm3)$r.sq,2)))
abline(lm3, lwd = 2, col = "#50009080")
with(datasets::anscombe, plot(x4,y4, pch=19, xlim=c(4,20), ylim = c(3,13),
     col="#005090", col.axis="#005090", col.lab="#005090"))
lm4 <- with(datasets::anscombe, lm(y4~x4))
text(4,12.5, pos=4, col="#005090",
     labels=paste0("y = ",signif(lm4$coef[2],2),"x + ",signif(lm4$coef[1],2)))
text(4,11.5, pos=4, col="#005090",
     labels=paste0("R\u00B2 = ",signif(summary(lm4)$r.sq,2)))
abline(lm4, lwd = 2, col = "#00509080")
