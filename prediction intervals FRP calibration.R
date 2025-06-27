clines <- read.csv("FRP_calibs.csv")

namz <- colnames(clines)[2:5]
# newAbs <- data.frame(PO4.P=seq(-0.1,1.1,l=6))
newAbs <- data.frame(Abs.99=seq(-0.1,0.66,0.01),Abs.9990=seq(-0.1,0.66,0.01),
                     Abs.9995=seq(-0.1,0.66,0.01),Abs.9999=seq(-0.1,0.66,0.01))
par(mfrow=c(2,2), mar=c(3,2.5,0.25,0.25), mgp=c(1.3,0.2,0), tcl=0.2,
    font.lab=2)

# R-sq = 0.99 -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-
calib <- lm(PO4.P ~ Abs.99, data=clines)
conf0 <- predict(calib, newAbs, interval = "conf")
pred0 <- predict(calib, newAbs, interval = "predict")
plot(calib$model[,1] ~ calib$model[,2], ylim=c(-0.1,1.1),
     type="n", cex.lab=1.1, cex.axis=1,
     xlab="Absorbance", ylab=expression(bold(paste(PO[4],"-P"," (mg/L)"))))
polygon(c(newAbs$Abs.99,rev(newAbs$Abs.99)), c(pred0[,2], rev(pred0[,3])),
        col="#4040ff40", border="transparent")
polygon(c(newAbs$Abs.99,rev(newAbs$Abs.99)), c(conf0[,2], rev(conf0[,3])),
        col="#ff0000a0", border="transparent")
abline(calib)
points(calib$model[,1] ~ calib$model[,2], pch=19)
mtext(paste("R²=",round(summary(calib)$r.sq,4)), side=1, line=-1.5, adj=0.95,
      cex=1.5)
legend("topleft", bty="n", legend=c("95% confidence", "95% prediction"), cex=1.1,
       title=expression(italic("Intervals")),
       col=c("#e00020a0","#4040ff40"), pch=15, pt.cex=2, y.int=0.9)
lines(rep(newAbs[40,1],2),c(par("usr")[3],pred0[40,3]), lty=3, col="dimgrey")
lines(c(par("usr")[1],newAbs[40,1]), rep(pred0[40,3],2), lty=3, col="dimgrey")
lines(c(par("usr")[1],newAbs[40,1]), rep(pred0[40,2],2), lty=3, col="dimgrey")
arrows(0,pred0[40,2],0,pred0[40,3], col="dimgrey", length=0.1, angle=20, code=3)

# R-sq = 0.999 -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-
calib <- lm(PO4.P ~ Abs.9990, data=clines)
conf0 <- predict(calib, newAbs, interval = "conf")
pred0 <- predict(calib, newAbs, interval = "predict")
plot(calib$model[,1] ~ calib$model[,2], ylim=c(-0.1,1.1),
     type="n", cex.lab=1.1, cex.axis=1,
     xlab="Absorbance", ylab=expression(bold(paste(PO[4],"-P"," (mg/L)"))))
polygon(c(newAbs$Abs.9990,rev(newAbs$Abs.9990)), c(pred0[,2], rev(pred0[,3])),
        col="#4040ff40", border="transparent")
polygon(c(newAbs$Abs.9990,rev(newAbs$Abs.9990)), c(conf0[,2], rev(conf0[,3])),
        col="#ff0000a0", border="transparent")
abline(calib)
points(calib$model[,1] ~ calib$model[,2], pch=19)
mtext(paste("R²=",round(summary(calib)$r.sq,4)), side=1, line=-1.5, adj=0.95,
      cex=1.5)
legend("topleft", bty="n", legend=c("95% confidence", "95% prediction"), cex=1.1,
       title=expression(italic("Intervals")),
       col=c("#e00020a0","#4040ff40"), pch=15, pt.cex=2, y.int=0.9)
lines(rep(newAbs[40,1],2),c(par("usr")[3],pred0[40,3]), lty=3, col="dimgrey")
lines(c(par("usr")[1],newAbs[40,1]), rep(pred0[40,3],2), lty=3, col="dimgrey")
lines(c(par("usr")[1],newAbs[40,1]), rep(pred0[40,2],2), lty=3, col="dimgrey")
arrows(0,pred0[40,2],0,pred0[40,3], col="dimgrey", length=0.05, angle=20, code=3)

# R-sq = 0.9995 -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-
calib <- lm(PO4.P ~ Abs.9995, data=clines)
conf0 <- predict(calib, newAbs, interval = "conf")
pred0 <- predict(calib, newAbs, interval = "predict")
plot(calib$model[,1] ~ calib$model[,2], ylim=c(-0.1,1.1),
     type="n", cex.lab=1.1, cex.axis=1,
     xlab="Absorbance", ylab=expression(bold(paste(PO[4],"-P"," (mg/L)"))))
polygon(c(newAbs$Abs.9995,rev(newAbs$Abs.9995)), c(pred0[,2], rev(pred0[,3])),
        col="#4040ff40", border="transparent")
polygon(c(newAbs$Abs.9995,rev(newAbs$Abs.9995)), c(conf0[,2], rev(conf0[,3])),
        col="#ff0000a0", border="transparent")
abline(calib)
points(calib$model[,1] ~ calib$model[,2], pch=19)
mtext(paste("R²=",round(summary(calib)$r.sq,4)), side=1, line=-1.5, adj=0.95,
      cex=1.5)
legend("topleft", bty="n", legend=c("95% confidence", "95% prediction"), cex=1.1,
       title=expression(italic("Intervals")),
       col=c("#e00020a0","#4040ff40"), pch=15, pt.cex=2, y.int=0.9)
lines(rep(newAbs[40,1],2),c(par("usr")[3],pred0[40,3]), lty=3, col="dimgrey")
lines(c(par("usr")[1],newAbs[40,1]), rep(pred0[40,3],2), lty=3, col="dimgrey")
lines(c(par("usr")[1],newAbs[40,1]), rep(pred0[40,2],2), lty=3, col="dimgrey")
arrows(0,pred0[40,2],0,pred0[40,3], col="dimgrey", length=0.03, angle=20, code=3)

# R-sq = 0.9999 -=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-=+=-
calib <- lm(PO4.P ~ Abs.9999, data=clines)
conf0 <- predict(calib, newAbs, interval = "conf")
pred0 <- predict(calib, newAbs, interval = "predict")
plot(calib$model[,1] ~ calib$model[,2], ylim=c(-0.1,1.1),
     type="n", cex.lab=1.1, cex.axis=1,
     xlab="Absorbance", ylab=expression(bold(paste(PO[4],"-P"," (mg/L)"))))
polygon(c(newAbs$Abs.9999,rev(newAbs$Abs.9999)), c(pred0[,2], rev(pred0[,3])),
        col="#4040ff40", border="transparent")
polygon(c(newAbs$Abs.9999,rev(newAbs$Abs.9999)), c(conf0[,2], rev(conf0[,3])),
        col="#ff0000a0", border="transparent")
abline(calib)
points(calib$model[,1] ~ calib$model[,2], pch=19)
mtext(paste("R²=",round(summary(calib)$r.sq,4)), side=1, line=-1.5, adj=0.95,
      cex=1.5)
legend("topleft", bty="n", legend=c("95% confidence", "95% prediction"), cex=1.1,
       title=expression(italic("Intervals")),
       col=c("#e00020a0","#4040ff40"), pch=15, pt.cex=2, y.int=0.9)
lines(rep(newAbs[40,1],2),c(par("usr")[3],pred0[40,3]), lty=3, col="dimgrey")
lines(c(par("usr")[1],newAbs[40,1]), rep(pred0[40,3],2), lty=3, col="dimgrey")
lines(c(par("usr")[1],newAbs[40,1]), rep(pred0[40,2],2), lty=3, col="dimgrey")
arrows(0,pred0[40,2],0,pred0[40,3], col="dimgrey", length=0.02, angle=20, code=3)

# VERSION 2 - interval for fixed value of Absorbance

newAbs <- data.frame(Abs.99=0.29,Abs.9990=0.29,
                     Abs.9995=0.29,Abs.9999=0.29)
par(mfrow=c(2,2), mar=c(3,2.5,0.25,0.25), mgp=c(1.3,0.2,0), tcl=0.2,
    font.lab=2)

  calib <- lm(PO4.P ~ Abs.99, data=clines)
  conf0 <- predict(calib, newAbs, interval = "conf")
  pred0 <- predict(calib, newAbs, interval = "predict")
  plot(calib$model[,1] ~ calib$model[,2], ylim=c(-0.1,1.1),
       pch=19, cex.lab=1.1, cex.axis=1,
       xlab="Absorbance", ylab=expression(bold(paste(PO[4],"-P"," (mg/L)"))))
  abline(calib, col="darkgrey")
  arrows()
  mtext(paste("R²=",round(summary(calib)$r.sq,4)), side=1, line=-1.5, adj=0.95,
        cex=1.5)
  legend("topleft", bty="n", legend=c("95% confidence", "95% prediction"), cex=1.1,
         title=expression(italic("Intervals")),
         col=c("#e00020a0","#4040ff40"), pch=15, pt.cex=2, y.int=0.9)
