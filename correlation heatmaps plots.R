require(RcmdrMisc) # needed for rcorr.adjust() function
cormat1 <- rcorr.adjust(afs23[c("Fe","S.log","As.log","Co.log","Cr","Cu.log","Mo.log","Ni","Pb.log","Zn.log")], 
                        type="pearson", use="pairwise.complete")
cormat1
library(corrplot)
par(mfrow=c(1,1))
corrplot(cormat1$R$r, 
         col=colorRampPalette(c("gold3","gray96","dodgerblue"))(90), 
         method="ellipse", diag=F, addCoef.col = "black", 
         tl.col = 1, tl.cex=1.5, cl.cex=1.5, number.cex = 1.2, number.font = 1)
corrplot.mixed(cormat1$R$r,lower = "number", diag="n", 
               upper="ellipse", lower.col = "black", plotCI = "rect", 
               tl.col = 1, tl.cex=1.5, cl.cex=1.5, number.cex = 1.5, number.font = 1)

spm(~Fe+S.log+As.log+Co.log+Cr+Cu.log+Mg+Ni+Pb.log+Zn.log | pH_band, 
    data=afs23, diagonal=F, smooth=F, pch=c(15,0,2,3,4,1,19), 
    col=colorRampPalette(c("red3","gold","blue"))(7))

afs23$Fe.log <- log10(afs23$Fe)
afs23$S.log <- log10(afs23$S)
afs23$As.log <- log10(afs23$As)
afs23$Co.log <- log10(afs23$Co)
afs23$Cr.log <- log10(afs23$Cr)
afs23$Cu.log <- log10(afs23$Cu)
afs23$Mo.log <- log10(afs23$Mo)
afs23$Ni.log <- log10(afs23$Ni)
afs23$Pb.log <- log10(afs23$Pb)
afs23$Zn.log <- log10(afs23$Zn)
