# R color chart
nc <- c(1:152,362:657)
par(mar=rep(0.5,4),xpd=TRUE)
pdf("RcolorChartNames.pdf",width=8.27, height=11.69,family = "Helvetica-Narrow")
plot(0:1,0:1, type="n", bty="n", axes=F,ann=F)
legend("topleft",ncol=6,pch=22,legend=paste0(nc," ",colors()[nc]), 
       pt.bg=colors()[nc], cex=0.65, pt.cex=1.8, bty="n",y.intersp = 1)
mtext("R Colour Chart – colour names", family="sans", font=2)
mtext("Colours 153 to 361 (gray0-gray100, grey0-grey100) are not shown", 
      side=1, family="sans", font=1)
dev.off()

col2hex <- function(cname)
{
  colMat <- col2rgb(cname)
  rgb(
    red=colMat[1,]/255,
    green=colMat[2,]/255,
    blue=colMat[3,]/255
  )
}
hexcols <- rep(NA, length(colors()))
for(i in 1:length(colors())){
  hexcols[i] <- col2hex(colors()[i])
}

pdf("RcolorChartHex.pdf",width=8.27, height=11.69,family = "Courier")
plot(0:1,0:1, type="n", bty="n", axes=F,ann=F)
legend("topleft",ncol=6,pch=22,legend=paste0(nc," ",hexcols[nc],"  "), 
       pt.bg=hexcols[nc], cex=0.65, pt.cex=1.8, bty="n",x.intersp = 1.2)
mtext("R Colour Chart – colour hexadecimal codes", family="sans", font=2)
mtext("Colours 153 to 361 (gray0-gray100, grey0-grey100) are not shown", 
      side=1, family="mono", font=1, cex=0.8)
dev.off()
