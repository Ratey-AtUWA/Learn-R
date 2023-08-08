dna22 <- read.table("clipboard", sep="\t", header = T)
new22 <- is.na(dna22$Easting)
adj22 <- which(dna22$Easting>1)

afrMap(xlim = c(399800,400700), ylim=c(6467900,6468450))
points(dna22[new22,4:5],pch=19,col="grey25")
text(dna22[new22,4:5]-1, labels=dna22[new22,"Site"], pos=c(3,1),col="white",cex=0.75)
text(dna22[new22,4:5]+1, labels=dna22[new22,"Site"], pos=c(3,1),col="white",cex=0.75)
text(dna22[new22,4:5], labels=dna22[new22,"Site"], pos=c(3,1),col="grey25",cex=0.75)
points(dna22[,7:8], pch =3, lwd = 2, col="red2", lend=2)
with(dna22[adj22,],
     text(Easting, Northing, labels = Site, pos=4, col = "red2"))
legend("bottomright", legend=c("Planned locations","Adjusted locations"),
       pch = c(19,3), pt.lwd = c(1,2), col = c("grey25","red2"),bty="n")
rm(list=c("new22","adj22"))
