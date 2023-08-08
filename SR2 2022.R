afs2021 <- read.csv("C:/Users/00028958/LocalData/R Projects/Learning R/afsed2021.csv", 
                    stringsAsFactors=TRUE)
with(afs2021, plot(Easting, Northing, asp=1,
                 pch=seq(0,12)[Sample.ID]))
with(afs2021, 
     text(Easting, Northing, labels = afs2021$Zone, 
          pos=sample(seq(1,4), length(afs2021$Zone), replace = T), # randomised positions {:o}
          col = "red3", cex=0.67))
# see code at end if need to re-generate zones
par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25, lend = "square", ljoin="mitre")
with(afs2021, plot(Easting, Northing, asp=1,
                   col = seq(0,6)[Zone],
                   pch = seq(0,6)[Zone], lwd = 2))
require(car)
spm(~Al+Fe+As+Cu+Zn|Zone, data = afs2021, smooth=F, log = "xy")

afs2021$As.log <- log10(afs2021$As)
afs2021$Al.log <- log10(afs2021$Al)
afs2021$Cu[56] <- NA
afs2021$Cu.log <- log10(afs2021$Cu)
afs2021$Fe.log <- log10(afs2021$Fe)
afs2021$Pb.log <- log10(afs2021$Pb)
afs2021$Zn.log <- log10(afs2021$Zn)

with(afs2021, shapiro.test(As)); with(afs2021, shapiro.test(As.log))
with(afs2021, shapiro.test(Al)); with(afs2021, shapiro.test(Al.log))
with(afs2021, shapiro.test(Cu)); with(afs2021, shapiro.test(Cu.log))
with(afs2021, shapiro.test(Fe)); with(afs2021, shapiro.test(Fe.log))
with(afs2021, shapiro.test(Pb)); with(afs2021, shapiro.test(Pb.log))
with(afs2021, shapiro.test(Zn)); with(afs2021, shapiro.test(Zn.log))

par(mfrow = c(3,2), 
    mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25, 
    lend = "square", ljoin="mitre")
palette(c("black","blue","purple","darkcyan","sienna","gray50","#008000","red3"))
carPalette(palette())
with (afs2021, plot(As ~ Fe, log="y", pch=c(0,15,1,19,2,17,3)[Zone], col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(As.log ~ Fe, subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
legend("bottomright", legend=levels(afs2021$Zone), bty="n", ncol = 2,
       pch = c(0,15,1,19,2,17,3), col = c(2:8))
with (afs2021, plot(As ~ Al, log="y", pch=c(0,15,1,19,2,17,3)[Zone], col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(As.log ~ Al, subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
with (afs2021, plot(Pb ~ Fe, log="y", pch=c(0,15,1,19,2,17,3)[Zone], col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(Pb.log ~ Fe, subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
with (afs2021, plot(Pb ~ Al, log="y", pch=c(0,15,1,19,2,17,3)[Zone], col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(Pb.log ~ Al, subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
with (afs2021, plot(Zn ~ Fe, log="y", pch=c(0,15,1,19,2,17,3)[Zone], col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(Zn.log ~ Fe, subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
with (afs2021, plot(Zn ~ Al, log="y", pch=c(0,15,1,19,2,17,3)[Zone], col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(Zn.log ~ Al, subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
par(mfrow = c(1,1))

lmAsFe_simp <- with(afs2021, lm(As.log ~ Fe))
summary(lmAsFe_simp)
lmAsFe_Zone <- with(afs2021, lm(As.log ~ Fe * Zone))
summary(lmAsFe_Zone)
sp(As ~ Fe | Zone, smooth=F, data=afs2021, log="y", legend=list(coords="bottomright"), reset.par=F)
anova(lmAsFe_simp,lmAsFe_Zone)

lmPbFe_simp <- with(afs2021, lm(Pb.log ~ Fe))
summary(lmPbFe_simp)
lmPbFe_Zone <- with(afs2021, lm(Pb.log ~ Fe * Zone))
summary(lmPbFe_Zone)
anova(lmPbFe_simp,lmPbFe_Zone)
sp(Pb ~ Fe | Zone, smooth=F, data=afs2021, log="y", legend=list(coords="bottomright"), reset.par=F)

lmZnFe_simp <- with(afs2021, lm(Zn.log ~ Fe))
summary(lmZnFe_simp)
lmZnFe_Zone <- with(afs2021, lm(Zn.log ~ Fe * Zone))
summary(lmZnFe_Zone)
anova(lmZnFe_simp,lmZnFe_Zone)
sp(Zn ~ Fe | Zone, smooth=F, data=afs2021, log="y", legend=list(coords="bottomright"), reset.par=F)

lmAsAl_simp <- with(afs2021, lm(As.log ~ Al))
summary(lmAsAl_simp)
lmAsAl_Zone <- with(afs2021, lm(As.log ~ Al * Zone))
summary(lmAsAl_Zone)
anova(lmAsAl_simp,lmAsAl_Zone)
sp(As ~ Al | Zone, smooth=F, data=afs2021, log="y", legend=list(coords="bottomright"), reset.par=F)

lmPbAl_simp <- with(afs2021, lm(Pb.log ~ Al))
summary(lmPbAl_simp)
lmPbAl_Zone <- with(afs2021, lm(Pb.log ~ Al * Zone))
summary(lmPbAl_Zone)
anova(lmPbAl_simp,lmPbAl_Zone)
sp(Pb ~ Al | Zone, smooth=F, data=afs2021, log="y", legend=list(coords="bottomright"), reset.par=F)

lmZnAl_simp <- with(afs2021, lm(Zn.log ~ Al))
summary(lmZnAl_simp)
lmZnAl_Zone <- with(afs2021, lm(Zn.log ~ Al * Zone))
summary(lmZnAl_Zone)
anova(lmZnAl_simp,lmZnAl_Zone)
sp(Zn ~ Al | Zone, smooth=F, data=afs2021, log="y", legend=list(coords="bottomright"), reset.par=F)

# [end code]
# unused code to follow
# with(afs2021, 
#      text(Easting, Northing, labels = row.names(afs2021), 
#           pos=c(2,4), col = "blue", cex=0.67))

# afs2021$Zone <- rep(NA, NROW(afs2021))
# afs2021$Zone[1:9] <- rep("NW", length(1:9))
# afs2021$Zone[10:18] <- rep("N_transect", length(10:18))
# afs2021$Zone[c(19,51:55)] <- rep("SW_transect", length(c(19,51:55)))
# afs2021$Zone[56:61] <- rep("Woolcock", length(c(56:61)))
# afs2021$Zone[c(20,21,25:32)] <- rep("NE", length(c(20,21,25:32)))
# afs2021$Zone[c(22:24,29)] <- rep("NW", length(c(22:24,29)))
# afs2021$Zone[c(33:37,40:42,46,47)] <- rep("SW", length(c(33:37,40:42,46,47)))
# afs2021$Zone[c(38,39,43:45,48:50)] <- rep("NE", length(c(38,39,43:45,48:50)))
# afs2021$Zone[c(62:65)] <- rep("KMD", length(c(62:65)))
# afs2021$Zone <- as.factor(afs2021$Zone)
