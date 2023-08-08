library(png)
addImg <- function(obj, x = NULL, y = NULL, width = NULL, interpolate = TRUE){
  if(is.null(x) | is.null(y) | is.null(width)){stop("Must provide args 'x', 'y', and 'width'")}
  USR <- par()$usr ; PIN <- par()$pin ; DIM <- dim(obj) ; ARp <- DIM[1]/DIM[2]
  WIDi <- width/(USR[2]-USR[1])*PIN[1] ;   HEIi <- WIDi * ARp 
  HEIu <- HEIi/PIN[2]*(USR[4]-USR[3]) 
  rasterImage(image = obj, xleft = x-(width/2), xright = x+(width/2),
              ybottom = y-(HEIu/2), ytop = y+(HEIu/2), interpolate = interpolate)
}

afs2021 <- read.csv("C:/Users/00028958/LocalData/R Projects/Learning R/afsed2021.csv", 
                    stringsAsFactors=TRUE)

source("C:/Users/00028958/LocalData/R Projects/Learning R/afr_map_FUNCTION.R")

palette(c("black", viridis::viridis(7)))
afrMap(xlim=c(399900,400680), ylim=c(6467850,6468400))
with(afs2021, points(Easting, Northing, asp=1,
                   pch=c(21:25,21,22)[Zone],
                   bg=seq(2,8)[Zone],
     cex=c(1.3,1,1,1,1,1.3,1)[Zone]))
legend("topright", bty="n", ncol=1, legend=levels(afs2021$Zone),
       pch=c(21:25,21,22), pt.bg=2:8, pt.cex=c(1.3,1,1,1,1,1.3,1),
       x.intersp = 0.5)
with(afs2021, 
     text(Easting, Northing, labels = afs2021$Zone, 
       pos=sample(seq(1,4), length(afs2021$Zone), replace = T), #random pos {:o}
       col = "red3", cex=0.67))
# see code at end if need to re-generate zones
par(mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25,
    lend = "square", ljoin="mitre")
with(afs2021, plot(Easting, Northing, asp=1, col = seq(0,6)[Zone],
                   pch = seq(0,6)[Zone], lwd = 2), 
                   ylab = expression(bold(paste(log[10],"[As (mg/kg)]"))))

# > names(afs2021[,10:44])
#  [1] "pH"   "EC"   "Al"   "As"   "Ba"   "Ca"   "Cd"   "Ce"   "Co"   "Cr"  
# [11] "Cu"   "Fe"   "Ga"   "Gd"   "K"    "La"   "Li"   "Mg"   "Mn"   "Mo"  
# [21] "Na"   "Nd"   "Ni"   "P"    "Pb"   "Rb"   "S"    "Sc"   "Sr"   "Th"  
# [31] "Ti"   "V"    "Y"    "Zn"   "Zone"

# swap As with As
# swap Cu with Gd
# swap Zn with Ni

require(car)

spm(~Al+Fe+As+Gd+Ni|Zone, data = afs2021, smooth=F, log = "y")

afs2021$As.log <- log10(afs2021$As)
afs2021$Al.log <- log10(afs2021$Al)
# afs2021$Gd[56] <- NA
afs2021$Gd.log <- log10(afs2021$Gd)
afs2021$Fe.log <- log10(afs2021$Fe)
afs2021$Pb.log <- log10(afs2021$Pb)
afs2021$Ni.log <- log10(afs2021$Ni)

with(afs2021, shapiro.test(As)); with(afs2021, shapiro.test(As.log))
with(afs2021, shapiro.test(Al)); with(afs2021, shapiro.test(Al.log))
with(afs2021, shapiro.test(Gd)); with(afs2021, shapiro.test(Gd.log))
with(afs2021, shapiro.test(Fe)); with(afs2021, shapiro.test(Fe.log))
with(afs2021, shapiro.test(Pb)); with(afs2021, shapiro.test(Pb.log))
with(afs2021, shapiro.test(Ni)); with(afs2021, shapiro.test(Ni.log))

par(mfrow = c(3,2), 
    mar = c(3,3,1,1), mgp = c(1.7,0.3,0), tcl = 0.25, 
    lend = "square", ljoin="mitre")
palette(c("black","blue","purple","darkcyan","sienna",
          "gray50","#008000","red3"))
carPalette(palette())
with (afs2021, plot(As ~ Fe, log="y", pch=c(0,15,1,19,2,17,3)[Zone], 
                    col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(As.log ~ Fe, 
                          subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
legend("bottomright", legend=levels(afs2021$Zone), bty="n", ncol = 2,
       pch = c(0,15,1,19,2,17,3), col = c(2:8))

with (afs2021, plot(As ~ Al, log="y", pch=c(0,15,1,19,2,17,3)[Zone], 
                    col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(As.log ~ Al, 
                          subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
with (afs2021, plot(Gd ~ Fe, log="y", pch=c(0,15,1,19,2,17,3)[Zone], 
                    col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(Gd.log ~ Fe, 
                          subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
with (afs2021, plot(Gd ~ Al, log="y", pch=c(0,15,1,19,2,17,3)[Zone], 
                    col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(Gd.log ~ Al, 
                          subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
with (afs2021, plot(Ni ~ Fe, log="y", 
                    pch=c(0,15,1,19,2,17,3)[Zone], col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(Ni.log ~ Fe, 
                          subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
with (afs2021, plot(Ni ~ Al, log="y", pch=c(0,15,1,19,2,17,3)[Zone], 
                    col = c(2:8)[Zone]))
for (i in 1:nlevels(afs2021$Zone)) {
  with(afs2021, abline(lm(Ni.log ~ Al, 
                          subset=afs2021$Zone==levels(afs2021$Zone)[i]),
                       col = i+1, lty = i))}
par(mfrow = c(1,1))

lmAsFe_simp <- with(afs2021, lm(As.log ~ Fe))
summary(lmAsFe_simp)
lmAsFe_Zone <- with(afs2021, lm(As.log ~ Fe * Zone))
summary(lmAsFe_Zone)
sp(As ~ Fe | Zone, smooth=F, data=afs2021, log="y", 
   legend=list(coords="bottomright"), reset.par=F)
anova(lmAsFe_simp,lmAsFe_Zone)

lmGdFe_simp <- with(afs2021, lm(Gd.log ~ Fe))
summary(lmGdFe_simp)
lmGdFe_Zone <- with(afs2021, lm(Gd.log ~ Fe * Zone))
summary(lmGdFe_Zone)
anova(lmGdFe_simp,lmGdFe_Zone)
sp(Gd ~ Fe | Zone, smooth=F, data=afs2021, log="y", 
   legend=list(coords="bottomright"), reset.par=F)

lmNiFe_simp <- with(afs2021, lm(Ni.log ~ Fe))
summary(lmNiFe_simp)
lmNiFe_Zone <- with(afs2021, lm(Ni.log ~ Fe * Zone))
summary(lmNiFe_Zone)
anova(lmNiFe_simp,lmNiFe_Zone)
sp(Ni ~ Fe | Zone, smooth=F, data=afs2021, log="y", 
   legend=list(coords="bottomright"), reset.par=F)

lmAsAl_simp <- with(afs2021, lm(As.log ~ Al))
summary(lmAsAl_simp)
lmAsAl_Zone <- with(afs2021, lm(As.log ~ Al * Zone))
summary(lmAsAl_Zone)
anova(lmAsAl_simp,lmAsAl_Zone)
sp(As ~ Al | Zone, smooth=F, data=afs2021, log="y", 
   legend=list(coords="bottomright"), reset.par=F)

lmGdAl_simp <- with(afs2021, lm(Gd.log ~ Al))
summary(lmGdAl_simp)
lmGdAl_Zone <- with(afs2021, lm(Gd.log ~ Al * Zone))
summary(lmGdAl_Zone)
anova(lmGdAl_simp,lmGdAl_Zone)
sp(Gd ~ Al | Zone, smooth=F, data=afs2021, log="y", 
   legend=list(coords="bottomright"), reset.par=F)

lmNiAl_simp <- with(afs2021, lm(Ni.log ~ Al))
summary(lmNiAl_simp)
lmNiAl_Zone <- with(afs2021, lm(Ni.log ~ Al * Zone))
summary(lmNiAl_Zone)
anova(lmNiAl_simp,lmNiAl_Zone)
sp(Ni ~ Al | Zone, smooth=F, data=afs2021, log="y", 
   legend=list(coords="bottomright"), reset.par=F)

library(lmtest)

diagnost <- data.frame(
  Parameter=c("R-squared","bgtest","bptest","raintest","outlierTest","shapiro.test"),
  lmAsFe_simp=c(summary(lmAsFe_simp)$adj,
                bgtest(lmAsFe_simp)$p.value,
                bptest(lmAsFe_simp)$p.value,
                raintest(lmAsFe_simp)$p.value,
                outlierTest(lmAsFe_simp)$p,
                shapiro.test(lmAsFe_simp$residuals)$p.value),
  lmGdAl_Zone=c(summary(lmGdAl_Zone)$adj,
                bgtest(lmGdAl_Zone)$p.value,
                bptest(lmGdAl_Zone)$p.value,
                raintest(lmGdAl_Zone)$p.value,
                outlierTest(lmGdAl_Zone)$p,
                shapiro.test(lmGdAl_Zone$residuals)$p.value),
  lmNiAl_simp=c(summary(lmNiAl_simp)$adj,
                bgtest(lmNiAl_simp)$p.value,
                bptest(lmNiAl_simp)$p.value,
                raintest(lmNiAl_simp)$p.value,
                outlierTest(lmNiAl_simp)$p,
                shapiro.test(lmNiAl_simp$residuals)$p.value)
)
print(diagnost, digits=3)

library(flextable)

set_flextable_defaults(digits=4, theme_fun = "theme_zebra")
ft <- flextable(diagnost) |> colformat_double(j = 2, digits=3) |>
  width(width = rep(1.5,4)) |>
  colformat_double(i=c(1:3,5), j = 3, digits=3) |>
  colformat_double(i=4:5, j = 3, digits=6) |>
  colformat_double(i=c(1:2,4:5), j = 4, digits=3) |>
  colformat_double(i=3:5, j = 4, digits=6) |>
  color(i = ~ lmAsFe_simp<0.05, j = ~lmAsFe_simp, color = "red3") |>
  color(i = ~ lmGdAl_Zone<0.05, j = ~lmGdAl_Zone, color = "red3") |>
  color(i = ~ lmNiAl_simp<0.05, j = ~lmNiAl_simp, color = "red3") |>
  set_header_labels(lmAsFe_simp="log(As)~Fe simple", lmGdAl_Zone="Gd~Al grouped", lmNiAl_simp="Ni~Al simple") |>
  set_caption(caption="Table 2: Diagnostic test p-values for the ‘best’ linear models for prediction of As, Gd, and Ni from Fe or Al in the Ashfield Flats 2021 sediment data.")
save_as_image(ft,"flextable2.png")

library(car)
par(oma=c(1,1,0,0), mgp=c(1.7,0.3,0), tcl=0.25)

png(file="AsFe.png", height=480, width=480)
sp(As ~ Fe, smooth=F, data = afs2021, boxplots=F, log="y", 
   xlab = "Fe (mg/kg)", ylab = "As (mg/kg)",
   cex=1.5, cex.axis=1.5, cex.lab=1.5, reset.par=F)
mtext("(a)",3,-2,adj = 0.02,cex=2,font=2)
dev.off()

png(file="GdAl.png", height=480, width=480)
sp(Gd ~ Al | Zone, smooth=F, data = afs2021, boxplots=F, log="y", 
   xlab = "Al (mg/kg)", ylab = "Gd (mg/kg)",
   legend=list(coords="bottomright", cex=1.4),
   cex=1.5, cex.axis=1.5, cex.lab=1.5, reset.par=F)
dev.off()

png(file="NiAl.png", height=480, width=480)
sp(Ni ~ Al, smooth=F, data = afs2021, boxplots=F, log="y", 
   xlab = "Al (mg/kg)", ylab = "Ni (mg/kg)",
   cex=1.5, cex.axis=1.5, cex.lab=1.5, reset.par=F)
dev.off()

par(mfrow=c(1,3), mar=c(0,0,0,0), oma=c(0,0,0,0))

plot(1,1,ann=F,xaxt="n",yaxt="n", type="n", bty="n")
graph <- readPNG("AsFe.png")
addImg(obj=graph,x=1,y=1,width=0.85)
plot(1,1,ann=F,xaxt="n",yaxt="n", type="n", bty="n")
graph <- readPNG("GdAl.png")
addImg(obj=graph,x=1,y=1,width=0.85)
plot(1,1,ann=F,xaxt="n",yaxt="n", type="n", bty="n")
graph <- readPNG("NiAl.png")
addImg(obj=graph,x=1,y=1,width=0.85)

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
