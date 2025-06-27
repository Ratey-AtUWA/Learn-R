# Al - raw
# Ca - log
# Fe - raw
# K - raw
# Mg - sqrt
# Na - ^0.333
# pH - raw
# S - log
# As - log
# Co - log
# Cr - raw
# Cu - log
# Ni - raw
# Pb - log
# Pb - sqrt

v1 <- "Pb"
shapiro.test(afs1923[,v1])
shapiro.test(log10(afs1923[,v1]))
(pt <- powerTransform(afs1923[,v1])$lambda)
shapiro.test(afs1923[,v1]^pt)
par(mfrow=c(1,3), mar=c(3,3,3,1), mgp=c(1.3,0.2,0), tcl=0.2, font.lab=2)
hist(afs1923[,v1], xlab=v1, main=paste(v1,"histogram"))
hist(log10(afs1923[,v1]), xlab=paste0("log-",v1),
     main=paste0("log-",v1," histogram"))
hist(afs1923[,v1]^pt, xlab=paste0("power-transformed-",v1),
     main=paste0("power-transformed-",v1," histogram"))

wcdrains <- st_read("../shapefiles/watercorpDrains/water_corp_drains.shp")
wcdrainsO <- st_read("../shapefiles/DrainsOpen/Drain_Open_Channel_WCORP_083.shp")
wcdrainsP <- st_read("../shapefiles/Drainage_Pipes/Drainage_Pipes.shp")
str(wcdrainsO)
str(wcdrainsP)
c(grep("KITCHENER", wcdrains$name), grep("CHAPMAN", wcdrains$name))
c(grep("KITCHENER", wcdrains$DRN_MAINNA), grep("CHAPMAN", wcdrains$DRN_MAINNA))
afdrains <- wcdrains[c(grep("KITCHENER", wcdrains$DRN_MAINNA), grep("CHAPMAN", wcdrains$DRN_MAINNA)),]
plot(afdrains[2])

newPreds <-
  data.frame(Al=seq(0, max(afs1923_multreg$Al), l=100),
             Fe=seq(0, max(afs1923_multreg$Fe), l=100),
             K=seq(0, max(afs1923_multreg$K), l=100),
             pH=seq(min(afs1923_multreg$pH)*0.9, max(afs1923_multreg$pH), l=100))

conf1 <- predict(lm_stepwise, newPreds, interval = "conf")
conf1 <- conf1[order(conf1[,1]),]
pred1 <- predict(lm_stepwise, newPreds, interval="prediction")
pred1 <- pred1[order(pred1[,1]),]
par(mar=c(4,4,1,1), mgp=c(2,0.5,0), font.lab=2, cex.lab=1,
    lend="square", ljoin="mitre")
plot(afs1923_multreg$Pb.pow ~ lm_stepwise$fitted.values,
     xlab="Pb.pow predicted from regression model",
     ylab="Pb.pow measured values", type="n")
mtext(side=3, line=-5.5, adj=0.05, col="blue3",
      text=paste("Adjusted Rsq =",signif(summary(lm_stepwise)$adj.r.squared,3)))
# lines(conf1[,1], conf1[,2], lty=2, col="red")
# lines(conf1[,1], conf1[,3], lty=2, col="red")
polygon(c(pred1[,1],rev(pred1[,1])), c(pred1[,2],rev(pred1[,3])),
        col="#0000ff40", border = "transparent")
polygon(c(conf1[,1],rev(conf1[,1])), c(conf1[,2],rev(conf1[,3])),
        col="#ff000040", border = "transparent")
abline(0,1, col="gold4", lty=2, lwd=2)
points(afs1923_multreg$Pb.pow ~ lm_stepwise$fitted.values,
       pch=3, lwd=2, cex=0.8, col="blue3")
legend("topleft", legend=c("Observations","1:1 line"), col=c("blue3","gold4"),
       text.col=c("blue3","gold4"), pch=c(3,NA), lty=c(NA,2), pt.lwd=2, lwd=2,
       box.col="grey", box.lwd=2, inset=0.02, seg.len=2.7, y.intersp=1.2)
legend("bottomright", bty="n", title="Intervals", pch=15, pt.cex=2,
       legend=c("95% confidence", "95% prediction"), col=c("#ff000040", "#0000ff40"))


# -=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-

# strip chart with row numbers instead of symbols ####
par(mar=c(3,6,1,1), mgp=c(1.3,0.2,0),tcl=0.2,font.lab=2,las=1)
with(sv2017[which(sv2017$Type=="Sediment"),],
     plot(sv2017$Fe, jitter(rep(1, length(sv2017$Fe)),5),
          log="x", type="n", xlab="Fe", ylab="",
          xlim=c(700,6000), ylim=c(0.4,3.6),  yaxt="n"))
axis(2, las=1, at=1:3, labels=levels(sv2017$Type))
with(sv2017[which(sv2017$Type=="Sediment"),],
     text(sv2017$Fe[which(sv2017$Type=="Sediment")],
          jitter(rep(1, length(which(sv2017$Type=="Sediment"))),5),
            labels=which(sv2017$Type=="Sediment"), col=2, cex=0.7))
mtext("46",4,0,col=2,cex=0.7, adj=.2)
with(sv2017[which(sv2017$Type=="Soil"),],
     text(sv2017$Fe[which(sv2017$Type=="Soil")],
          jitter(rep(2, length(which(sv2017$Type=="Soil"))),8),
            labels=which(sv2017$Type=="Soil"), col=4, cex=0.7))
with(sv2017[which(sv2017$Type=="Street dust"),],
     text(sv2017$Fe[which(sv2017$Type=="Street dust")],
          jitter(rep(3, length(which(sv2017$Type=="Street dust"))),5),
            labels=which(sv2017$Type=="Street dust"), col=6, cex=0.7))

# -=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-=+#+=-

# large upper regression residuals = contamination? ####
library(sf)            # Simple Features spatial data in R
library(maptiles)      # get open-source map tiles for background maps
library(prettymapr)    # add scale bar and north arrows to maps
library(viridis)       # colourblind-friendly colour palettes
library(scico)         # more colourblind-friendly colour palettes

git <- "https://raw.githubusercontent.com/Ratey-AtUWA/"
afr_map <- read.csv(file=paste0(git,"spatial/main/afr_map_v2.csv"), 
                    stringsAsFactors = TRUE)
extent <- st_as_sf(data.frame(x=c(399900,400600),y=c(6467900,6468400)),
                   coords = c("x","y"), crs = UTM50S)
aftiles <- get_tiles(extent, provider = "OpenStreetMap.HOT", crop = TRUE)

RcmdrMisc::numSummary(afs1923[,c(17,18,24:50)])

regdata <- na.omit(afs1923[,c("Pb","pH","Al","Ca","Fe","S","P","Na","K","Mg")])
row.names(regdata) <- NULL
cat("original data has",nrow(afs1923),"rows, subset has",nrow(regdata),"rows")
cors <- cor(regdata)
cors[which(cors==1)] <- NA
print(cors, digits=2, na.print = "")

# Na, Mg correlated with r>0.8, so too collinear

regdata <- na.omit(afs1923[,c("Pb","pH","Al","Ca","Fe","S","P","Na","K")])
row.names(regdata) <- NULL
cat("original data has",nrow(afs1923),"rows, subset has",nrow(regdata),"rows")
spm(~pH+Al+Ca+Fe+S+P+Na+K, data=regdata, cex=0.5, smooth=FALSE)
regdata$Ca.log <- log10(regdata$Ca)
regdata$S.log <- log10(regdata$S)
regdata$P.log <- log10(regdata$P)
regdata$Na.log <- log10(regdata$Na)
spm(~pH+Al+Ca.log+Fe+S.log+P.log+Na.log+K, data=regdata, cex=0.5, smooth=FALSE)
cors <- cor(regdata[,c("Pb","pH","Al","Ca.log","Fe","S.log","P.log","Na.log","K")])
cors[which(cors==1)] <- NA
print(signif(cors,2), na.print = "")
lm0 <- lm(Pb ~ pH+Al+Ca.log+Fe+S.log+P.log+Na.log+K, data=regdata)
summary(lm0)
vif(lm0)

lm1 <- step(lm0, direction = "both")
summary(lm1)
par(mfrow=c(1,1), mgp=c(1.5,0.2,0), tcl=0.25, mar=c(3,3,0.5,0.5), lend="square", xpd=F)
plot(lm1$model$Pb ~ lm1$fitted.values); abline(0,1,col=4)
boxplot(scale(lm1$residuals), xlab="Pb", ylab="Standardised residual from multiple regression model ")
which(scale(lm1$residuals)>=2)
# just to check, run forward model
lm1a <- step(lm(Pb ~ 1, data=regdata), direction = "forward", 
             scope = (~ pH+Al+Ca.log+Fe+S.log+P.log+Na.log+K))
summary(lm1a)

lm1$model$Pb[which(scale(lm1$residuals)>=2)]
ISQG

par(mfrow=c(1,1), oma=c(0,0,0,0), lend="square", xpd=TRUE)
plot_tiles(aftiles, adjust=F, axes=TRUE, mar=c(3,3,0.5,0.5)) # use axes = TRUE

mtext("Easting (UTM Zone 50, m)", side = 1, line = 1.5, font=2)
mtext("Northing (UTM Zone 50, m)", side = 2, line = 1.5, font=2)
addnortharrow(text.col=1, border=1)
addscalebar(plotepsg = 32750, label.col = 1, linecol = 1, 
            label.cex = 1.2, htin=0.15, widthhint = 0.3)
with(afr_map, lines(drain_E, drain_N, col = "cadetblue", lwd = 2))
with(afr_map, polygon(wetland_E, wetland_N, col = "#5F9EA080", 
                      border="cadetblue", lwd = 1, lty = 1))
text(c(400263, 399962, 400047), c(6468174, 6468083, 6468237),
     labels = c("Chapman Drain","Kitchener Drain", "Woolcock Drain"),
     pos = c(2,2,4), cex = 0.8, font = 3, col = "cadetblue")
clrz <- plasma(15)[6:15]
with(afs1923[which(scale(lm1$residuals)>=2),], 
     points(Easting, Northing, pch=21, col="#000000b0", bg="#ffe000b0", cex=1.4, lwd=2))
with(afs1923[which(scale(lm1$residuals)>=2),], 
     text(Easting, Northing, labels=signif(Pb,2), cex=0.8, 
          pos=c(2,4,2,2,2,2,4,2), offset=0.3))
legend("bottomright", bty="n", y.intersp=1.75, 
       legend=c("Sediment samples with \nPb standardised residuals ≥ 2",
                "Numbers next to points are\nPb concentrations (mg/kg)"),
       pch = c(21,NA), col = c("#000000b0","black"), pt.bg = "#ffe000b0", 
       inset = c(0.02,0.04), pt.cex=c(1.4,0.8), pt.lwd=2)

# are unusual residuals from boxplot same as above prediction interval?
par(mfrow=c(1,1), mgp=c(1.5,0.2,0), tcl=0.25, mar=c(3,3,0.5,0.5), las=1, lend="square", xpd=F)
plot(lm1$model$Pb.log ~ lm1$fitted.values,
     xlab=expression(bold(paste(log[10],"Pb predicted by model"))),
     ylab=expression(bold(paste("Observed ",log[10],"Pb"))),
     pch=19, cex=1.2, col="#0000ff40"); abline(0,1,col="sienna")
hipts <- which(scale(lm1$residuals)>2)
points(lm1$model$Pb.log[hipts] ~ lm1$fitted.values[hipts], pch=21, bg="gold")
newdata <- data.frame(pH=seq(min(regdata$pH),max(regdata$pH),l=50),
                      Al=seq(min(regdata$Al),max(regdata$Al),l=50),
                      Ca.log=seq(min(regdata$Ca.log),max(regdata$Ca.log),l=50),
                      Fe=seq(min(regdata$Fe),max(regdata$Fe),l=50),
                      P.log=seq(min(regdata$P.log),max(regdata$P.log),l=50),
                      Na.log=seq(min(regdata$Na.log),max(regdata$Na.log),l=50))
conffit <- predict(lm1, interval = "prediction", level=0.95)
conf0 <- conffit[order(conffit[,1]),]
lines(conf0[,1], conf0[,2], col="grey", lty=3, lwd=2)
lines(conf0[,1], conf0[,3], col="grey", lty=3, lwd=2)
legend("topleft", bty="n", inset=0.01, pt.cex=1.2, pch=c(19,21,NA,NA), 
       legend=c("Observations","Obs. with scaled residuals > 2","1:1 line","± 95% prediction"),
       col=c("#0000ff40",1,"sienna","grey"), lty=c(NA,NA,1,3),lwd=c(NA,NA,1,2),
       pt.bg=c(NA,"gold",NA,NA))

# car::scatter3d test for PCA ####
library(car)
carPalette(c("black","purple","darkcyan","sienna"))
scatter3d(pca_ashfield_open$x[,1],
          pca_ashfield_open$x[,2],
          pca_ashfield_open$x[,3], 
          xlab="PC1",ylab="PC2",zlab="PC3", surface.alpha=0, 
          pos.res.col="transparent", neg.res.col="transparent",
          point.col=c("dimgrey","gold","dodgerblue")[data0$Type],
          # ellipsoid=T, groups=data0$Type, ellipsoid.alpha=0.06,
          col=c("dimgrey","gold","dodgerblue"))

# PCA other dims loop ####
palette(c("black","moccasin","cyan2","navy","firebrick"))
par(mfrow=c(3,2), mar = c(3,3,3,3), oma = c(0,0,0,0), 
    mgp=c(1.3,0.2,0), tcl = 0.25, font.lab=2, cex.lab=1.2,
    lend = "square", ljoin = "mitre")
v1list <- c(1,1,2,2,3) ; v2list <- c(3,4,3,4,4)
s0 <- 0.09 # components & scaling factor for this plot
for(i in 1:5){
  v1 <- v1list[i]; v2 <- v2list[i]; s0s <- c(0.09,0.12,0.12,0.12,0.13)
  biplot(pca_ashfield_open, choices = c(v1,v2), col = c("transparent",5), cex=c(0.8,1),
         pc.biplot = FALSE, scale = 0.2, arrow.len = 0.08,
         # xlim = c(-1.2,3.2), ylim = c(-3.5,1.7),
         xlab = paste0("Scaled PC",v1," Component Loadings"),
         ylab = paste0("Scaled PC",v2," Component Loadings")) 
  mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.3, font = 2, cex=0.8)
  mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.3, font = 2, cex=0.8)
  points(pca_ashfield_open$x[,v1]*s0s[i], pca_ashfield_open$x[,v2]*s0s[i],
         pch = c(22,21,24)[data0$Type],
         lwd = 1, bg = c(2,3,4)[data0$Type],
         col = 1,
         cex = c(1.2,1.4,1)[data0$Type])
  mtext(paste0("(",letters[i],")"), side = 3, line = -1.5, font = 2, adj = 0.02)
}
plot(0:1,0:1, type="n",bty="n",axes=F,ann=F)
legend("topleft", bty = "o", inset = 0.03,
       box.col = "gray", box.lwd = 2, bg = "white",
       legend = levels(data0$Type), title="Sample Type",
       pch = c(22,21,24), pt.lwd = 1,
       col = 1, pt.bg = c(2,3,4),
       pt.cex = c(1.2, 1.4, 1)*1.4,
       cex = 1.4, y.intersp = 0.9)

par(mfrow=c(1,1), mgp=c(1.6,0.2,0), font.lab=2)
s3d <- scatterplot3d(x=pca_ashfield_open$rotation[,1], 
                     y=pca_ashfield_open$rotation[,2], 
                     z=pca_ashfield_open$rotation[,3], mar=c(3,3,0,2.6),
                     box=T, grid=T, type="h", col.axis="grey", angle=25, scale.y = 0.8, 
                     pch = 1, cex.symbols=3, color="gray",
                     xlab="PC1", ylab="PC2",zlab="PC3")
text(s3d$xyz.convert(pca_ashfield_open$rotation[, 1:3]), labels = rownames(pca_ashfield_open$rotation),
     cex= 0.7)

# try deleting p>0.05 from PERMANOVA pairwise matrix before displaying
for(i in 1:ncol(AF_plainPW_all)){
  AF_plainPW_all[which(AF_plainPW_all[,i]>0.05),i] <- "ns"
  }
flextable(cbind(s0,AF_plainPW_all)) |> width(width=0.5) |> theme_booktabs() |> 
  bold(bold=T,part="header") |> bold(bold=T, j=1) |> 
  set_caption(caption="Table: Pairwise comparison matrix based on PERMANOVA testing the effect of Site on multivariate species presence-absence (ns = p > 0.05).", align_with_table = F)


# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# leave-one-out cross validation for LDA ####
data0 <- Hallberg_clr
data0[,11:24] <- scale(data0[,11:24]) # scale just numeric variables
lda_rock_open <- lda(formula = Rock ~ SiO2 + TiO2 + Al2O3 + Fe2O3 + FeO + MnO + 
                       MgO + CaO + Na2O + K2O + P2O5, 
                     data = data0,
                     prior = as.numeric(summary(data0$Rock))/nrow(data0), 
                     CV=TRUE) 
print(lda_rock_open)
preds <- apply(lda_rock_open$posterior, MARGIN = 1,
      FUN = function(x){levels(lda_rock_open$class)[which(x==max(x, na.rm=TRUE))]})
obsvs <- data0$Rock
matches <- data.frame(Pred_class=preds, Actual_class=obsvs, Match=preds==obsvs)
flextable(matches[1:10,]) |> bold(part="header") |> width(width=c(4.5,2.5,3.5), unit="cm") |> 
  set_header_labels(Pred_class="Predicted class", Actual_class="Actual class", 
                    Match="Is LDA correct?") |> 
  set_caption(caption="Table 1: Matches between LDA model and actual data based on clr-transformed major element concentrations in the whole rock dataset compiled by Hallberg (2006).")

#
# stepwise LDA ####

minimp <- c(0.0005,0.001,0.002,0.003,0.005,0.01)
for(j in 1:length(minimp)){
  stepwise.lda <- stepclass(formula = Rock ~ SiO2 + TiO2 + Al2O3 + Fe2O3 + FeO + 
                            MnO + MgO + CaO + Na2O + K2O + P2O5, 
                            data = data0, output=T, 
                            prior=as.numeric(summary(data0$Rock))/nrow(data0), 
                            method="lda", improvement=minimp[j], 
                            direction="both", criterion="AS")
  }


directs <- c("backward","forward","both")
minimp <- c(0.001,0.002,0.003,0.005,0.01) # ,0.02,0.03,0.05
crits=c("CR","AC","AS","CF") # "CFvec",
cat("Sort order, Direction of steps",",", "Improvement tolerance",",","Improvement Criterion",
    ",", "Final predictor variables",",","Criterion value\n")
for (k in 1:3) {
  for (j in 1:5) {
    for (i in 1:4){
      # cat("\n",j,i,"Stepwise LDA, min. improvement tolerance =",minimp[j], ", Direction = both,
      #   Criterion = ", crits[i],"\n")
      stepwise.lda <- stepclass(formula = Type ~ Compact + Open + Lightweight + Industry, 
                                data=cities, output=FALSE, 
                                prior=as.numeric(summary(cities$Type))/
                                  nrow(cities), 
                                method="lda", improvement=minimp[j], direction=directs[k], 
                                criterion=crits[i])
      # print(stepwise.lda)
      finvars <- as.character(stepwise.lda$formula[3]) 
      finperf <- as.numeric(stepwise.lda$process[nrow(stepwise.lda$process),4])
      perfmeas <- stepwise.lda$performance.measure
      cat(((k*100)+((j*10)+i)),",",directs[k],",",minimp[j],",",perfmeas," (",crits[i],") ",
          ",", finvars,",", finperf,"\n")
    }
  }
}
# end nested loop
rm(list=c('directs','minimp','crits','i','j','k','finvars','finperf','perfmeas'))

# -=-=-=-=-=-=-=-
# check LDA observation score plot options ####

par(mfrow = c(2,2), mar = c(3.5,3.5,1,1), mgp = c(1.5,0.3,0), tcl = 0.25,
    lend = "square", ljoin = "mitre", cex.main = 0.9, font.lab=2)
palette(c("black", viridis::viridis(6, alp=0.7, end=0.99), "gray","transparent"))

plot(ldaPred_rock_clos$x[,1], ldaPred_rock_clos$x[,2], 
     bg=c(2:7)[Hallberg$Rock],
     pch=c(21:25,21)[Hallberg$Rock], lwd=1, cex = 1.5, 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]")
abline(v=0,col="grey",lty=2); abline(h=0,col="grey",lty=2)
mtext("Closed, actual", 1, -1.5, adj = 0.95, cex = 1.2, font = 2)

plot(ldaPred_rock_open$x[,1], ldaPred_rock_open$x[,2], 
     bg=c(2:7)[Hallberg$Rock],
     pch=c(21:25,21)[Hallberg$Rock], lwd=1, cex = 1.5, 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]")
abline(v=0,col="grey",lty=2); abline(h=0,col="grey",lty=2)
mtext("Open, actual", 3, -1.5, adj = 0.95, cex = 1.2, font = 2)

plot(ldaPred_rock_clos$x[,1], ldaPred_rock_clos$x[,2], 
     bg=c(2:7)[ldaPred_rock_clos$class],
     pch=c(21:25,21)[ldaPred_rock_clos$class], lwd=1, cex = 1.5, 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]")
abline(v=0,col="grey",lty=2); abline(h=0,col="grey",lty=2)
mtext("Closed, predicted", 1, -1.5, adj = 0.95, cex = 1.2, font = 2)

plot(ldaPred_rock_open$x[,1], ldaPred_rock_open$x[,2], 
     bg=c(2:7)[ldaPred_rock_open$class],
     pch=c(21:25,21)[ldaPred_rock_open$class], lwd=1, cex = 1.5, 
     xlab="Linear Discriminant [1]", ylab="Linear Discriminant [2]")
abline(v=0,col="grey",lty=2); abline(h=0,col="grey",lty=2)
mtext("Open, predicted", 3, -1.5, adj = 0.95, cex = 1.2, font = 2)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# try a factor to split eDNA ordinations ####
# coords <- read.csv("../Ashfield_Flats/eDNA_locations.csv")
# coords
# eDNA2022$Easting <- rep(coords[,2], each=4)
# eDNA2022$Northing <- rep(coords[,3], each=4)
# with(eDNA2022, plot(Easting, Northing, asp=1, col="grey"))
# with(eDNA2022, text(Easting, Northing, labels=Site))
# colnames(eDNA2022)
# eDNA2022 <- data.frame(eDNA2022[,1:3],eDNA2022[,26:27],eDNA2022[,4:25])
# write.csv(eDNA2022, file="eDNA2022.csv", row.names = FALSE)

eDNA2022$Zone <- cut(eDNA2022$Northing, breaks = c(1,6468100,9999999), labels=c("South","North"))
# eDNA2022$Zone <- cut(eDNA2022$Easting, breaks = c(1,400100,99999999), labels=c("West","East"))

library(sf)
library(maptiles)
extent <- st_as_sf(data.frame(x=c(399920,400670), y=c(6467950,6468516)),
                   coords = c("x","y"), crs=st_crs(32750))
afmap <- get_tiles(extent, "Esri.WorldImagery", 17, crop=TRUE)
par(oma=c(0,0,0,0), mgp=c(1.5,0.2,0))
plot_tiles(afmap, axes=TRUE, mar=c(3,3,1,1))
plot(eDNA2022, add=T)

plot(AF_nmds_all, display="sites", cex=1.4, xlim=c(-1.5,1.5), ylim=c(-1.5,1.5))
mtext("(a)", side=3, line=-1.6, adj=0.03, cex=1.4)
points(AF_nmds_all$points, col=c(2,5)[eDNA2022$Zone], pch=19, cex=1.4)
ordiellipse(AF_nmds_all, groups=eDNA2022$Zone, col=c(2,5), lwd=2, 
            kind = "sd", conf=0.75)
shadowtext(tapply(AF_nmds_all$points[,1], eDNA2022$Zone, mean),
           tapply(AF_nmds_all$points[,2], eDNA2022$Zone, mean),
           labels=levels(eDNA2022$Zone), cex=1.2, col=c(2,5), bg=7, r=0.2)
legend("bottomright", bty="n", legend=levels(eDNA2022$Zone), title="Zone",
       col=c(2,5), pt.bg=c(2,4,5), lty=1, lwd=2, pch=NA, pt.cex=1.5, cex=1.2)
legend("bottomright", bty="n", legend=levels(eDNA2022$Zone), col=7, 
       pt.bg=c(2,5), lty=NA, lwd=2, pch=21, pt.cex=2.2, cex=1.2)

par(oma=c(0,0,0,0), mar=c(3,3,1,1), mgp=c(1.5,0.2,0), xpd=F, font.lab=2,
    tcl=-0.15)
plot(st_coordinates(LCloc), xaxs="i", yaxs="i", type="n", asp=1,
     xlab="Easting (UTM Zone 50, m)", ylab="Northing (UTM Zone 50, m)",
     cex.axis=0.85)
plot_tiles(LCloc_map, add=TRUE)
plot(wcdrains, add=T, type="l",col="purple")

# LCdrains <- wcdrains[c(grep("CHAPMAN",wcdrains$DRN_MAINNA),
#                        grep("KITCHENER",wcdrains$DRN_MAINNA)),]
c(grep("ROAD",wcdrains$DRN_MAINNA))
  
# make better drains obj ####
lcd <- st_union(lcd1,lcd2,lcd20,lcd3,lcd4,lcd5,lcd6,lcd7,lcd8,lcd9,lcd10,lcd11,lcd12,lcd13,lcd14,lcd15,lcd16,lcd17,lcd18,lcd19,lcd20)
lcd <- st_union(lcd1,lcd2,lcd3,lcd4,lcd5)

lcd <- st_multilinestring(x= list(as.matrix(st_drop_geometry(lcd1[,1:2])),
                                  as.matrix(st_drop_geometry(lcd2[,1:2])),
                                  as.matrix(st_drop_geometry(lcd3[,1:2])),
                                  as.matrix(st_drop_geometry(lcd4[,1:2])),
                                  as.matrix(st_drop_geometry(lcd5[,1:2])),
                                  as.matrix(st_drop_geometry(lcd6[,1:2])),
                                  as.matrix(st_drop_geometry(lcd7[,1:2])),
                                  as.matrix(st_drop_geometry(lcd8[,1:2])),
                                  as.matrix(st_drop_geometry(lcd9[,1:2])),
                                  as.matrix(st_drop_geometry(lcd10[,1:2])),
                                  as.matrix(st_drop_geometry(lcd11[,1:2])),
                                  as.matrix(st_drop_geometry(lcd12[,1:2])),
                                  as.matrix(st_drop_geometry(lcd13[,1:2])),
                                  as.matrix(st_drop_geometry(lcd14[,1:2])),
                                  as.matrix(st_drop_geometry(lcd15[,1:2])),
                                  as.matrix(st_drop_geometry(lcd16[,1:2])),
                                  as.matrix(st_drop_geometry(lcd17[,1:2])),
                                  as.matrix(st_drop_geometry(lcd18[,1:2])),
                                  as.matrix(st_drop_geometry(lcd19[,1:2])),
                                  as.matrix(st_drop_geometry(lcd20[,1:2]))
                                  ), crs=st_crs(32750))
plot(lcd,add=T)

par(oma=c(0,0,0,0), mar=c(3,3,1,1), mgp=c(1.5,0.2,0), xpd=F, font.lab=2, tcl=-0.15, lend=2) ; plot(st_coordinates(LCloc), xaxs="i", yaxs="i", type="n", asp=1, xlab="Easting (UTM Zone 50, m)", ylab="Northing (UTM Zone 50, m)", cex.axis=0.85) ; plot_tiles(LC_locMap, add=TRUE); addnortharrow() ; addscalebar(plotepsg = 32750, pos="bottomright", label.col = 1, linecol = 1, label.cex = 1.2, htin=0.15, widthhint=0.3, padin=c(0.15,0.2)) ; shadowtext(401900,6467200, labels="Perth\nAirport", col=1, bg=11, r=0.07) ; shadowtext(399635,6468960, labels="Midland Railway", col=1, bg=11, r=0.07, srt=47) ; shadowtext(400260, 6468180, labels="Ashfield\nFlats", col=13, bg="#DAECBC", r=0.3) ; shadowtext(399300, 6469200, labels="Tonkin\nIndustrial\nPark", col=4,  bg="#E5E0DA") ; text(401090, 6468020, labels="Swan River", col=3, font=3,srt=60)
# ...continuing previous code...
# plot(wcdrains[6], add=TRUE, type="l", pal=c("darkcyan"), 
#      lwd=2)
plot(DoWdrains[2], add=TRUE, type="l", pal=c("steelblue"),
     lwd=1)
plot(dow2[2], add=TRUE, type="l", pal=c("navy"),
     lwd=2)
# plot(lcd, add=TRUE, type="l", col="purple", lwd=2)
legend("bottomleft", box.col=12, bg="#ffffffa0", 
       title=expression(bold("Stormwater Main Drains")),
       legend=c("Open earthen drains","Underground pipe drains"), 
       col=c("dodgerblue", "gold3"), lwd=2)
shadowtext(400290, 6468130, labels="Ashfield\nFlats\nReserve", 
           col=4, bg="#DAECBC")

whextent <- st_as_sf(data.frame(Long=c(174.255,174.592), Lat=c(-35.869,-35.553)),
                     coords=c("Long","Lat"), crs=st_crs(4326))
whmap <- get_tiles(whextent, provider="Stadia.Stamen.Terrain", 
                   apikey = read.csv("../StadiaMaps-API.csv"), 
                   zoom=11, crop=T, forceDownload = TRUE)
plot_tiles(whmap)
