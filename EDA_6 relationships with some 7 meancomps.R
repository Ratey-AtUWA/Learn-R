#### -+-+-+-+- EDA WORKFLOW 7: Comparing means -+-+-+-+- ####
#         ____ ___  ____     __  ____ ____ ___    ____ ____ _  _ ____ _  _                   
#         |___ |  \ |__|    |__] |__| |__/  |     [__  |___ |  | |___ |\ |                   
#         |___ |__/ |  |    |    |  | |  \  |     ___] |___  \/  |___ | \|                   
# _  _ ____ ____ _  _ ____    ____ ____ _  _ ___  ____ ____ _ ____ ____ _  _ ____ 
# |\/| |___ |__| |\ | [__     |    |  | |\/| |__] |__| |__/ | [__  |  | |\ | [__  
# |  | |___ |  | | \| ___]    |___ |__| |  | |    |  | |  \ | ___] |__| | \| ___]

# In some cases we will know the factor(s) which separate groups 
# in our dataset - for example different sampling zones within a
# stratified sampling design.
# In other instances we may find it useful to create factors based
# on our data, as discussed in Part 5 of this EDA workflow series.
# Excerpts from Part 5 (with some new code as well) are at the 
# bottom of this code file.

# We dealt with means comparisons in Part 5 to some extent, so 
# let's look at another strategy based initially on exploring 
# relationships!

# Comparing means of a new variable with a new factor! ####

# from one plot in a scatter plot matrix in EDA Workflow Part 6
library(car)
palette(c("black", rainbow(12, v = 0.75, end = 0.7)))
par(mar=c(3,3,1,1),mgp=c(1.7,0.3,0),tcl=0.3,font.lab=2,
    lend="square",ljoin="mitre")
sp(S~Na | Zone, cex=1.5, 
   data = afw20, log = "xy",smooth = FALSE, regLine=FALSE,
   legend = list(coords="topleft"), col = c(1,7,5,3,9), pch = seq(14,18))

# It looks like there are some points in the Kitchener Drain and 
# the NW and S wetlands that are above the overall trend in the data.
# This is interesting because excess sulfur, measured by the 
# sulfate/chloride ratio is diagnostic for active acid sulfate soils.

# Let's make some assumptions:
# 1. the S (sulfur) content from water analysis by ICP-OES is mainly sulfate;
# 2. chloride will be proportional to Na concentration in the same ratio as
#   seawater (which we know is true for rainwater, for example).
# So, 
# 1. sulfate concentration should be very close to sulfur*(96/32) = S*3
#    (since formula mass of sulfate ~96 g/mol, and atomic mass of sulfur ~32 g/mol)
# 2. chloride concentration should be approximately Na*(19356/10759) = Na*1.799
#    (since concentrations in mean seawater are Cl 19356 mg/L and Na 10759 mg/L)
# So using our water analysis data: 
#   SO4/Cl = (S*3)/(Na*1.799)

# So let's look at this with a couple of plots:
scatterplot(((S*3)/(Na*1.799))~seq(1,length(S)) | Zone, cex=1.5, 
   data = afw20,smooth = FALSE, regLine=FALSE,
   xlab = "Row number", ylab = "Sulfate/chloride ratio",
   legend = list(coords="topright"), 
   col = c(1,7,5,3,9), pch = seq(14,18))

par(mar = c(3,6,1,1), las = 1)
boxplot(((S*(96/32))/(Na*1.799)) ~ Zone, 
        data = afw20, horizontal = T,
        ylab="", col = c(11,7,5,3,9), cex.axis = 1.2)
abline(v=0.14, col = 12, lty = 2, lwd = 1)
text(0.14 ,2, pos=4, col = 12, cex = 0.85, offset = 0.15,
     labels="Mean sea\nwater:\nSO4/Cl=0.14")
abline(v=0.5, col = 2, lty = 2, lwd = 2)
text(0.5 ,5, pos=4, col = 2,
  labels = "Acid\nsulfate\ntrigger:\nSO4/Cl = 0.5")
par(mar = c(3,3,1,1), las = 1)

which(((afw20$S*(96/32))/(afw20$Na*1.799))>0.5)
which(((afw20$S*(96/32))/(afw20$Na*1.799))>0.14)
afw20$Zone[which(((afw20$S*(96/32))/
                   (afw20$Na*1.799))>0.5)]
afw20[which(((afw20$S*(96/32))/
               (afw20$Na*1.799))>0.5), c("Zone","pH")]

# APPENDIX ####
#  ____ _  _ ____ ____ ____ ___  ___ ____    ____ ____ ____ _  _ 
#  |___  \/  |    |___ |__/ |__]  |  [__     |___ |__/ |  | |\/| 
#  |___ _/\_ |___ |___ |  \ |     |  ___]    |    |  \ |__| |  | 
#                                                                
#              ___  ____ ____ ___    ____ _ _  _ ____            
#              |__] |__| |__/  |     |___ | |  | |___            
#              |    |  | |  \  |     |    |  \/  |___            
#                                                                
# make a new factor based on Ca conc.
sv18$Ca_split <- cut(sv18$Ca, 
                     breaks = c(0,10^3.6,999999), 
                     labels = c("low Ca","high Ca"))

# See if the 2 apparent populations correspond to
# differences in any other variables
# Initially try Gd and Zn

boxplot(log10(sv18$Zn) ~ sv18$Ca_split, notch = T)
# try a t-test, we could also use wilcox.test()
t.test(log10(sv18$Zn) ~ sv18$Ca_split)
tapply(sv18$Zn, sv18$Ca_split, mean, na.rm = TRUE)

# We can use a loop to use factor to test means for
# many variables:
# (i) make names vector and data frame before running loop
varz <- names(sv18)
table <- data.frame(Variable = varz[10:40],
                    t_test_p = rep(NA,length(10:40)), 
                    Wilcox_p = rep(NA,length(10:40)),
                    Mean_near = rep(NA,length(10:40)), 
                    Mean_far = rep(NA,length(10:40)))
# (ii) run loop using vector of selected column numbers
for(i in c(10:11,15:40)){
  tt0 <- with(sv18, t.test(log10(sv18[,i]) ~ Ca_split))
  wx0 <- with(sv18, wilcox.test(sv18[,i] ~ Ca_split))
  meanz <- tapply(sv18[,i], sv18$Ca_split, mean, na.rm=T)
  table[i-9,2:5] <- c(round(tt0$p.value,5), 
                      round(wx0$p.value,5), 
                      round(meanz[1],1), 
                      round(meanz[2],1))
}
print(table)
rm(list = c("varz","table","tt0","wx0","meanz"))

# 5.2 New Factors based on spatial information ####

## dividing by sampling zone ####
# see annotations to this code in Part 5
afs20$Loc0 <- cut(afs20$Easting, breaks = c(0,400170,999999),
                  labels = c("W","N"))
afs20$Loc1 <- cut(afs20$Northing, 
                  breaks = c(0,6468125,9999999),
                  labels = c("S","N_NW"))
afs20$Loc <- paste0(afs20$Loc0,afs20$Loc1)
afs20$Loc <- gsub("NN_NW","N",afs20$Loc)
afs20$Loc <- gsub("WN_NW","NW",afs20$Loc)
afs20$Loc <- gsub("WS","S",afs20$Loc)
afs20$Loc[42] <- NA
afs20$Loc[57] <- NA
afs20$Loc[58] <- NA
afs20$Loc <- as.factor(afs20$Loc)
afs20$Loc0 <- NULL
afs20$Loc1 <- NULL

# plot, distinguishing points by the factor we just created
with(afs20, plot(Northing ~ Easting, asp = 1,
                 pch = c(15,16,17)[Loc],
                 col = c(2,4,6)[Loc]))

# use the new factor in a series of statistical tests
# (this is not in Part 5, it's new for Part 7!)
library(PMCMRplus)
with(afs20, boxplot(Zn ~ Loc, las = 0))
with(afs20, kruskal.test(Zn ~ Loc))
with(afs20, kwAllPairsConoverTest(Zn ~ Loc))

varz <- names(afs20)
table <- data.frame(Variable = varz[9:38],
                    Kruskal_Chisq = rep(NA,length(9:38)), 
                    Kruskal_p = rep(NA,length(9:38)),
                    Mean_1 = rep(NA,length(9:38)), 
                    Mean_2 = rep(NA,length(9:38)), 
                    Mean_3 = rep(NA,length(9:38)),
                    pwise12 = rep(NA,length(9:38)),
                    pwise13 = rep(NA,length(9:38)),
                    pwise23 = rep(NA,length(9:38)))
# run loop using vector of selected column numbers except Cd
for(i in c(9:14,16:38)){
  KWt <- with(afs20, kruskal.test(afs20[,i] ~ Loc))
  KWpw <- with(afs20, kwAllPairsConoverTest(afs20[,i] ~ Loc))
  meanz <- tapply(afs20[,i], afs20$Loc, mean, na.rm=T)
  table[i-8,2:9] <- c(round(KWt$statistic,1), 
                      round(KWt$p.value,5), 
                      round(meanz[1],1), 
                      round(meanz[2],1), 
                      round(meanz[3],1),
                      round(KWpw$p.value[1],5),
                      round(KWpw$p.value[2],5),
                      round(KWpw$p.value[4],5))
}
{print(table)
cat("Key: level 1 = ", levels(afs20$Loc)[1], 
    "; level 2 = ", levels(afs20$Loc)[2], 
    "; level 3 = ", levels(afs20$Loc)[3],"\n")}

# draw some plots as well
par(mfrow = c(4,4), las = 0)
# select some columns . . .
for(i in c(9:14,17:20,22,25:26,31,32,38)){
  with(afs20, boxplot(afs20[,i] ~ Loc, ylab="", cex = 1.2,
       col = c("rosybrown1", "moccasin", "lemonchiffon2"),
       cex.lab = 1.4)
       )
  mtext(varz[i], side = 2, line = 1.5, font = 2)
}
par(mfrow = c(1,1))

rm(list = c("varz","table","KWt","KWpw","meanz"))


# 5.3 New factor from a new variable ####

# use the code from the EDA WORKFLOW 4 session
# to calculate distance from the Chapman Drain for 
# all 2020 sample locations
# (assume linear drain ax + by + c = 0)
a <- 0.9862637; b <- -1; c <- 6073413
afs20$dist.Chapman  <-  with(afs20,
                             (abs(a*Easting + b*Northing + c))/(sqrt(a^2 + b^2)))

# then cut this new variable into a 2-level factor
afs20$prox.Chapman <- cut(afs20$dist.Chapman, 
                          breaks = c(0,50,999), 
                          labels = c("lt.50m","gt.50m"))
# NW wetland is not near the drain, so:
#   - subset the data using subset()
#   - remove unused factor levels using droplevels()
#     (!= means 'not equal to')
afs20_Chapman <- droplevels(subset(afs20, 
                                   subset = afs20$Loc!="NW"))

# test all this using a loop running mean comparison tests
# make names vector and data frame before running loop
varz <- names(afs20_Chapman)
table <- data.frame(Variable = varz[9:38],
                    t_test_p = rep(NA,30), 
                    Wilcoxon_p = rep(NA,30),
                    Mean_near = rep(NA,30), 
                    Mean_far = rep(NA,30))
# set up the loop with desired columns
for(i in 9:38){
  tt0 <- with(afs20_Chapman, 
              t.test(afs20_Chapman[,i] ~ prox.Chapman))
  wx0 <- with(afs20_Chapman, 
              wilcox.test(afs20_Chapman[,i] ~ prox.Chapman))
  meanz <- tapply(afs20_Chapman[,i], 
                  afs20_Chapman$prox.Chapman, mean, na.rm=T)
  table[i-8,2:5] <- c(round(tt0$p.value,4), 
                      round(wx0$p.value,4), 
                      round(meanz[1],1), 
                      round(meanz[2],1))
}
print(table)
rm(list = c("varz","table","tt0","wx0","meanz"))

# alternatively analyze data graphically with new factor
# make names vector and set multi-frame plot before loop
varz <- names(afs20_Chapman)
par(mfrow = c(3,3), mar = c(3,3,1,1), mgp = c(1.5,0.3,0),
    tcl = 0.25, font.lab = 2)
# set up the loop with your desired columns
for(i in c(9,11,12,14,16,19,20,31,32)){
  boxplot(afs20[,i] ~ afs20$prox.Chapman,
          xlab = "Dist. range from drain (m)",
          ylab="", names = c("< 50","> 50"))
  mtext(paste(varz[i],"(mg/kg)"), 
        side = 2, line = 1.5, font = 2)
}
print(table)
rm("varz")
