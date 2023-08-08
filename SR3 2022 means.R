afs2020 <- read.table("clipboard", sep="\t", header = T, stringsAsFactors = T)
str(afs2020)

shapiro.test(afs2020$pH)
shapiro.test(log10(afs2020$pH))
shapiro.test(afs2020$pH^powerTransform(afs2020$pH)$lambda)

shapiro.test(afs2020$EC)
shapiro.test(log10(afs2020$EC))
shapiro.test(afs2020$EC^powerTransform(afs2020$EC)$lambda)

shapiro.test(afs2020$As)
shapiro.test(log10(afs2020$As))
shapiro.test(afs2020$As^powerTransform(afs2020$As)$lambda)

shapiro.test(afs2020$Gd)
shapiro.test(log10(afs2020$Gd))
shapiro.test(afs2020$Gd^powerTransform(afs2020$Gd)$lambda)

shapiro.test(afs2020$Pb)
shapiro.test(log10(afs2020$Pb))
shapiro.test(afs2020$Pb^powerTransform(afs2020$Pb)$lambda)

shapiro.test(afs2020$Zn)
shapiro.test(log10(afs2020$Zn))
shapiro.test(afs2020$Zn^powerTransform(afs2020$Zn)$lambda)

names(afs2020)
afs2020$Group

afs2020$Zone <- as.character(afs2020$Group)
afs2020$Zone <- gsub("10","S",afs2020$Zone)
afs2020$Zone <- gsub("9","S",afs2020$Zone)
afs2020$Zone <- gsub("8","S",afs2020$Zone)
afs2020$Zone <- gsub("7","N",afs2020$Zone)
afs2020$Zone <- gsub("6","N",afs2020$Zone)
afs2020$Zone <- gsub("5","N",afs2020$Zone)
afs2020$Zone <- gsub("4","N",afs2020$Zone)
afs2020$Zone <- gsub("3","NW",afs2020$Zone)
afs2020$Zone <- gsub("2","NW",afs2020$Zone)
afs2020$Zone <- gsub("1","NW",afs2020$Zone)

afs2020$Zone <- as.factor(afs2020$Zone)

with(afs2020, plot(Easting, Northing, asp = 1, col=8))
with(afs2020, text(Easting, Northing, labels = Zone, cex = 0.75))

# Based on Shapiro-Wilk
# normal = pH, Gd
# power = Pb
# nonparam = EC, As, Zn

afs2020$Pb.pow <- afs2020$Pb^powerTransform(afs2020$Pb)$lambda
with(afs2020, bartlett.test(pH, Zone)) # heteroscedastic
with(afs2020, bartlett.test(Gd, Zone)) # heteroscedastic
with(afs2020, bartlett.test(Pb.pow, Zone)) # homoscedastic

with(afs2020, oneway.test(pH ~ Zone)) # P < 0.001
with(afs2020, oneway.test(Gd ~ Zone)) # P < 0.001
anova_Pb <- with(afs2020, aov(Pb.pow ~ Zone)) # P < 0.001
summary(anova_Pb) # P < 0.001
with(afs2020, kruskal.test(EC ~ Zone)) # P < 0.001
with(afs2020, kruskal.test(As ~ Zone)) # P < 0.001
with(afs2020, kruskal.test(Zn ~ Zone)) # P < 0.001

library(PMCMRplus)

# pairwise tests
pw_pH <- with(afs2020, pairwise.t.test(pH, Zone, pool.sd = F))
pw_Gd <- with(afs2020, pairwise.t.test(Gd, Zone, pool.sd = F))
pw_Pb <- with(afs2020, pairwise.t.test(Pb.pow, Zone, pool.sd = T))
pw_EC <- kwAllPairsConoverTest(afs2020$EC ~ afs2020$Zone)
