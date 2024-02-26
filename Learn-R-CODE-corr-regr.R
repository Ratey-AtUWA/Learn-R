# Week 5 correlation & regression 1
# basic correlation analyses ####
# 
# relationship between Cd and Zn
sv2017$Cd.log <- log10(sv2017$Cd)
sv2017$Zn.log <- log10(sv2017$Zn)
shapiro.test(sv2017$Cd)
shapiro.test(sv2017$Cd.log)
shapiro.test(sv2017$Zn)
shapiro.test(sv2017$Zn.log)
# Pearson's r changes with transformation
cor.test(sv2017$Cd, sv2017$Zn, alternative="two.sided", 
         method="pearson") # is Pearson valid?
cor.test(sv2017$Cd.log, sv2017$Zn.log, alternative="two.sided", 
         method="pearson") # is Pearson valid?
# spearman coefficient doesn't change with transformation
cor.test(sv2017$Cd, sv2017$Zn, alternative="two.sided", 
         method="spearman") # can we use method="pearson"?
cor.test(sv2017$Cd.log, sv2017$Zn.log, alternative="two.sided", 
         method="spearman") # can we use method="pearson"?
#
# simple scatterplot by groups ####
palette(c("black","blue","red3","darkgreen","purple",
          "darkcyan","sienna3","grey50","white"))
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.5,0.5,0), oma=c(0,0,0,0), tcl=0.2,
    cex=1.2, cex.lab=1.2, cex.axis=1., lend="square", ljoin="mitre", font.lab=2)
# We're using base R to plot - you might like to try using scatterplot()
plot(sv2017$Cd~sv2017$Zn, col=c(6,7,1)[sv2017$Type], log="xy", 
     pch=c(16,1,15)[sv2017$Type], lwd=c(1,2,1)[sv2017$Type], cex=1.5,
     xlab="Zn (mg/kg)", ylab="Cd (mg/kg)")
abline(lm(sv2017$Cd.log~sv2017$Zn.log))
legend("topleft", legend=levels(sv2017$Type), cex=1.2, col=c(6,7,1), 
       pch=c(16,1,15), pt.lwd=c(1,2,1), pt.cex=1.5, bty="n", inset=0.02,
       title=expression(italic("Sample type")))
#
# Spearman correlation matrix - two ways of doing it ####
# the p-values give the probablility of the observed relationship if 
# the null hypothesis (i.e. no relationship) is true
require(Hmisc) # needed for rcorr() function
corr_table <- rcorr(as.matrix(sv2017[c("pH","Al","Ca","Fe","Cd","Pb","Zn")]), 
                    type="spearman")
print(corr_table)
# version from RcmdrMisc which calculates _corrected_ P-values
# the p-values give the probablility of the observed relationship if 
# the null hypothesis (i.e. no relationship) is true.
# Corrections are to reduce risk of Type 1 Errors (false positives)
require(RcmdrMisc) # needed for rcorr.adjust() function
rcorr.adjust(sv2017[c("pH","Al","Ca","Fe","Cd","Pb","Zn")], type="spearman", 
             use="pairwise.complete")
#
# scatter plot matrix to check correlation matrix
require(car) # needed for scatterplotMatrix() function
carPalette(palette())
scatterplotMatrix(~pH+ log10(Al)+ log10(Ca)+ log10(Cd)+ 
                    log10(Fe)+ log10(Pb)+ log10(Zn) | Type, 
                  smooth=FALSE, ellipse=FALSE, by.groups=TRUE, 
                  col=c(6,7,1), pch=c(16,1,15), cex.lab=1.5, data=sv2017,
                  legend=list(coords="bottomleft"))
#
# linear regression model predicting chromium from iron: 1. simple ####
# first create log-transformed variable columns
sv2017$Fe.log <- log10(sv2017$Fe)
sv2017$Cr.log <- log10(sv2017$Cr)
# next use lm() function to create a linear model object, then summarise it
lmCrFesimple <- lm(sv2017$Cr.log~sv2017$Fe.log)
summary(lmCrFesimple)
#
# regression model predicting chromium from iron: 2. by groups ####
# note the syntax used to separate by factor categories
lmCrFe_byType <- lm(sv2017$Cr.log~sv2017$Fe.log * sv2017$Type)
summary(lmCrFe_byType)
#
# compare the two models (null hypothesis is equal predictive ability) ####
anova(lmCrFesimple,lmCrFe_byType)
#
# 'base R' scatter plots representing regression models
par(mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(1.5,0.5,0), oma=c(0,0,0,0), tcl=0.2,
    cex=1.2, cex.lab=1.2, cex.axis=1., lend="square", ljoin="mitre", font.lab=2)
# simple scatterplot
plot(sv2017$Cr~sv2017$Fe, log="xy")
mtext(side=3, line=-1.2, text="(a)", adj=0.05, cex=1.4)
abline(lmCrFesimple, col=8, lty=2)
# grouped scatterplot
plot(sv2017$Cr~sv2017$Fe, log="xy", col=c(1,7,2)[sv2017$Type], pch=c(0,2,16)[sv2017$Type])
mtext(side=3, line=-1.2, text="(b)", adj=0.05, cex=1.4)
# use for() {...} loop to add individual regression lines
for (i in 1:NROW(levels(sv2017$Type))) {
  abline(lm(log10(sv2017$Cr)~log10(sv2017$Fe), 
            subset=sv2017$Type==levels(sv2017$Type)[i]), col=i, lty=2)
}
legend("bottomright", legend=levels(sv2017$Type), col=c(1,7,2), pch=c(0,2,16),
       bty="n", inset=0.02)
#
# diagnostic plots for regression ####
# simple linear regression, Cr ~ Fe
par(mfrow=c(2,2), mar=c(3,3,2,1))
plot(lmCrFesimple)
par(mfrow=c(1,1), mar=c(3,3,1,1))
# grouped linear regression, Cr ~ Fe
par(mfrow=c(2,2), mar=c(3,3,2,1))
plot(lmCrFe_byType)
par(mfrow=c(1,1), mar=c(3,3,1,1))
#
#### more things to try ####

# try ls(lm_object) to see what is stored in regression results
  # (or str(lm_object))
# you might be able to use the contents of a lm object to:
  #1 plot calculated values (from the regression model) vs. measured values 
  #2 add a 1:1 relationship to the plot in #1 above
  #3 find out if any regression residuals are unusual
  #4 ...and so on.
