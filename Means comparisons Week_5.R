#### Data Practical 5 ENVT4461: R code to work through ####

## Intro: Comparisons of means between groups #### --------------------

# Comparison of means tests help you determine whether 
# or not your groups have similar means.
#   There are many cases in statistics where you'll 
# want to compare means for two or more 
# populations, samples, or sample types. The parametric 
# tests like t-tests or ANOVA compare the variance 
# between groups with the variance within groups,and 
# use the relative sizes of these 2 variances to 
# estimate the probability that means are different.
# The parametric mean comparison tests require the 
# variables to have normal distributions.
# 
# Means comparisons based on 
# Null Hypothesis Statistical Testing (NHST)
# compares the variance between groups with the
# variance within groups,and generate a statistic 
# which, if large/unlikely enough (i.e. p <= 0.05), 
# allows rejection of the null hypothesis 
# (H0 = no difference between means in each/all groups).
#
# [Further down in this R Code Examples session, 
# we'll look at 'non-parametric' ways of comparing means, 
# to be used when our variable(s) don't meet the requirements 
# of conventional NHST.]

# In this session we're going to use the 2017 Smith's Lake 
# -- Charles Veryard Reserves dataset to 
# compare means between groups for factors having:
# 1. only two groups (using only the soil data);
# 2. more than two groups (using the whole dataset).

# First create a factor separating the two Reserves into 
# groups AND limit the data to only soil
# We split the dataset at Northing = 6466535 m, which 
# represents Bourke Street.

# read the data
sv2017 <- read.csv(file = "sv2017_original.csv", 
                   stringsAsFactors = TRUE)

sv2017$Reserve <- cut(sv2017$Northing,
                      breaks = c(0,6466535,9999999),
                      labels = c("Smiths","Veryard"))
sv17_soil <- subset(sv2017, subset=sv2017$Type=="Soil")
{cat("Number of soil samples in each reserve\n")
summary(sv17_soil$Reserve)}

# check factor with a plot
par(mfrow=c(1,1), mar=c(3.5,3.5,1.5,1.5), mgp=c(1.6,0.5,0),
    font.lab=2, font.main=3, cex.main=0.8, tcl=0.3)
# use the asp = 1 option for a map-like plot to 
# avoid distortion
plot(sv17_soil$Northing~sv17_soil$Easting,
     pch = c(1,2)[sv17_soil$Reserve],
     col = c(2,4)[sv17_soil$Reserve],
     lwd = 2, asp=1, xlab="Easting (UTM Zone 50, m)",
     ylab="Northing (UTM Zone 50, m)",
     main = "Samples by reserve")
legend("bottomleft", legend = levels(sv17_soil$Reserve),
       pch = c(1,2), col = c(2,4),
       pt.lwd = 2, title = "Reserve",
       bty = "n", inset = 0.02)

# Hopefully this looks OK!

# Means comparisons for exactly two groups -------------------- ####

# For variables which are normally distributed, we can use 
# conventional, parametric statistics. The following example 
# applies a t-test to compare mean values between Reserve. 
# By default R uses the Welch t-test, which doesn't 
# require the variance in each group to be equal.
#   Of course, we still need to use appropriately transformed 
# variables!

require(car)
powerTransform(sv17_soil$Na)
sv17_soil$Na.pow <- sv17_soil$Na^0.127

t.test(sv17_soil$Na.pow ~ sv17_soil$Reserve)

# We can visualize means comparisons in a few different ways. 
# My favourite is the boxplot with means included as extra  
# information - with a bit of additional coding we can include 
# the 95% confidence intervals as well! (but this is not shown 
# in this document).

# =+=+=+=+=+=+=+= start code block for 3 plots =+=+=+=+=+=+=+=
# =+=+=+=+=+=+=+=+=+=+= (scroll down) =+=+=+=+=+=+=+=+=+=+=+=+
# visualising differences in means - 2 groups
par(mfrow=c(1,3), mar=c(3.5,3.5,1.5,1.5), mgp=c(1.6,0.5,0),
    font.lab=2, font.main=3, cex.main=0.8, tcl=-0.2,
    ljoin = "mitre", lend = "square")
boxplot(sv17_soil$Na.pow ~ sv17_soil$Reserve, 
        notch=T, col="grey92",
        xlab="Reserve", ylab="Na (power-transformed)")
require(RcmdrMisc)
plotMeans(sv17_soil$Na.pow, sv17_soil$Reserve, 
          error.bars="conf.int",
          xlab="Reserve", ylab="Na (power-transformed)",
          main = "Don't include plot titles for reports!")
#
# the third plot is a box plot with the means overplotted
boxplot(sv17_soil$Na.pow ~ sv17_soil$Reserve, 
        notch=F, col="thistle",
        xlab="Reserve", ylab="Na (power-transformed)")
# make a temporary object 'meanz' containing the means
meanz <- tapply(sv17_soil$Na.pow, sv17_soil$Reserve, mean, 
                na.rm=T)
# plot means as points (boxplot boxes are centered on 
# whole numbers)
points(seq(1, nlevels(sv17_soil$Reserve)), meanz, 
       col = 6, pch = 3, lwd = 2)
legend("bottomright", "Mean values", 
       pch = 3, pt.lwd = 2, col = 6,
       bty = "n", inset = 0.03)
rm(meanz) # tidy up
# =+=+=+=+=+=+=+= end code block for 3 plots =+=+=+=+=+=+=+=

# You could tidy these 3 plots up a bit by using the same 
# y-axis scale for all three!

# Homogeneity of variance using Bartlett's Test -------------------- ####

# We can actually check if the variances are equal in 
# each group using Bartlett's Test:
bartlett.test(sv17_soil$Na.pow ~ sv17_soil$Reserve)

# The Bartlett test shows that H0 (that variances are equal) can not be
# rejected, so we can assum equal variance. We can visualise this with
# (for instance) a boxplot or density plot:

require(car)
par(mfrow=c(1,2), mar=c(3.5,3.5,1.5,1.5), mgp=c(1.6,0.5,0),
    font.lab=2, font.main=3, cex.main=0.8, tcl=-0.2)
boxplot(sv17_soil$Na.pow ~ sv17_soil$Reserve, 
        notch=FALSE, col="grey92",
        xlab="Reserve", ylab="Na (power-transformed)")
densityPlot(sv17_soil$Na.pow ~ sv17_soil$Reserve, 
            xlab="Na (power transformed)", adjust=1.5, 
            ylim=c(0,19))
par(mfrow=c(1,1)) # reset multiple graphics panes

# In each case it's apparent that the variance in Na in 
# the Veryard soil is similar to that at Smith's Lake.

# One-sided comparisons -------------------- ####

# A t-test can also compare the mean value with a fixed value:
# (e.g. let's pretend the local background Pb concentration
# in soil is 10 mg/kg - is our mean greater?)
t.test(sv17_soil$Pb, mu = 10, alternative = "greater")


# Effect size for means comparisons: Cohens d ####
# Statistical tests which compare means only estimate if 
# there is a difference or not. We would also usually like 
# to know how big the difference (or 'effect') is! 
# The Cohen's d statistic is a standardised measure 
# of effect size available in the 'effsize' R package.

# Cohen (1992) originally categorised d as follows: |d|<0.2 "negligible",
# |d|<0.5 "small", |d|<0.8 "medium", |d|>=0.8 "large"

require(effsize)
cohen.d(sv17_soil$Na.pow ~ sv17_soil$Reserve)

# The calculated value of Cohen's d is 
# 0.5 =< d < 0.8, which is "medium". The 95% CI 
# for the estimate of Cohen's d (i.e. between 
# 'inf' and 'sup') does not include zero, so we can 
# probably rely on it.

# More recently than Cohen, Sawilowsky (2009) proposed 
# that for Cohen's d:
# d (.01) = very small,
# d  (.2) = small, 
# d  (.5) = medium, 
# d  (.8) = large, 
# d (1.2) = very large, and 
# d (2.0) = huge

# Means comparisons for 3 or more groups -------------------- ####

# If we have a factor with 3 or more levels (a.k.a.
# groups, or categories), we can use analysis of variance 
# (ANOVA) to compare means of a normally distributed 
# variable. In this example we'll use the factor 'Type' 
# (= sample type) from the Smith's--Veryard data 
# (not just soil!).
# We still need to use appropriately transformed 
# variables!
  
## one-way analysis of variance -------------------- ####

powerTransform(sv2017$Al)
sv2017$Al.pow <- sv2017$Al^0.455

anova_Al <- aov(sv2017$Al.pow ~ sv2017$Type)
print(anova_Al$call)
summary(anova_Al)
{cat("\nMeans for transformed variable\n")
meansAl <- tapply(sv2017$Al.pow, sv2017$Type, mean, 
                  na.rm=TRUE)
print(signif(meansAl,3))} # output means with 3 signif. digits
{cat("\nMeans for original (untransformed) variable\n")
meansAl <- tapply(sv2017$Al, sv2017$Type, mean, na.rm=TRUE)
print(signif(meansAl,4))} # output means with 4 signif. digits
rm(list=c("anova_Al","meansAl")) # tidy up

# In the output above, the p-value in the ANOVA table "Pr(>F)" 
# is less than 0.05 allowing us to reject the null hypothesis. 

## Visualising differences in means - 3 or more groups -------------------- ####

# =+=+=+=+=+=+=+= start code block for 3 plots =+=+=+=+=+=+=+=
# =+=+=+=+=+=+=+=+=+=+= (scroll down) =+=+=+=+=+=+=+=+=+=+=+=+
par(mfrow=c(1,3), mar=c(3.5,3.5,1.5,1.5), mgp=c(1.6,0.5,0),
    font.lab=2, font.main=3, cex.main=0.8, tcl=-0.2)
boxplot(sv2017$Al.pow ~ sv2017$Type, 
        notch=T, col="grey92", cex = 1.4,
        xlab="Sample type", ylab="Al (power-transformed)",
        ylim = c(13,64))
require(RcmdrMisc)
plotMeans(sv2017$Al.pow, sv2017$Type, error.bars="conf.int",
          xlab="Sample type", ylab="Al (power-transformed)",
          ylim = c(13,64))
#
boxplot(sv2017$Al.pow ~ sv2017$Type, 
        notch=F, col="thistle", cex = 1.4,
        xlab="Reserve", ylab="Al (power-transformed)",
        ylim = c(13,64))
meanz <- tapply(sv2017$Al.pow, sv2017$Type, mean, na.rm=T)
points(seq(1, nlevels(sv2017$Type)), meanz, 
       col = 4, pch = 3, lwd = 2, cex = 1.2)
legend("bottomright", "Mean values", 
       pch = 3, pt.lwd = 2, col = 4, pt.cex = 1.2,
       bty = "n", inset = 0.03)
rm(meanz) # tidy up

# NOTE!
# You would never present 3 plots like this in a report,
# since they all show essentially the same thing. 
# In practice we would choose the option that best shows our data.

# =+=+=+=+=+=+=+= end code block for 3 plots =+=+=+=+=+=+=+=

# Check homogeneity of variances, 3 or more groups -------------------- ####

# ANOVA also requires variance for each group to be 
# (approximately) equal. 

bartlett.test(sv2017$Al.pow~sv2017$Type)

## Visualise variance for each group
require(car)
par(mfrow=c(2,1), mar=c(3.5,3.5,1,1), mgp=c(1.6,0.5,0),
    font.lab=2, font.main=3, cex.main=0.8, tcl=-0.2)
boxplot(sv2017$Al.pow ~ sv2017$Type, 
        notch=FALSE, col="grey92",
        xlab="Reserve", ylab="Al (power-transformed)")
densityPlot(sv2017$Al.pow ~ sv2017$Type, adjust=2, 
            xlab="Al (power transformed)",
            legend = list(title="Type"))
par(mfrow=c(1,1)) # reset multiple graphics panes

# In each case it's apparent that the variance in Al is 
# Sediment > Street dust > soil.

# Analysis of variance with unequal variances -------------------- ####

# We can use the Welch f-test (oneway.test(...)) if 
# our variable has different variance for different 
# factor levels.

oneway.test(sv2017$Al.pow ~ sv2017$Type)

# The Welch correction for unequal variances means the 
# p-value is now too high to reject the null hypothesis,
# so we find no difference between means.

# Effect sizes for 3 or more groups -------------------- ####

# It's not possible to 
# calculate Effect sizes for 3 or more groups directly. 
# We would need to create subsets of our dataset which 
# include only two groups (e.g., with only Soil and 
# Sediment), and then run cohen.d() from the 'effsize' R 
# package. Or we could do some custom coding...

#    ___      _               _
#   / _ \__ _(_)_ ____      _(_)___  ___
#  / /_)/ _` | | '__\ \ /\ / / / __|/ _ \
# / ___/ (_| | | |   \ V  V /| \__ \  __/
# \/    \__,_|_|_|    \_/\_/ |_|___/\___|
#    ___                                 _
#   / __\___  _ __ ___  _ __   __ _ _ __(_)___  ___  _ __  ___
#  / /  / _ \| '_ ` _ \| '_ \ / _` | '__| / __|/ _ \| '_ \/ __|
# / /__| (_) | | | | | | |_) | (_| | |  | \__ \ (_) | | | \__ \
# \____/\___/|_| |_| |_| .__/ \__,_|_|  |_|___/\___/|_| |_|___/
#                      |_|


# Pairwise comparisons -------------------- ####

#   If our analysis of variance allows rejection of 
# H0, we still don't necessarily know 
# which means are different. The test may return 
# a p-value <= 0.05 even if only one mean is different 
# from all the others. If the p-value <= 0.05, we can 
# compute Pairwise Comparisons. The examples below 
# show pairwise comparisons in an analysis of variance for 
# Ba, in groups defined by the factor 'Type'.

sv2017$Ba.log <- log10(sv2017$Ba) # make transformed variable
anova0 <- with(sv2017, aov(Ba.log ~ Type))
summary(anova0)
# The simplest way to conduct pairwise comparisons is using a
# pairwise t-test.
# first do a Bartlett test...
with(sv2017, bartlett.test(Ba ~ Type))
# showing that we can use the default option in the 
# pairwise t-test pool.sd = TRUE
# if the Bartlett test showed unequal variances, we would use
# with(sv2017, pairwise.t.test(Ba.log, Type, pool.sd = FALSE))
with(sv2017, pairwise.t.test(Ba.log, Type))

# Pairwise compact letter display
# Using the multcompView package we can view this another way:
library(rcompanion)
library(multcompView)
pw_Ba <- with(sv2017, pairwise.t.test(Ba.log, Type))
pw_Ba_pv <- fullPTable(pw_Ba$p.value)
# multcompLetters() gives a somewhat familiar "compact letter display":
multcompLetters(pw_Ba_pv)
rm(list = c("pw_Ba","pw_Ba_pv")) # tidy up

# Using either the pairwise output or 
# we see that we can reject H0 for Sediment-Soil and
# Sediment-Street dust, but not for Soil-Street dust.

# The pairwise t-test includes an adjustment (increase) of
# p-values to account for the increased likelihood of
# Type-1 Errors (false positives) when making simultaneous 
# multiple comparisons.
# We can conduct a more rigorous pairwise comparison 
# using the glht() and cld() functions in the 
# multcomp R package.
# N.B. we have to use the anova function inside 
# with() so that the variable names are consistent 
# in the next functions [ glht() and cld() ]
anova0 <- with(sv2017, aov(Ba.log ~ Type))
summary(anova0)
require(multcomp)
# note that 'Type' in the next line is the name of the
# factor we are comparing means against!
pwise0 <- glht(anova0, linfct = mcp(Type="Tukey"))
summary(pwise0)
# cld() gives the by now familiar compact letter display:
cld(pwise0)

# Groups assigned a different letter by cld are significantly 
# different at the specified probability level (p<=0.05 
# by default). In this example, Ba concentration in 
# sediment ('a') is significantly different from both Soil 
# and Street dust (both 'b', so not different from each 
# other). 

# We can get the confidence intervals and p-values 
# for each pairwise comparison using the TukeyHSD() 
# function (HSD='Honestly Significant Difference'):

# Pairwise Tukey multiple comparisons of means

TukeyHSD(anova0)
rm(list = c("anova0", "pwise0")) # tidy up

# The table of output (after '$Type') shows the 
# differences between mean values for each pairwise 
# comparison (diff), and the lower (lwr) and upper (upr) 
# limits of the 95% confidence interval for the difference 
# in means. If the 95% CI includes zero (e.g. for the 
# Street dust-Soil comparison above), there is no 
# significant difference.

# This is supported by the last column in the table of
# output, showing an adjusted p-value of 0.633 (i.e. > 0.05) 
# for the Street dust-Soil comparison. Also, as indicated 
# by the 'compact letter display' from cld() above, any 
# comparison including Sediment has p<=0.05. (This also 
# matches the results of the pairwise t-test)

# If we needed to use the Welch's F-test instead of ANOVA,
# we just use the pairwise.t.test() function with Holm's 
# correction for multiple comparisons:

with(sv2017, oneway.test(Ba.log ~ Type))
with(sv2017, pairwise.t.test(Ba.log, Type))
#   _   _                                                
#  | \ | |                                               
#  |  \| | ___  _ __                                     
#  | . ` |/ _ \| '_ \ 
#  | |\  | (_) | | | |
#  |_|_\_|\___/|_|_|_|
#                                        _        _      
#                                       | |      (_)     
#   _ __   __ _ _ __ __ _ _ __ ___   ___| |_ _ __ _  ___ 
#  | '_ \ / _` | '__/ _` | '_ ` _ \ / _ \ __| '__| |/ __|
#  | |_) | (_| | | | (_| | | | | | |  __/ |_| |  | | (__ 
#  | .__/ \__,_|_|  \__,_|_| |_| |_|\___|\__|_|  |_|\___|
#  | |                                                   
#  |_|  
#   _            _       
#  | |          | |      
#  | |_ ___  ___| |_ ___ 
#  | __/ _ \/ __| __/ __|
#  | ||  __/\__ \ |_\__ \
#   \__\___||___/\__|___/

# Non-parametric comparisons. 1. Wilcoxon test -------------------- ####

# From previous sessions we know that most
# untransformed variables are not normally distributed. 
# For comparison between exactly 2 groups we use the 
# Wilcoxon test.The Wilcoxon test is based on 
# ranking of observations, so should be independent of
# transformation as in the example below:

# wilcoxon rank sum test
wilcox.test(sv17_soil$Na ~ sv17_soil$Reserve)
{cat("Means for original (untransformed) variable\n")
meansNa <- tapply(sv17_soil$Na, sv17_soil$Reserve, mean,
                  na.rm=TRUE)
print(signif(meansNa, 3))
cat("\n--------------------------------------------\n")}
wilcox.test(sv17_soil$Na.pow ~ sv17_soil$Reserve)
{cat("Means for transformed variable\n")
meansNa <- tapply(sv17_soil$Na.pow, sv17_soil$Reserve, mean, 
                  na.rm=TRUE)
print(signif(meansNa, 3))}
rm(meansNa) # remove temporary object(s)

# effect size
require(effsize)
cohen.d(sv17_soil$Na.pow ~ sv17_soil$Reserve)

# we can also use the Wilcoxon test like a one-sided t-test:
# (e.g. let's pretend the local background Pb concentration
# in soil is 10 mg/kg)
wilcox.test(sv17_soil$Pb, mu = 10, alternative = "greater")

# non-parametric comparisons. 2. Kruskal-Wallis test -------------------- ####

# This example is testing differences in Fe between sample 
# Types in the complete Smith's--Veryard 2017 dataset.

# Kruskal-Wallis test
kruskal.test(sv2017$Fe ~ sv2017$Type)
{cat("Means for original (untransformed) variable\n")
  meansFe <- tapply(sv2017$Fe, sv2017$Type, mean,
                    na.rm=TRUE)
  print(signif(meansFe),4)}
rm(meansFe) # tidy up
# 
# With a p-value of ~0.016, H0 can be rejected.
# We still have the problem of not knowing which means 
# are significantly different from each other. The 
# PMCMRplus package allows multiple comparisons of means 
# following statistically significant Kruskal-Wallis 
# comparisons (there are several options; we will use the 
# Conover's non-parametric all-pairs comparison test for 
# Kruskal-type ranked data).

# Pairwise comparisons following a Kruskal-Wallis test -------------------- ####

# Conover pairwise comps
require(PMCMRplus)
pwFe <- kwAllPairsConoverTest(Fe~Type, data=sv2017)
print(pwFe)

# compact letter display again
require(rcompanion)
require(multcompView)
pwFe_pv <- fullPTable(pwFe$p.value)
multcompLetters(pwFe_pv)

# another visualisation
mcl <- multcompLetters(pwFe_pv)
par(xpd = TRUE)
boxplot(Fe.log ~ Type, data = sv2017)
text(seq(1:nlevels(sv2017$Type)), 
     rep(par('usr')[4]+0.03*(par('usr')[4]-par('usr')[3]),nlevels(sv2017$Type)),
     labels = as.character(mcl$Letters), col = 4, font = 2)
rm(list = c("pwFe","pwFe_pv")) # tidy up

# The output above shows that p<=0.05 only for the 
# Soil-Street dust comparison. We can't reject 
# H0 for any other pairwise comparisons. [We 
# would get slightly different results using the functions 
# kwAllPairsDunnTest() or kwAllPairsNemenyiTest() 
#  - try it if you want.

#       _______ _            ______           _ 
#     |__   __| |          |  ____|         | |
#       | |  | |__   ___  | |__   _ __   __| |
#      | |  | '_ \ / _ \ |  __| | '_ \ / _` |
#     | |  | | | |  __/ | |____| | | | (_| |
#    |_|  |_| |_|\___| |______|_| |_|\__,_|
             
#             [end code]

# except for References 

# Cohen, J. (1988). Statistical power analysis for the behavioral sciences
# (2nd ed.). New York:Academic Press.

# Sawilowsky, S. (2009). "New effect size rules of thumb". Journal of Modern
# Applied Statistical Methods. 8 (2): 467-474.

citation("car", auto = TRUE)
citation("RcmdrMisc", auto = TRUE)
citation("effsize", auto = TRUE)
citation("multcomp", auto = TRUE)
citation("PMCMRplus", auto = TRUE)
citation("rcompanion", auto = TRUE)
citation("multcompView", auto = TRUE)
