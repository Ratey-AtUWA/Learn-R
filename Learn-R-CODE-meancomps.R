#     ____                                 _             
#    / ___|___  _ __ ___  _ __   __ _ _ __(_)_ __   __ _ 
#   | |   / _ \| '_ ` _ \| '_ \ / _` | '__| | '_ \ / _` |
#   | |__| (_) | | | | | | |_) | (_| | |  | | | | | (_| |
#    \____\___/|_| |_| |_| .__/ \__,_|_|  |_|_| |_|\__, |
#                        |_|                       |___/ 
#                __  __                                  
#               |  \/  | ___  __ _ _ __  ___             
#               | |\/| |/ _ \/ _` | '_ \/ __|            
#               | |  | |  __/ (_| | | | \__ \            
#               |_|  |_|\___|\__,_|_| |_|___/            
#                                                        
#### Data Practical ENVT3361: Mean comparisons 1 - R code examples ####

## Intro: Comparisons of means between groups ####
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
# compare the variance between groups with the
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

# Means comparisons for exactly two groups ####
#
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

# Homogeneity of variance using Bartlett's Test ####
# We can actually check if the variances are equal in 
# each group using Bartlett's Test:
with(sv17_soil, bartlett.test(Na.pow ~ Reserve))
with(sv17_soil, var.test(Na.pow ~ Reserve))

# The Bartlett test shows that H0 (that variances are equal) 
# can be rejected. We can visualise this with (for instance) 
# a boxplot or density plot:

require(car)
par(mfrow=c(1,2), mar=c(3.5,3.5,1.5,1.5), mgp=c(1.6,0.5,0),
    font.lab=2, font.main=3, cex.main=0.8, tcl=-0.2,
    cex.lab = 1, cex.axis = 1)
boxplot(sv17_soil$Na.pow ~ sv17_soil$Reserve, 
        notch=FALSE, col="grey92",
        xlab="Reserve", ylab="Na (power-transformed)")
densityPlot(sv17_soil$Na.pow ~ sv17_soil$Reserve, 
            xlab="Na (power transformed)", adjust=1.5, ylim=c(0,5))
par(mfrow=c(1,1)) # reset multiple graphics panes

# In each case it's apparent that the variance in Na in 
# the Veryard soil is less than at Smith's Lake.


# Effect size for means comparisons: Cohens d ####

# Statistical tests which compare means only estimate if 
# there is a difference or not. We would also usually like 
# to know how big the difference (or 'effect') is! 
# The Cohen's d statistic is a standardised measure 
# of effect size available in the 'effsize' R package.

require(effsize)
cohen.d(sv17_soil$Na.pow ~ sv17_soil$Reserve)

# The calculated value of Cohen's d is 
# 0.5 =< d < 0.8, which is medium. The 95% CI 
# for the estimate of Cohen's d (i.e. between 
# 'inf' and 'sup') does not include zero, so we can 
# probably rely on it.

# Means comparisons for 3 or more groups ####

# If we have a factor with 3 or more levels (a.k.a.
# groups, or categories), we can use analysis of variance 
# (ANOVA) to compare means of a normally distributed 
# variable. In this example we'll use the factor 'Type' 
# (= sample type) from the Smith's--Veryard data 
# (not just soil!).
# We still need to use appropriately transformed 
# variables!

## one-way analysis of variance ####

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

## Visualising differences in means - 3 or more groups ####

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
# =+=+=+=+=+=+=+= end code block for 3 plots =+=+=+=+=+=+=+=

# check homogeneity of variances, 3 or more groups ####
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

# Analysis of variance with unequal variances ####
# We can use the Welch f-test (oneway.test(...)) if 
# our variable has different variance for different 
# factor levels.

with(sv2017, oneway.test(Al.pow ~ Type))

# The Welch correction for unequal variances means the 
# p-value is now too high to reject the null hypothesis,
# so we find no difference between means.

# Effect sizes for 3 or more groups ####

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


# Pairwise comparisons ####
#   If our analysis of variance allows rejection of 
# H0, we still don't necessarily know 
# which means are different. The test may return 
# a p-value <= 0.05 even if only one mean is different 
# from all the others. If the p-value <= 0.05, we can 
# compute Pairwise Comparisons. The examples below 
# show pairwise comparisons in an analysis of variance for 
# Ba, in groups defined by the factor 'Type'.

# Pairwise compact letter display (cld)
sv2017$Ba.log <- log10(sv2017$Ba) # make transformed variable
require(multcomp)
# N.B. we have to use the function inside with() so 
# that the variable names are consistent in the next 
# functions [ glht() and cld() ]
anovaBa <- with(sv2017, aov(Ba.log ~ Type))
cat("==== Analysis of Variance ====\n");summary(anovaBa)

#Next we generate the compact letters using the fullPTable() 
# function from the rcompanion package and multcompLetters() 
# from the multcompView package:

library(rcompanion)
library(multcompView)
(pwBa <- with(sv2017, pairwise.t.test(Ba.log, Type)))
cat("\n==== Compact letters ====\n") 
pwBa_pv <- fullPTable(pwBa$p.value)                 # from rcompanion
multcompLetters(pwBa_pv)                            # from multcompView

# In the output above, the table of p-values show a significant 
# difference (p < 0.05) between Sediment and Soil, and Sediment 
# and Street dust (note the use of Holm’s p-value adjustment, 
# which is the default method). We get the same interpretation 
# from the compact letters; Sediment ("a") is different from 
# Soil and Street dust (both "b"). Since Soil and Street dust 
# have the same letter ("b"), they are not significantly 
# different, which matches the p-value (0.3634).

# OPTIONAL: Pairwise compact letter display ####

# This method using the cld() function is way harder to 
# make sense of… it’s probably more rigorous, but leads 
# to the same conclusion. We need to load the 'multcomp 
# package.

require(multcomp)
pwise0 <- glht(anova0, linfct = mcp(Type="Tukey"))
cld(pwise0)

# Groups assigned a different letter are significantly 
# different at the specified probability level (p<=0.05 
# by default). In this example, Ba concentration in 
# sediment ('a') is significantly different from both Soil 
# and Street dust (both 'b', so not different from each 
# other). 

# We can get the confidence intervals and p-values 
# for each pairwise comparison using the TukeyHSD() 
# function (HSD='Honestly Significant Difference'):

# OPTIONAL: Pairwise Tukey multiple comparisons of means ####

# Also more complicated to understand and, like the code 
# above, we need to have a normally-distributed, homoskedastic, 
# variable. Usually we don’t.

TukeyHSD(anova0)
rm(list=c("anova0","pwise0"))

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
# comparison including Sediment has p<=0.05.

# If we needed to use the Welch's F-test instead of ANOVA,
# we can use for pairwise.t.test() function. This includes 
# a correction (Holm's) for multiple comparisons:

with(sv2017, oneway.test(Ba.log ~ Type))
with(sv2017, pairwise.t.test(Ba.log, Type))

#  __  __  ___    _    _  _  ___       ___   _    ___  _____       ___   _
# |  \/  || __|  /_\  | \| |/ __|     | _ \ /_\  | _ \|_   _|     |_  ) (_)
# | |\/| || _|  / _ \ | .` |\__ \     |  _// _ \ |   /  | |        / /   _
# |_|  |_||___|/_/ \_\|_|\_||___/     |_| /_/ \_\|_|_\  |_|       /___| (_)
#                                                                        
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

# Non-parametric comparisons. 1. Wilcoxon test ####

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

#                 _                                             _     ___  
#                | |                                           | |   |__ \ 
#   _ __    ___  | |_     _ __    ___   _ __  _ __ ___    __ _ | |      ) |
#  | '_ \  / _ \ | __|   | '_ \  / _ \ | '__|| '_ ` _ \  / _` || |     / / 
#  | | | || (_) || |_    | | | || (_) || |   | | | | | || (_| || |    |_|  
#  |_| |_| \___/  \__|   |_| |_| \___/ |_|   |_| |_| |_| \__,_||_|    (_)  
#                                                                           
# Non-parametric comparisons. 2. Kruskal-Wallis test ####

# This example is testing differences in Fe between sample 
# Types in the complete Smith's--Veryard 2017 dataset.

# Kruskal-Wallis test
kruskal.test(sv2017$Fe ~ sv2017$Type)
{cat("Means for original (untransformed) variable\n")
  meansFe <- tapply(sv2017$Fe, sv2017$Type, mean,
                    na.rm=TRUE)
  print(signif(meansFe),4)}
rm(meansFe)
# 
# With a p-value of ~0.016, H0 can be rejected.
# We still have the problem of not knowing which means 
# are significantly different from each other. The 
# PMCMRplus package allows multiple comparisons of means 
# following statistically significant Kruskal-Wallis 
# comparisons (there are several options; we will use the 
# Conover's non-parametric all-pairs comparison test for 
# Kruskal-type ranked data).

# Pairwise comparisons following a Kruskal-Wallis test ####

# Conover pairwise comps
require(PMCMRplus)
kwAllPairsConoverTest(Fe~Type, data=sv2017)

# The output above shows that p<=0.05 only for the 
# Soil-Street dust comparison. We can't reject 
# H0 for any other pairwise comparisons. [We 
# would get slightly different results using the functions 
# kwAllPairsDunnTest() or kwAllPairsNemenyiTest() 
#  - see below. The conclusions are the same for each version 
# of the test, in this example.]

kwAllPairsDunnTest(Fe~Type, data=sv2017)
cat('\n-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-\n')
kwAllPairsNemenyiTest(Fe~Type, data=sv2017)

# _______ _            ______           _ 
# |__   __| |          |  ____|         | |
#    | |  | |__   ___  | |__   _ __   __| |
#    | |  | '_ \ / _ \ |  __| | '_ \ / _` |
#    | |  | | | |  __/ | |____| | | | (_| |
#    |_|  |_| |_|\___| |______|_| |_|\__,_|

#             [end code]

