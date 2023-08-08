# ENVT3361 Data Workshop Week 6 ####
#### non-parametric comparisons. 1. Wilcoxon test ####
# 
# From previous sessions we know that most untransformed 
# variables are not normally distributed. For comparison
# between _exactly 2 groups_, we use the Wilcoxon test. The 
# Wilcoxon test is based on ranking of observations, so
# should be independent of transformation as in the 
# example below:
# __________________________________________
# read input data
# ! change filename from "sv2017_original.csv" to match download !
sv2017 <- read.csv(file = "sv2017_original.csv")
#
# __________________________________________
#### use cut() to make new factor from the northing coords ####
# since northing = 6466535 separates the two parks
# Northings between < 6466535m are Smith's Lake Reserve
# Northings > 6466535m are Charles Veryard Reserve
sv2017$Reserve <- cut(sv2017$Northing,
                      breaks = c(0, 6466535, 10000000),
                      labels = c("Smiths", "Veryard"))
summary(sv2017$Reserve) # check it worked!
# another check with a scatter plot:
plot(sv2017$Easting, sv2017$Northing, 
     col=c(2,4)[sv2017$Reserve], 
     pch=c(15,1)[sv2017$Reserve], 
     asp = 1) # you just made a map :^)
legend("bottomleft", legend = levels(sv2017$Reserve),
       col = c(2,3), pch=c(15,1), bty = "n")
#
# __________________________________________
# compare means of Na with non-parametric Wilcoxon test:
wilcox.test(sv17_soil$Na ~ sv17_soil$Reserve)
#
# Output means for original (untransformed) variable
meansNa <- tapply(sv2017$Na, sv2017$Reserve, mean, na.rm=TRUE)
print(signif(meansNa, 3))
rm(meansNa) # remove temporary object in R
#
# __________________________________________
# try it again, this time using transformed Na:
# (the Wilcoxon test should give exacly the same result!)
require (car)
powerTransform(sv2017$Na) # use result in next line of code
sv2017$Na.pow <- -1 * (sv2017$Na ^ -0.3711)
wilcox.test(sv17_soil$Na.pow ~ sv17_soil$Reserve)
#
# Output means for transformed variable
meansNa <- tapply(sv2017$Na.pow, sv2017$Reserve, mean, 
                  na.rm=TRUE)
print(signif(meansNa, 3))
rm(meansNa) # remove temporary object(s)
#
# __________________________________________
# Don't forget to calculate Cohen's d effect size for 
# any 2-level comparison!
#
#### effect size (Cohen's d) ####
#
# Statistical tests which compare means only estimate if there 
# is a difference or not. We would also usually like to know 
# how big the difference (or 'effect') is! The Cohen's d 
# statistic is a standardised measure of effect size
# available in the 'effsize' R package.
# The numerical value of Cohen's d is given a category, i.e.
# |d|<0.2 "negligible", |d|<0.5 "small", 
# |d|<0.8 "medium", otherwise (>0.8) "large"
#
require(effsize) # package needed for cohen.d() function
cohen.d(sv17_soil$Na.pow ~ sv17_soil$Reserve)

#
# __________________________________________
#### Non-parametric comparisons. 2. Kruskal-Wallis test ####
#
# This is the non-parametric equivalent for 
# an f-test or analysis of variance (ANOVA),
# based (like Wilcoxon) on the ranks of a variable,
# not its values (so it's not affected by distributions)
#
# We use Kruskal-Wallis when there are 3 or more groups.
#
# This example is testing differences in Fe between 
# sample Types in the complete Smith's-Veryard 2017 dataset.
# 
kruskal.test(sv2017$Fe ~ sv2017$Type)
#
# Means for original (untransformed) variable
meansFe <- tapply(sv2017$Fe, sv2017$Type, mean,
                  na.rm=TRUE)
print(signif(meansFe),4)
rm(meansFe)

# We still have the problem of not knowing which means are
# significantly different from each other.
# __________________________________________
#### Pairwise comparisons following a Kruskal-Wallis test ####
#
# The PMCMRplus package allows multiple comparisons of 
# means following statistically significant Kruskal-Wallis
# comparisons (there are several options; we will use the 
# Conover's non-parametric all-pairs comparison test for
# Kruskal-type ranked data).
#
attach(sv2017, warn.conflicts=FALSE)
# PMCMRplus package needed for kwAllPairsConoverTest() function
require(PMCMRplus) 
kwAllPairsConoverTest(Fe~Type, data=sv2017)

# The output above shows that p<0.05 only for the 
# Soil-Street dust comparison. We can't reject H0 for 
# any other pairwise comparisons. 