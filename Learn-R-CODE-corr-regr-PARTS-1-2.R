#### ALL THE CORRELATION & REGRESSION CODE ####

# Basic correlation analyses ####

# load packages we need
library(car)
library(Hmisc)
library(RcmdrMisc)
library(lmtest)
library(effects)
library(tinytex)

# Let's look at the relationship between Cd and Zn in the Smiths Lake and
# Charles Veryard Reserves data from 2017. For Pearson's correlation we
# need variables with normal distributions, so first log-transform Cd and Zn...

# [the data file supplied to you might have a different name, e.g.
# sv2017_original.csv].

# read data and intial transformations
sv2017 <- read.csv(file = "SmithsVeryard_data.csv", stringsAsFactors = TRUE)
sv2017$Cd.log <- log10(sv2017$Cd)
sv2017$Zn.log <- log10(sv2017$Zn)

# ...and test if the distributions are normal. Remember that the null hypothesis
# for the Shapiro-Wilk test is that "*the distribution of the variable of
# interest is not different from a normal distribution*". So, if the P-value
# $\ge$ 0.05, we can't reject the null and therefore our variable is normally
# distributed.

shapiro.test(sv2017$Cd)
shapiro.test(sv2017$Cd.log)
shapiro.test(sv2017$Zn)
shapiro.test(sv2017$Zn.log)

### Pearson's r changes with transformation ####

cor.test(sv2017$Cd, sv2017$Zn, alternative="two.sided", 
         method="pearson") # is Pearson valid?
cor.test(sv2017$Cd.log, sv2017$Zn.log, alternative="two.sided", 
         method="pearson") # is Pearson valid?

# For the Pearson correlation we get a t statistic and  associated degrees of
# freedom (df), for which there is a p-value. The null hypothesis is that there
# is "*no relationship between the two variables*", which we can reject if p <
# 0.05. We also get a 95% confidence interval for the correlation coefficient,
# and finally an estimate of Pearson's r (cor).

# A Spearman coefficient doesn't change with transformation, since it is
# calculated from the ranks (ordering) of each variable.

cor.test(sv2017$Cd, sv2017$Zn, alternative="two.sided", 
         method="spearman") # can we use method="pearson"?
cor.test(sv2017$Cd.log, sv2017$Zn.log, alternative="two.sided", 
         method="spearman") # can we use method="pearson"?

# For the Spearman correlation we get an S statistic and  associated p-value.
# The null hypothesis is that there is no relationship between the two
# variables, which we can reject if p $\le$ 0.05. We also get an estimate of
# Spearman's rho (analogous to Pearson's r).

### Simple scatterplot by groups ####

# We're using base R to plot - you might like to try using
# scatterplot() from the car package. In base R we get the plot shown
# after the code below

# Before plotting, we first set our desired colour palette (optional) and
# graphics parameters (useful!):

# set palette par and plot
palette(c("black","blue","red3","darkgreen","purple",
          "darkcyan","sienna3","grey50","white"))
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.5,0.5,0), oma=c(0,0,0,0), tcl=0.2,
    cex=1.2, cex.lab=1.2, cex.axis=1., lend="square", ljoin="mitre", font.lab=2)

# . . . then plot the data
plot(sv2017$Cd~sv2017$Zn, col=c(6,7,1)[sv2017$Type], log="xy", 
     pch=c(16,1,15)[sv2017$Type], lwd=c(1,2,1)[sv2017$Type], cex=1.5,
     xlab="Zn (mg/kg)", ylab="Cd (mg/kg)")
abline(lm(sv2017$Cd.log~sv2017$Zn.log)) # line of best fit
legend("topleft", legend=c(levels(sv2017$Type),"Best-fit line for all types"), 
       cex=1.2, col=c(6,7,1,1), 
       pch=c(16,1,15,NA), pt.lwd=c(1,2,1), lwd = c(NA,NA,NA,1), 
       pt.cex=1.5, bty="n", inset=0.02,
       title=expression(italic("Sample type")))

## Correlation matrix - two ways of doing it

# We'll generate Spearman correlation matrices in these examples, but it's easy
# to change the code to generate Pearson (or other) correlation matrices.
# The p-values give the probability of the observed relationship if the
# null hypothesis (i.e. no relationship) is true.

# correlation matrix using Hmisc
require(Hmisc) # needed for rcorr() function
corr_table <- rcorr(as.matrix(sv2017[c("pH","Al","Ca","Fe","Cd","Pb","Zn")]), 
                    type="spearman")
print(corr_table)

# The output has three sub-tables:
# 1. the correlation coefficients for each pair of variables (note symmetry)
# 2. the number of pairs of observations for each relationship (some
#    observations may be missing)
# 3. the p values for each relationship (note symmetry)

# [Note that for Pearson correlations we instead use type="pearson" in
# the rcorr function.]

# Since a correlation coefficient is a standardised measure of association, we
# can treat it as an 'effect size'. Cohen (1988) suggested the
# following categories:
# 0 < |r| < 0.1 negligible 
# 0.1 < |r| < 0.3 small 
# 0.3 < |r| < 0.5 medium 
# 0.5 < |r| < 1 large 

## Correlation matrix with p-values corrected for multiple comparisons

# We can do this using the rcorr.adjust()function in the
# RcmdrMiscpackage which calculates corrected P-values. As
# above,the p-values give the probability of the observed relationship if the
# null hypothesis (i.e. no relationship) is true. Corrections are to reduce
# the risk of Type 1 Errors (false positives).
#
# We include use="pairwise.complete"in the rcorr.adjust()
# function parameters, otherwise the correlations are calculated on the number
# of observations for the variable with the most missing values (try changing it
# after reading help("rcorr.adjust"))

# correlation matrix using RcmdrMisc package, message=FALSE, warning=FALSE# 
require(RcmdrMisc) # needed for rcorr.adjust() function
rcorr.adjust(sv2017[c("pH","Al","Ca","Fe","Cd","Pb","Zn")], type="spearman", 
             use="pairwise.complete")

# The output from rcorr.adjust() has a fourth sub-table:
# 4. the adjusted p-values for each pair of variables, which depend
#    on the number of variables being compared

### scatter plot matrix to check correlation matrix

require(car) # needed for scatterplotMatrix() function
carPalette(palette())
scatterplotMatrix(~pH+ log10(Al)+ log10(Ca)+ log10(Cd)+ 
                    log10(Fe)+ log10(Pb)+ log10(Zn) | Type, 
                  smooth=FALSE, ellipse=FALSE, by.groups=TRUE, 
                  col=c(6,7,1), pch=c(16,1,15), cex.lab=1.5, data=sv2017,
                  legend=list(coords="bottomleft"))
# -=-=-=-

# A scatter plot matrix such as the one in spm is very useful for checking
# relationships established by numerical means such as correlation matrices. The
# graphical representation can identify if (a) a relationship exists
# but is masked since groups have different trends, or (b) there is no
# real relationship but features such as outliers result in an invalid
# significant correlation coefficient.

## Linear Regression

# We will first make a simple linear regression model predicting chromium from
# iron.

# We first create log-transformed variable columns and inspect the scatterplot
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(1.5,0.5,0), oma=c(0,0,0,0), tcl=0.2,
    cex=1.2, cex.lab=1.2, cex.axis=1., lend="square", ljoin="mitre", font.lab=2)
carPalette(palette())
sv2017$Fe.log <- log10(sv2017$Fe)
sv2017$Cr.log <- log10(sv2017$Cr)
scatterplot(Cr ~ Fe | Type, data = sv2017, 
            log = "xy", smooth = FALSE, col = c(6,7,1),
            xlab = "Fe (mg/kg)", ylab = "Cr (mg/kg)", 
            legend = list(coords = "bottomright"))

# What do the ungrouped scatterplot and regression line look like?

# Next use the lm() function to create a linear model object (give it a
# sensible name), then summarise it:

# simple regression
sv2017$Cr.log <- log10(sv2017$Cr); sv2017$Fe.log <- log10(sv2017$Fe)
lmCrFesimple <- with(sv2017, lm(Cr.log ~ Fe.log))
summary(lmCrFesimple)

# The output from a linear model summary in R is quite extensive:

# Call: gives the model we specified (for checking)

# Residuals: some summary statistics for the difference of the model from the
#   measured values -- these differences are called the residuals

# Coefficients: a table showing the parameters of the line of best fit, shown by
#   the estimates. The *intercept* of the line is in the first row, and the
#   *slope* labelled by the predictor variable. The other columns in the sub-table
#   give the uncertainty in the parameters (Std.Error), and the null hypothesis
#   p-value (Pr(>|t|)) based on a t-statistic for each parameter (against H0 that
#   there is no effect of a predictor, i.e. the slope = 0)

# Signif. codes: just explains the asterisks * or  or *

# The last block of text contains information on how well the model fits the
#   data. We will focus on the R-squared value, which is equivalent to the
#   proportion of variance in the dependent variable (Cr.log in this example)
#   which is explained by the predictor (Fe.log in this example). We should also
#   note the overall p-value, based on the variance ratio F-statistic, which
#   tests H0 that there is no effect of any predictor.

## Predicting chromium from iron using a regression model which varies by groups ####

# [Note the syntax used in the lm() function to separate by factor categories]

# grouped 1-predictor regr model
lmCrFe_byType <- with(sv2017, lm(Cr.log ~ Fe.log * Type))
summary(lmCrFe_byType)

# This is similar output to simple linear regression in the previous example,
# but the Coefficients: table is much more complicated.

# The first two rows of the Coefficients: table under the headings give the
# intercept and slope for the 'base case', which by default is the first group
# in the factor separating the groups. In this example the first level of the
# factor 'Type' is 'sediment' (frustratingly, the output does not show this).
# The next rows, starting with the factor name (i.e. TypeSoil and
# TypeStreetDust), show the *difference* between the intercepts for 'Soil'
# and 'Street dust' groups compared with the base case. Similarly, the final
# rows, beginning with the predictor variable name (Fe.log:TypeSoil and
# Fe.log:TypeStreet dust), show the *difference* between the slopes for
# 'Soil' and 'Street dust' groups compared with the base case.

### Compare the two models 

# Sometimes models can have greater R-sq but only because we've made them more
# complex by grouping our observations or by adding more predictors. We want the
# simplest model possible. We compare the models using an analysis of variance
# (where the null hypothesis is equal predictive ability). The models compared
# need to be nested, that is, one is a subset of the other.

# ANOVA compare nested models# 
anova(lmCrFesimple,lmCrFe_byType)

# The output here shows us on the basis of an F-test that we can reject H0, so
# the more complex model in this case really is better at prediction, since 
# p < 0.05.

## 'base R' scatter plots representing regression models ####

par(mfrow=c(1,2), mar=c(3,3,1,1), mgp=c(1.5,0.5,0), oma=c(0,0,0,0), tcl=0.2,
    cex=1.2, cex.lab=1.1, cex.axis=1., lend="square", ljoin="mitre", font.lab=2)
# simple scatterplot
with(sv2017, plot(Cr ~ Fe, log="xy"))
mtext(side=3, line=-1.2, text="(a)", adj=0.05, cex=1.4)
abline(lmCrFesimple, col=8, lty=2)
# grouped scatterplot
with(sv2017, plot(Cr ~Fe, log="xy", col=c(1,2,3)[Type], pch=c(0,2,16)[Type]))
mtext(side=3, line=-1.2, text="(b)", adj=0.05, cex=1.4)
# use for() {...} loop to add individual regression lines
for (i in 1:length(levels(sv2017$Type))) {
  abline(lm(log10(sv2017$Cr)~log10(sv2017$Fe),
            subset=sv2017$Type==levels(sv2017$Type)[i]), 
         col=i, lty=2)
}
legend("bottomright", legend=levels(sv2017$Type), 
       col=c(1,2,3), pch=c(0,2,16), bty="n", inset=0.02)

## Diagnostic plots for regression ####

# Simple linear regression, Cr ~ Fe

par(mfrow=c(2,2), mar=c(3,3,2,1))
plot(lmCrFesimple)
par(mfrow=c(1,1), mar=c(3,3,1,1))

### Grouped linear regression, Cr ~ Fe * Type

par(mfrow=c(2,2), mar=c(3,3,2,1))
plot(lmCrFe_byType)
par(mfrow=c(1,1), mar=c(3,3,1,1))

# The diagnostic plots are a visual test of some of the assumptions of linear
# regression models, which relate mainly to the residuals. An alternative from
# the car package is influenceIndexPlot(yourModelName).

# The top left and bottom left plots allow us to assess the assumption of
# homoscedasticity, that is, the residuals should be of similar absolute
# magnitude independent of the value of the dependent variable (actual or
# predicted). The top left plot also helps us to decide if the residuals are
# independent. In both the top left and bottom left plots, residuals should
# appear randomly distributed with a near-horizontal smoothed (red) line.

# The top right plot is a Q-Q plot which tests another assumption of regression;
# that the residuals should be normally distributed. The points should lie along
# (or close to) the theoretical (dotted) line.

# Finally the bottom left plot tests whether any observations have an unusual
# influence on the regression statistics (the assumption is that they do not).

# We can test all of these assumptions with formal statistical tests using the
# car (Fox and Weisberg, 2019) and lmtest (Zeileis and Hothorn, 2002)
# packages.

# The Breusch-Godfrey test is for residual autocorrelation; H0 is that
# residuals are not autocorrelated 
# (i.e. observations are probably independent)

# testing regression autocorrelation
require(lmtest)
require(car)
cat("------- Residual autocorrelation (independence assumption):")
bgtest(lmCrFe_byType) # Breusch-Godfrey test for autocorrelation (independence)
# -=-=-=-

# The Breusch-Pagan test is for heteroscedasticity; H0 is that residuals are
# homoscedastic (i.e. variance independent of value of variable).

# testing regression homoscedasticity
cat("\n------- Test of homoscedasticity assumption:")
bptest(lmCrFe_byType) # Breusch-Pagan test for homoscedasticity

# The Rainbow test is to test the assumption of linearity; H0 is that the
# relationship is linear.

# testing regression linearity
cat("\n------- Test of linearity assumption:")
raintest(lmCrFe_byType) # Rainbow test for linearity

# The outlierTest() function in the car package implements the Bonferroni
# outlier test; H0 is that all residuals are from the same population (i.e. no
# outliers). H0 is tested with the Bonferroni (NOT unadjusted) p-value. If no
# Bonferroni p<0.05 allows rejection of H0, the function outputs the largest
# residual(s).

# testing regression outliers
cat("\n------- Bonferroni Outlier test for influential observations:\n\n")
outlierTest(lmCrFe_byType) # Bonferroni outlier test 

### More things to try

# Try str(lm_object) and/or ls(lm_object) to see what is stored in regression
# results. You might be able to use the contents of a lm object to:

# plot calculated values (from the regression model) vs. measured values 
# add a line of slope 1 and zero intercept to the plot in 1. above
# find out if any regression residuals are unusual
# . . . and so on.

## Steps in running a multiple regression model ####

# Multiple regression models predict the value of one variable (the 'dependent
# variable') from two or more predictor variables (or just 'predictors'). They
# can be very useful in environmental science, but there are several steps we
# need to take to make sure that we have a valid model.

# In this example we're going to develop a regression model to predict soil
# copper (Cu) concentrations from several predictors. It makes sense to choose
# predictors that represent bulk soil properties that could plausibly control
# trace element concentrations. So, we choose variables like pH, EC, organic
# carbon, cation exchange capacity, and the major elements as predictors (but
# not other trace elements), as their concentrations are probably too low to
# control the concentration of anything else!)

# Since we don't have organic carbon or cation exchange capacity in this
# dataset, and there are many missing values for EC, our initial predictors
# will be Al, Ca, Fe, K, Mg, Na, pH, and S. Both the predictors and dependent
# variable need to be appropriately transformed before we start!

# Also, some of our initial predictors may be highly correlated (co-linear)
#  with each other. In multiple regression, we don't want to include co-linear
# predictors, since then we'll have two (or more) predictors which effectively
# contain the same information -- see below.

# Assess collinearity between initial set of predictors ####

# First we inspect the correlation matrix. It's useful to include the dependent
# variable as well, just to see which predictors are most closely correlated.

# (We try to generate a 'tidier' table by restricting numbers to 3 significant
# digits, based on the smallest r value in each column. We don't need to use
# rcorr() or rcorr.adjust(), since we're not so interested in P-values for
# this purpose.)

# Note that all variables are appropriately transformed!

cor0 <- 
  rcorr(as.matrix(sv2017[,c("Al.pow","Ca.pow","Fe.pow","K.log","Mg.log",
                            "Na.pow","pH","S.pow","Cu.pow")]))
print(round(cor0$r, 3))
rm(cor0)

# The rule of thumb we use is that if predictor variables are correlated with
# Pearson's r > 0.8 or r < -0.8, then the collinearity is too large and one of
# the correlated predictors should be omitted. In the correlation table above
# this applies to the correlation between [transformed] Ca and Mg, with r=0.85.
# In this example we will run two versions of the model, one keeping both Ca and
# Mg, and one omitting Mg.

# In either case, whether we run the model with or without omitting predictors,
# it's a good idea to calculate 'Variance Inflation Factors' on the predictor
# variables in the model (see below) which can tell us if collinearity is a
# problem.

# Generate multiple regression model for Cu (co-linear predictors KEPT) ####

# make new data object containing relevant variables with no missing values
sv2017_multreg <- na.omit(sv2017[c("Cu.pow","pH","Al.pow","Ca.pow","Fe.pow",
                                   "K.log","Mg.log","Na.pow","S.pow")])
# run model using correctly transformed variables
lm_multi <- lm(Cu.pow ~ pH + Al.pow + Ca.pow + Fe.pow + K.log + Mg.log + 
                 Na.pow + S.pow, data=sv2017_multreg)
summary(lm_multi)

# Note that the null hypothesis probability Pr(>|t|) for some predictors is
# >0.05, so we can't reject the null hypothesis -- that this predictor has no
# effect on the dependent variable.

### Calculate variance inflation factors (VIF) for the predictors in the 'maximal' model 

require(car)
{cat("Variance Inflation Factors\n")
  vif(lm_multi)}

# A general rule of thumb is that if VIF > 4 we need to do some further
# investigation, while VIFs > 10 are signs of serious multi-collinearity
# requiring correction, such as removing one of a collinear pair of variables
# (Hebbali, 2018). As we probably expected from the correlation coefficient
# (above), VIFs for both Ca and Mg are >4 in this model (and remember they had
# Pearson's r > 0.8), so we should try a model which omits Ca or Mg (we'll
# choose Mg)...

# Multiple regression model for Cu, OMITTING co-linear predictors ####

# make new data object containing relevant variables with no missing values
sv2017_multreg <- na.omit(sv2017[c("Cu.pow","pH","Al.pow","Ca.pow","Fe.pow",
                                   "K.log","Mg.log","Na.pow","S.pow")])
# run model using correctly transformed variables 
#   (omitting co-linear predictors)
lm_multi2 <- lm(Cu.pow ~ pH + Al.pow + Ca.pow + Fe.pow + 
                  K.log + Na.pow + S.pow, data=sv2017_multreg)
summary(lm_multi2)

# Note that again the null hypothesis probability Pr(>|t|) for some predictors
# is >0.05, so we can't reject the null hypothesis --that this predictor has no
# effect on the dependent variable.

# calculate VIF for the model omitting co-linear predictors ####

require(car)
{cat("Variance Inflation Factors\n")
  vif(lm_multi2)}

# With the co-linear variable(s) omitted (on the basis of |Pearson's r| > 0.8),
# we now have no VIFs > 4. So we can move on to stepwise refinement of our
# [new] 'maximal' model...

### Stepwise refinement of maximal multiple regression model
#   (omitting co-linear predictors)

# We don't want to have too any predictors in our model -- just the predictors
# which explain significant proportions of the variance in our dependent
# variable. In addition, our data may be insufficient to generate a very complex
# model; one rule-of-thumb suggests 10-20 observations are needed to calculate
# coefficients for each predictor. Riemann et al. (2008) recommend that the
# number of observations should be at least 5 times the number of predictors.
# So, we use a systematic stepwise procedure to test variations of the model,
# which omits unnecessary predictors.

# stepwise refinement of multiple regression model
lm_stepwise <- step(lm_multi2, direction="both", trace=0)
# ls(lm_stepwise) # if we want to see what's in the output object
summary(lm_stepwise)
require(car)
{cat("\nVariance Inflation Factors\n")
  vif(lm_stepwise)}

# In the optimised model, we find that the stepwise() procedure has generated a
# new model with fewer predictor variables. You should notice that the p-values
# (Pr(>|t|)) for intercept and predictors are all now < 0.05, so we
# can reject the null hypothesis for all predictors (i.e. none of them
# have 'no effect' on Cu). Our VIFs are now all close to 1, meaning negligible
# collinearity between predictors.

# It's always a good idea to run diagnostic plots (see diagnostic
# below) on a regression model (simple or multiple), to check for (i) any
# systematic trends in residuals, (ii) normally distributed residuals, and 
# (iii) any unusually influential observations.

### regression diagnostic plots

par(mfrow=c(2,2), mar=c(3.5,3.5,1.5,1.5), mgp=c(1.6,0.5,0), 
    font.lab=2, font.main=3, cex.main=0.8, tcl=-0.2)
plot(lm_stepwise)
par(mfrow=c(1,1))
# -=-=-=-

# The optimal model is Cu.pow ~ Al.pow + Ca.pow + Fe.pow + S.pow, where
# suffixes .pow and .log represent power- and log10-transformed variables
# respectively. The point labelled '86' does look problematic...

# testing regression assumptions
require(lmtest)
require(car)
cat("------- Residual autocorrelation (independence assumption):")
bgtest(lm_stepwise) # Breusch-Godfrey test for autocorrelation (independence)
cat("\n------- Test of homoscedasticity assumption:")
bptest(lm_stepwise) # Breusch-Pagan test for homoscedasticity
cat("\n------- Test of linearity assumption:")
raintest(lm_stepwise) # Rainbow test for linearity
cat("\n------- Bonferroni Outlier test for influential observations:\n\n")
outlierTest(lm_stepwise) # Bonferroni outlier test for influential observations

# multiple regression effect plots

require(effects)
plot(allEffects(lm_stepwise, confidence.level=0.95))

# The effects plots show that the confidence interval for
# prediction of is narrowest (most precise) for Fe, and least precise
# for Ca. Effects for Ca, Fe, and S are positive, with Al having a
# negative effect on Cu.

# Scatterplot of observed vs. fitted values

par(mar=c(4,4,1,1), mgp=c(2,0.5,0), font.lab=2, cex.lab=1.2, 
    lend="square", ljoin="mitre")
plot(sv2017_multreg$Cu.pow ~ lm_stepwise$fitted.values,
     xlab="Cu.pow predicted from regression model",
     ylab="Cu.pow measured values", pch=3, lwd=2, 
     cex=0.8, col="blue3")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", legend=c("1:1 line"), col="red", 
       text.col="red", pch=NA, lty=2, lwd=2, 
       box.col="grey", box.lwd=2, inset=0.02, seg.len=2.7)
mtext(side=3, line=-4., adj=0.03, col="blue3",
      text=paste("Adj. R\u00B2 =",signif(summary(lm_stepwise)$adj.r.squared,3)))

# For a multiple regression model, where several variables predict the value of
# a single variable, an "observed vs. fitted" plot is the only easy way of
# visualising the goodness-of-fit of the model without trying to draw a plot in
# multiple dimensions. For instance, if we had 3 predictors we would need a
# 4-dimensional plot, which would be tricky!

# Some brief interpretation ####

# The adjusted R-squared value of the final model is 0.506, meaning that 50.6%
# of the variance in Cu is explained by variance in the model's predictors.
# (The remaining 49.4% of variance must therefore be due to random variations,
# or 'unknown' variables not included in our model.)
# From the model coefficients and the effect plots we can see that Cu
# increases as Ca, Fe, and S increase, but Cu decreases as Al increases. This
# doesn't necessarily correspond with the individual relationships; Cu IS
# positively correlated with Ca, Fe, and S, but actually has no significant
# relationship with Al (you can check this!).
# Although we can't attribute a causal relationship to correlation or
# regression relationships, the observed effects in our model are consistent
# with real phenomena. For example, copper is positively related to iron (Fe)
# in soils at the continental scale; see Hamon *et al*. (2004) and Caritat and
# Rate (2017).

# References and Packages ####

# Caritat, P. and Rate, A. W. (2017). Detecting anomalous metal concentrations
# in the regolith using cross-compositional detrending. *Paper presented at the
# Goldschmidt Conference 2017*, Paris, France.
# https://whiteiron.org/uploads/conferences/27/abstracts/finalPDFs/2017002103-20170327183746.pdf
#
# Cohen, J. 1988. *Statistical Power Analysis for the Behavioral Sciences*,
# Second Edition. Erlbaum Associates, Hillsdale, NJ, USA.
#
# Fox, J. (2020). *RcmdrMisc: R Commander Miscellaneous Functions. R package
# version 2.7-1*.   https://CRAN.R-project.org/package=RcmdrMisc
#
# Fox, J. and Weisberg, S. (2019). An R Companion to Applied Regression,
# Third Edition. Thousand Oaks   CA: Sage. URL:
# https://socialsciences.mcmaster.ca/jfox/Books/Companion/
#
# Hamon, R. E., McLaughlin, M. J., Gilkes, R. J., Rate, A. W., Zarcinas, B.,
# Robertson, A., Cozens, G., Radford, N. and Bettenay, L. (2004). Geochemical
# indices allow estimation of heavy metal background concentrations in soils.
# Global Biogeochemical Cycles, 18(GB1014),
# http://dx.doi.org/10.1029/2003GB002063.
#
# Harrell Jr, F.E. with contributions from Charles Dupont and many others.
# (2020). Hmisc: Harrell   Miscellaneous. R package version 4.4-1.
# https://CRAN.R-project.org/package=Hmisc
#
# Hebbali, A. (2018). Collinearity Diagnostics, Model Fit and Variable
# Contribution. Vignette for R Package 'olsrr'. Retrieved 2018.04.05, from
# https://cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html.
#
# Reimann, C., Filzmoser, P., Garrett, R.G., Dutter, R., (2008).
# Statistical Data Analysis Explained: Applied Environmental Statistics
# with R. John Wiley and Sons, Chichester, England (see Chapter 16).
#
# Zeileis, A., Hothorn, T. (2002). Diagnostic Checking in Regression
# Relationships. *R News* 2(3), 7-10.  https://CRAN.R-project.org/doc/Rnews/
