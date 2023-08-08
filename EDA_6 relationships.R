#### -+-+-+-+- EDA WORKFLOW 6: Relationships -+-+-+-+- ####
# Relationships Between variables ####

## Use a scatter plot matrix (car package)
library(car)
palette(c("black", rainbow(12, v = 0.75, end = 0.7)))
par(mar=c(3,3,1,1),mgp=c(1.7,0.3,0),tcl=0.3,font.lab=2,
    lend="square",ljoin="mitre")
# the function scatterplotMatrix() can be
# abbreviated to spm()
spm(~Al + log10(Fe) + log10(Ca) + pH + 
      Cu + log10(Pb) + log10(Zn), 
    data = afs20, 
    smooth = FALSE)

spm(~Al + log10(Fe) + log10(Ca) + pH + 
      Cu + log10(Pb) + log10(Zn) | Zone, 
    data = afs20, col = c(1,2,13), 
    smooth = FALSE)

# WATER
spm(~log10(Fe) + log10(Ca) + 
      log10(Na) + log10(S) + pH, 
    data = afw20, col = c(1,2,4,11,13), 
    smooth = FALSE)

spm(~log10(Fe) + log10(Ca) + 
      log10(Na) + log10(S) + pH | Zone, 
    data = afw20, col = c(1,2,4,11,13), 
    smooth = FALSE)

# Correlations ####

# don't forget to ADJUST for multiple comparisons if
# you are making a correlation matrix
# also use the option 'use = "pairwise.complete.obs" ' in
# the rcorr.adjust function so that all possible data are 
# used in calculating correlations
library(RcmdrMisc)
rcorr.adjust(afs20[,c("Ca.log","Fe.log","pH",
                      "Cu.log","Pb.log","Zn.log")],
             type = "spearman", 
             use = "pairwise.complete.obs")
# but never forget to check apparent correlations
#  (or lack of correlation) with scatter plots!
require(car)
par(mar=c(3,3,1,1),mgp=c(1.7,0.3,0),tcl=0.3,font.lab=2,
    lend="square",ljoin="mitre")
# scatterplotMatrix() abbreviated to spm()
spm(~log10(Fe) + log10(Ca) + pH + 
      log10(Cu) + log10(Pb) + log10(Zn) | Zone, 
    data = afs20, smooth = FALSE, col = c(2,4,6))

# Regression Models ####

# The first question to ask is "do I really need a regression model?".
# You might want one to:
#   1. predict something (e.g. filling in missing values, estimating 
#      values of a measurement we didn't do based on another dataset)
#   2. Look for unusual values of a variable (e.g. a contaminant)
#      based on analysis of regression residuals
#   3  To show parametrically that relationships between variables 
#      differ in different environments (using grouped regression).
# If we are just interested in relationships to infer the effect of
# X on Y, correlation is probably a better option (or maybe multivariate
# methods based on correlation such as Principal Components Analysis)

## Regression for prediction  ####
# in the afw20 dataset, there are some missing Na concentrations in
# rows 3, 4, and 22. Since Na+ is a major ion in natural waters, we 
# may be able to predict its value from EC. 
# First, let's inspect the data
afw20[,c("EC", "Na")]
# Unfortunately EC is also missing in row 3, but we can still 
# try to "fill in" the two missing Na values in rows 4 and 22
# Next plot Na vs EC...
with(afw20, plot(Na ~ EC, log = "xy"))
# ...and add the regression line using logs since we plotted 
# with log-transformed axes
abline(lm(log10(Na) ~ log10(EC), data = afw20), col = "red2")

# The relationship isn't visually perfect but let's try anyway.
afw20$EC.log <- log10(afw20$EC)
afw20$Na.log <- log10(afw20$Na)
Na_model <- lm(Na.log ~ EC.log, data = afw20)
summary(Na_model)
par(mfrow=c(2,2)); plot(Na_model); par(mfrow=c(1,1))

# We use the predict() function
# the interval can also be "confidence" (default confidence interval 95%)
Na_predict <- predict(Na_model, afw20[c(4,22),], interval = "prediction")
print(Na_predict)

# and visualize with a plot
with(afw20, plot(Na.log ~ EC.log, ylim = c(1,4.4)))
abline(Na_model, col = "red2")
points(Na_predict[1:2] ~ afw20[c(4,22),"EC.log"], col = 4, pch=15)
# make some error bars representing the interval ("confidence" or "prediction")
arrows(x0 = afw20[c(4,22),"EC.log"], y0 = Na_predict[3:4], 
       y1 = Na_predict[5:6], 
       col = 4, angle = 90, length = 0.1, code = 3)

# (prediction using a linear model will also work for other types of
#  regression such as multiple or grouped)

## Regression for identifying unusual values ####

# using the afs20 (sediment) data
with(afs20, plot(As ~ Fe, log = "xy"))
afs20$As.log <- log10(afs20$As)
afs20$Fe.log <- log10(afs20$Fe)
abline(lm(As.log ~ Fe.log, data = afs20), col = 4)
As_model <- lm(As.log ~ Fe.log, data = afs20)
summary(As_model)
par(mfrow=c(2,2)); plot(As_model); par(mfrow=c(1,1))
# the diagnostic plots suggest some unusually high residuals
# calculate the scaled residuals (to normal distribution mean = 0, SD = 1)
z_resid <- scale(As_model$residuals)
stripchart(z_resid, method = "jitter", vertical = TRUE)
# a "rule-of-thumb" or heuristic is that scaled residuals > 2 are unusual
# as they are > 2 standard deviations from the mean:
abline(h = 2, col = 2, lty = 2)

afs20$As_resid <- scale(As_model$residuals)
which(afs20$As_resid>2)
# so 5 residuals suggest unusual As concentrations...
# ... so let's use this information to find out which samples
afs20[which(afs20$As_resid>2), c("Sample","Fe","As","As.log","As_resid")]
# note that we have categorised even some quite low As concentrations
# as 'unusual' since the samples also have lower Fe.
# Only 2 samples exceed the ISQG-Low trigger of As > 20mg/kg ,
# and none exceed the ISQG-High trigger of 70 mg As/kg
# 
# One interpretation is that the samples with scaled As residuals >2
# represent addition of As, possibly by human activity.

## Regression to show different relationships for different groups ####
require(car)
# compare the following 2 plots:
scatterplot(Zn ~ Fe, data = afs20, smooth = F, log = "xy", boxplots = FALSE)
scatterplot(Zn ~ Fe | Zone, data = afs20, smooth = F,
            legend = list(coords = "topleft"),
            col = c(1,2,4), log = "xy")
# make some matching regression models
afs20$Zn.log <- log10(afs20$Zn)
afs20$Fe.log <- log10(afs20$Fe)

Zn_model1 <- lm(Zn.log ~ Fe.log, data = afs20)
summary((Zn_model1))

Zn_model2 <- lm(Zn.log ~ Fe.log * Zone, data = afs20)
summary((Zn_model2))

# there's a much greater R-squared for the grouped model but we should test to
# see if the improvement is statistically significant.
# For nested models we can use anova, where the null hypothesis is that the two
# models are equivalent in prediction ability.
# (regression models are called nested if one contains all the predictors
# of the other, and some additional predictors)
anova(Zn_model1, Zn_model2)

# if p < 0.05 we conclude that the model with greater R-squared is better!
# We probably would have guessed this by comparing the un-grouped and
# grouped scatter plots
