#### Steps in running a multiple regression model ####
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
# Multiple regression models predict the value of one variable 
# (the 'dependent variable') from two or more predictor variables 
# (or just 'predictors'). They can be very useful in environmental 
# science, but there are several steps we need to take to make sure  
# that we have a valid model.
#   In this example we're going to develop a regression model to 
# predict molybdenum (Mo) concentrations from several predictors. 
# It makes sense to choose predictors that represent bulk soil properties 
# that could <u>plausibly</u> control trace element concentrations. So, 
# we choose varibales like pH, EC, organic carbon, cation exchange 
# capacity, and the major elements as predictors (but not other 
# trace elements, as their concentrations are probably 
# too low to control the concentration of any thing else!)
# Since we don't have organic carbon or cation exchange capacity 
# in this dataset, and there are many missing values for EC, our initial 
# predictors will be Al, Ca, Fe, K, Mg, Na, pH, and S. 
# Both the predictors and dependent variable need to be appropriately 
# transformed before we start!
#   Also, some of our initial predictors may be highly correlated 
# (co-linear) with each other. In multiple regression, we don't want to 
# include co-linear predictors, since then we'll have two (or more) 
# predictors which effectively contain the same information - see below.
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
#
#### Read input data ####
#
afw19 <- read.csv("afw19_export.csv")
print(afw19[1:5,2:11], row.names = FALSE)
#
#### Assess collinearity between initial set of predictors ####
#
#   First we inspect the correlation matrix. It's useful to include the 
# dependent variable as well, just to see which predictors are 
# most closely correlated.
# 
# (We try to generate a 'tidier' table by restricting numbers to 3 
#  significant digits, based on the smallest r value in each column.)
# 
# Note that all variables are appropriately transformed!
# 
require(Hmisc)
cor0 <- rcorr(as.matrix(afw19[,c("Al.log","Ca.log","Fe.log",
                                 "K.log","Mg.log","Na.log",
                                 "pH.log","EC.log","S.log","Mo.log")]))
print(cor0$r, digits=3)
rm(cor0)
#
#    The rule of thumb we use is that if predictor variables are
# correlated with Pearson's r > 0.8 or r < -0.8, then the 
# collinearity is too large and one of the correlated predictors 
# should be omitted. In the correlation table above this applies 
# to the correlation between [transformed] Ca and Mg, with r=0.85. 
#     In this example we will run two versions of the model, one 
# keeping both Ca and Mg, and one omitting Mg.
#     In either case, whether we run the model with or without 
# omitting predictors, it's a good idea to calculate 
#'Variance Inflation Factors' on the predictor 
# variables in the model (see below) which can tell us if 
# collinearity is a problem.
#
#### generate mutiple regression model for Mo (co-linear predictors NOT omitted) ####
#
# make new data object containing relevant variables with no missing values
#
afw19_multreg <-
  na.omit(afw19[c("Al.log","Ca.log","Fe.log",
                  "K.log","Mg.log","Na.log",
                  "pH.log","EC.log","S.log","Mo.log")])
# run model using correctly transformed variables
lm_multi <- lm(Mo.log ~ pH.log + Al.log + Ca.log + Fe.log + 
                 K.log  + Na.log + S.log, 
               data=afw19_multreg)
summary(lm_multi)
#
# Note that the null hypothesis probability Pr(>|t|) for some predictors 
# is >0.05, so we can't reject the null hypothesis - that this predictor 
# has no effect on the dependent variable.
#
# calculate variance inflation factors (VIF) ####
#     for the predictors in the 'maximal' model 
require(car)
{cat("Variance Inflation Factors\n")
  vif(lm_multi)}
#
# A general rule of thumb is that if VIF > 4 we need to do some further 
# investigation, while VIFs > 10 are signs of serious multicollinearity requiring 
# correction (Hebbali, 2018). As we probably expected from the correlation 
# coefficient (above), VIFs for both Ca and Mg are >4 in this model, 
# so we should try a model which omits Ca or Mg (we'll choose Mg)...
#
# generate mutiple regression model for Mo omitting co-linear predictors ####
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
# make new data object containing relevant variables with no missing values
afw19_multreg <- na.omit(afw19[c("Mo.log","pH","Al.log","Ca.log",
                                   "Fe.log","K.log","Mg.log","Na.log","S.log")])
# run model using correctly transformed variables (omitting co-linear predictors)
lm_multi2 <- lm(Mo.log ~ pH.log + Al.log + Ca.log + Fe.log + 
                  K.log  + S.log, 
                data=afw19_multreg)
summary(lm_multi2)
#
# Note that again the null hypothesis probability Pr(>|t|) for some 
# predictors is >0.05, so we can't reject the null hypothesis - that 
# this predictor has no effect on the dependent variable.

# calculate variance inflation factors (VIF) for the model omitting 
#   co-linear predictors
require(car)
{cat("Variance Inflation Factors\n")
  vif(lm_multi2)}
#
# With the co-linear variable(s) omitted (on the basis of |Pearson's r| > 0.8), 
# we now have no VIFs > 4. So we can move on to stepwise refinement of our 
# [new] 'maximal' model...
#
# stepwise refinement of maximal multiple regression model ####
    # (omitting co-linear predictors)
lm_stepwise <- step(lm_multi2, direction="both", trace=0)
summary(lm_stepwise)
require(car)
{cat("Variance Inflation Factors\n")
vif(lm_stepwise)}
    #
    # try just a forward model
    lm_min = lm(Mo.log ~ 1, data = afw19_multreg)
    lm_fwd = step(lm_min, direction = "forward", 
                     scope=(~ Mo.log ~ pH.log + Al.log + Ca.log + 
                             Fe.log + K.log + S.log))
    summary(lm_fwd)
    #
# In the optimised model, we find that the stepwise procedure has 
# generated a new model with fewer predictor variables. You should 
# notice that the p-values (Pr(>|t|)) for intercept and predictors 
# are all now <0.05, so we can reject the null hypothesis for all 
# predictors (i.e. none of them have 'no effect' on Mo). 
# Our VIFs are now all close to 1, meaning negligible collinearity 
# between predictors.
# 
# It's always a good idea to run diagnostic plots (see below) on a 
# regression model (simple or multiple), to check for (i) any 
# systemaic trends in residuals, (ii) normally distributed residuals, 
# and (iii) any unusually influential observations.
#   
# regression diagnostic plots ####
# -~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~
par(mfrow=c(2,2), mar=c(3.5,3.5,1.5,1.5), mgp=c(1.6,0.5,0), 
    font.lab=2, font.main=3, cex.main=0.8, tcl=-0.2)
plot(lm_stepwise)
par(mfrow=c(1,1))
#
# multiple regression effect plots ####
require(effects)
plot(allEffects(lm_stepwise, confidence.level=0.95))
#
# scatterplot of observed vs. fitted values ####
par(mar=c(4,4,1,1), mgp=c(2,0.5,0), font.lab=2, cex.lab=1.2, 
    lend="square", ljoin="mitre")
plot(afw19_multreg$Mo.log ~ lm_stepwise$fitted.values,
     xlab="Mo.log predicted from regression model",
     ylab="Mo.log measured values", pch=3, lwd=2, 
     cex=0.8, col="blue3")
abline(0,1, col="red", lty=2, lwd=2)
legend("topleft", legend=c("Observations","1:1 line"), col=c("blue3","red"), 
       text.col=c("blue3","red"), pch=c(3,NA), lty=c(NA,2), pt.lwd=2, lwd=2, 
       box.col="grey", box.lwd=2, inset=0.02, seg.len=2.7, y.intersp=1.2)
mtext(side=3, line=-5.5, adj=0.05, col="blue3",
      text=paste("Adjusted R\u00B2 =",
                 signif(summary(lm_stepwise)$adj.r.squared,3)))
#
# brief interpretation
#1. The adjusted R&sup2; value of the final model is 0.506, meaning 
#   that 50.6% of the variance in Mo is explained by variance in the 
#   model's predictors. [The remaining 49.4% ofvariance must therefore 
#   be due to random variations, or 'unknown' variables not included 
#   in our model.]
#2. From the model coefficients and the effect plots we can see that 
#   Mo increases as pH and S increase, but Mo decreases as Ca 
#   increases. This doesn't necessarily correspond with the 
#   individual relationships; as it happens, Mo is *positively* 
#   correlated with Ca, pH, and S (you can check this!), even though 
#   the regression model has a negative coefficient for Ca.
#3. Although we can't attribute a causal relationship to correlation 
#   or regression relationships, the observed effects in our model 
#   are consistent with real phenomena. 
#   For example, we would expect Mo concentrations in water to increase 
#   with increasing pH, since at low pH the molybdate anion will adsorb 
#   to sediment components much more strongly. Greater concentrations of 
#   S (as sulfate) would be likely to displace adsorbed molybdate, also 
#   giving greater Mo concentration in water (see Sposito 2008, Ch.8).
#
# references ####
# Hebbali, A. (2018). Collinearity Diagnostics, Model Fit & Variable Contribution. 
#   Vignette for R Package 'olsrr'  Retrieved 2018.04.05, from 
#   cran.r-project.org/web/packages/olsrr/vignettes/regression_diagnostics.html.
# Sposito, G., 2008 The Chemistry of Soils, Second Edition. Oxford University 
#   Press, New York, 329 pp.
# 
# [end code]