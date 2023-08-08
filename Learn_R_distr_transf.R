# Week 3 - distributions and transformation of variables
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# In case we need to read in the data... uncomment the line below and run it
# hubbard <- read.csv("hubbard.csv", stringsAsFactors = TRUE)

#### Visualizing distributions ####

# We can use histograms, density plots, or Q-Q plots;
# probably histograms are easiest to interpret.
hist(log10(hubbard$Cd), breaks=15, freq=F, col="#e0e0ff")
lines(density(na.omit(log10(hubbard$Cd))), col="#8080c0", lwd=4)

# it's worth looking at _log-transformed_ histograms since bimodal or
# multi-modal distributions may be easier to see:
# From the histograms we can see if (1) the variable is skewed 
# (and therefore probably not normally distributed), or 
# (2) if the distribution appears to be bimodal
# or multi-modal.

# Q-Q- plots

require(car) # the qqPlot function in car is better than base-R qqplot
qqPlot(hubbard$Al)
qqPlot(log10(hubbard$Al)) # not perfect but better than untransformed

## power transformation

# sometimes a log transformation will not give our variable(s)
# a normal distribution
# if so, we can try a power transformation.
# to do this though, we need to know what power to 
# raise our variable to!
# there is an algorithm called 'Box-Cox' which can estimate the 
# power term. It's the basis of the powerTransform() function
# in the car package.

powerTransform(hubbard$OM.pct)
hist(hubbard$OM.pct, breaks=15)
hist(hubbard$OM.pct^0.0125, breaks=15)

powerTransform(hubbard$Al)
qqPlot(-1*(hubbard$Al^-0.1061))

### NOTE that we use a different form of the power 
# transformation equation, depending on whether the
# estimated power term is positive or negative
# If the power term is negative, we multiply the 
# answer by -1, otherwise the order of observations is
# reversed! (lowest becomes highest and vice-versa)

## Tests for normality ####

# OF COURSE these analyses of distributions are only visual. 
# We should also run tests for normality such as Shapiro-Wilk:

sv2017 <- read.csv(file="sv2017_original.csv", stringsAsFactors = TRUE)

shapiro.test(sv2017$Ca)
shapiro.test(log10(sv2017$Ca))
powerTransform(sv2017$Ca)
shapiro.test(-1*(sv2017$Ca^-0.195))

# Which transformation makes sv2017$Ca normally distributed?

#### many transformations efficiently with R programming ####
#
# the following code uses a programming loop and 
# if-then structure to semi-automate the process
#
# this version creates new log- and power-transformed variables
# and tests all [un]transformed variables for normality
#
# load required packages
require(car)
# create temp object with names of variables to be transformed
names.of.cols <- names(sv2017)
#
# generate matrix of comma separated values
# and calculate new variables
#
# define starting and ending columns
c1 <- 9
cn <- 36
# make initial output data frame
transf_results <- data.frame("Variable"=seq(c1,cn),
                             "W_orig"=seq(c1,cn),
                             "p_orig"=seq(c1,cn), "W_log_tr"=seq(c1,cn),
                             "p_log_tr"=seq(c1,cn), "W_pow_tr"=seq(c1,cn),
                             "p_pow_tr"=seq(c1,cn), "Pow_term"=seq(c1,cn))
# start loop that assesses variable distributions and creates new variables
for (i in c1:cn) {
  pt1 <- powerTransform(sv2017[, i])
  sv2017[paste0(names.of.cols[i],".log")]<-log10(sv2017[i])
  # if ... else applies factor of -1 to
  # power transforms with negative terms
  # delete next 8 lines of you don't want new columns
  if (as.vector(pt1$lambda) > 0) {
    sv2017[paste0(names.of.cols[i], ".pow")] <-
      sv2017[i] ^ as.numeric(unlist(pt1$lambda))
  }
  else {
    sv2017[paste0(names.of.cols[i], ".pow")] <-
      -1 * ((sv2017[i]) ^ as.numeric(unlist(pt1$lambda)))
  }
  # generate and save test statistics
  sw0 <- shapiro.test(sv2017[, i])
  sw1 <- shapiro.test(log10(sv2017[, i]))
  sw2 <- shapiro.test((sv2017[, i]) ^ as.vector(pt1$lambda))
  transf_results[i-(c1-1),] <- c(names.of.cols[i], signif(sw0$statistic, 4),
                           signif(sw0$p.value, 4), signif(sw1$statistic, 4),
                           signif(sw1$p.value, 4), signif(sw2$statistic, 4),
                           signif(sw2$p.value, 4), signif(as.vector(pt1$lambda), 4))
}
#
# output to console (screen)
cat("Table. Shapiro-Wilk statistics and p-values for untransformed (_orig) and transformed
(_log, _pow) variables from soil and sediemnt analysis at Smith's Lake Reserve.\n\n")
print(transf_results, row.names = FALSE)

##
# export results to a csv file for Excel (if desired)
write.csv(transf_results, file = "transformations.csv", row.names = FALSE)
# remove temporary objects
# to keep R workspace tidy
rm(list=c("names.of.cols","pt1","sw0","sw1","sw2","i"))
# end code

# Once you get this to work you should verify that it worked!
# e.g. by (1) plotting a variable against its transformed version
#  or (2) comparing histograms or QQ-plots for transformed and
# untransformed variable pairs