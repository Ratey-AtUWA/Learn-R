#### -+-+-+-+- EDA WORKFLOW 3: distributions -+-+-+-+- ####

# Visualizing distributions ####
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# We can use histograms, density plots, or Q-Q plots;
# probably histograms are easiest to interpret.
# We can draw these one at a time, e.g.:
hist(afs20$Al)

# For more efficiency we could use a programming loop:
# first make a vector of column names in our data:
varnames <- names(afs20[,1:38]) # column 38 is last concentration
# check this vector
varnames
# set graphics parameters to plot multiple frames (etc.)
# Note that mfrow = c(4,4) gives us 4 x 4 = 16 plots
par(mfrow = c(4,4), mar = c(4,4,1,1), mgp = c(1.7,0.3,0), 
    tcl = 0.25, font.lab=2)
# then make a loop for first 16 columns:
for(i in 9:24){
  hist(afs20[,i], xlab=NA, main=NA, breaks = 15)
  mtext(varnames[i], side=1, line = 2, cex = 1.2, font = 2)
}
# and then the rest, adding density curves this time:
for(i in 25:38){
  hist(afs20[,i], xlab=NA, main=NA, breaks = 15, freq = FALSE)
  lines(density(afs20[,i], na.rm = TRUE), col = "blue2")
  mtext(varnames[i], side=1, line = 2, cex = 1.2, font = 2)
}
# or, we could just use (i in 9:38) and use the 'Previous plot' and
# 'Next plot' arrows in the Plots pane to see all the output.

# it's worth looking at _log-transformed_ histograms since bimodal or
# multi-modal distributions may be easier to see:
par(mfrow = c(4,4))
for(i in 9:38){
  hist(log10(afs20[,i]), xlab=NA, main=NA, breaks = 15)
  mtext(paste0("log10(",varnames[i],")"), 
        side=1, line = 2, cex = 1.2, font = 2)
}
par(mfrow = c(1,1)) # reset back to one plot per page

# From the histograms we can see if (1) the variable is skewed 
# (and therefore probably not normally distributed), or 
# (2) if the distribution appears to be bimodal
# or multi-modal.

# or, if you prefer, Q-Q plots
# (using the better plots in the car R package):
library(car)
par(mfrow = c(4,3), mar = c(4,4,1,1), mgp = c(1.7,0.3,0), 
    tcl = 0.25, font.lab = 2)
# omitting column 15 (Cd) which is mostly zeros
for(i in c(9:14,16:38)){
  qqPlot(afs20[,i], ylab=NA, main=NA, lwd=1, cex = 1.4)
  mtext(varnames[i], side=2, line = 2, cex = 1.2, font = 2, las = 1)
}
# 'las = 1' makes axis labels horizontal
par(mfrow = c(1,1)) # reset back to one plot per page

## You'd THINK there'd be a PACKAGE to do all this... ####
# ...and there is :^)
library(DataExplorer)
plot_histogram(afs20[,9:38])
# and also (among many other options)
plot_qq(afs20[,9:17])

## We can also use scatterplotMatrix() in the car package:
scatterplotMatrix(~pH + EC + Al + As + Ba + Ca + Ce + Co + Cr |Zone, 
                  data = afs20, smooth = FALSE,
                  col = c("blue","darkcyan","darkmagenta"))
# Or if we want histograms on the diagonal:
scatterplotMatrix(~pH + EC + Al + As + Ba + Ca + Ce + Co + Cr |Zone, 
                  data = afs20, 
                  diagonal = list(method ="histogram", breaks=15),
                  smooth = FALSE,
                  col = c("blue","darkcyan","darkmagenta"))
# The diagonal displays some form of distribution plot. 
# We'll use the scatterplotMatrix function in a later session 
# to help us visualise relationships between variables.

## Tests for normality ####
# OF COURSE these analyses of distributions are 
# only visual. We should also run tests for 
# normality such as Shapiro-Wilk:
shapiro.test(afs20$Al)

# We've already seen how we can do this in a loop:
require(car)
data <- afs20; data$Cd <- NULL # Cd column has mostly zeros
names.of.cols <- names(data)
c1 <- 9 # define starting column
cn <- 37 # define ending column
# make initial output data frame
table <- data.frame("Variable"=seq(c1,cn), 
                    "W_orig"=seq(c1,cn), 
                    "p_orig"=seq(c1,cn), "W_log_tr"=seq(c1,cn),
                    "p_log_tr"=seq(c1,cn), "W_pow_tr"=seq(c1,cn),
                    "p_pow_tr"=seq(c1,cn), "Pow_term"=seq(c1,cn))
# start loop that assesses variable [and transformed] distributions 
for (i in c1:cn) {
  pt1 <- powerTransform(data[, i])
  sw0 <- shapiro.test(data[, i])
  sw1 <- shapiro.test(log10(data[, i]))
  sw2 <- shapiro.test((data[, i]) ^ as.vector(pt1$lambda))
  table[i-(c1-1),] <- c(names.of.cols[i], signif(sw0$statistic, 4), 
                        signif(sw0$p.value, 4), signif(sw1$statistic, 4), 
                        signif(sw1$p.value, 4), signif(sw2$statistic, 4),
                        signif(sw2$p.value, 4), signif(as.vector(pt1$lambda), 4))
}
print(table, row.names = FALSE)
# remove temporary objects
rm(list=c("names.of.cols","data","c1","cn","pt1","sw0","sw1","sw2","i","table"))

# The table resulting gives us W and p-values for Shapiro-Wilk tests
# on the variables in afs20 columns 9 to 38 (except cadmium).
# We can use these values to choose whether we can use parametric 
# statistical tests.
# We should ALWAYS use both graphical analysis AND statistical tests.
# The plots suggest normality or not, the tests confirm it.
# The tests can identify non-normality, but not why (e.g bimodality).

# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

# +=+=+=+ Counting values above a threshold +=+=+=+ ####

# We can use untransformed variables for this...
# ...but for plotting we might want to transform at least the axis

## Counting without plotting ####
# use the ISQG-high threshold for Zn = 410 mg/kg
# the function which() returns a vector of the rows meeting a criterion
# try it:
which(afs20$Zn > 410)

# and we can use length() to count the number of values in this vector,
# which is the number of values meeting the criterion,
# in this case how many Zn values > ISQG-high:
length(which(afs20$Zn > 410))

## Visualizing values above a threshold in a plot ####

# simplest way to do this is with a stripchart and abline
# try omitting ', log = "y"' to see what happens
stripchart(afs20$Zn, method = "jitter", vertical = TRUE, log = "y")
abline(h = 410, col = 2, lty = 2)

# also...
hist(log10(afs20$Zn), breaks = "FD")
abline(v = log10(410), col = 4, lty = 3)
text(log10(410), par('usr')[4]*0.8, 
     labels = "ISQG (high)\nZn = 410 mg/kg",
     pos = 4, col = 4)
# NB in above code par('usr')[4]*0.8 is 80% of the y-axis scale,
# and pos = 4 means to the right of the specified location

# and also...
boxplot(log10(afs20$Zn))
abline(h = log10(410), col = 6, lty = 3)
text(1.4, log10(410), 
     labels = "ISQG (high)\nZn 410 mg/kg",
     pos = 3, col = 6)
# NB in above code pos = 3 means above the specified location

## A bit more advanced - testing several columns with a loop
# maybe first make a vector of column names to use in loop?
ISQGs <- names(afs20)[1:38] # elements stop at column 38
print(ISQGs)

# ...but let's be clever and add some data...
ISQGs <- data.frame(variable = names(afs20)[1:38],
                      ISQG_lo = rep(NA, 38),
                    ISQG_hi = rep(NA, 38),
                    n.gt.lo = rep(NA, 38),
                    n.gt.hi = rep(NA, 38)) 
row.names(ISQGs) <- ISQGs$variable

# We have a new data frame, empty except for a column
# of variable names from the data we're analysing
# (other columns will hold data & results)
# Check its contents:
print(ISQGs)

# next add the ISQG trigger concentrations
# for relevant elements
# https://www.waterquality.gov.au/anz-guidelines/guideline-values/default/sediment-quality-toxicants 
ISQGs["As",2:3] <- c(20,70)
ISQGs["Cd",2:3] <- c(1.5,10)
ISQGs["Cr",2:3] <- c(80,370)
ISQGs["Cu",2:3] <- c(65,270)
ISQGs["Pb",2:3] <- c(50,220)
ISQGs["Ni",2:3] <- c(21,52)
ISQGs["Zn",2:3] <- c(200,410)
#
# then make the loop
for (i in c(12,15,18,19,30,32,38)){
  ISQGs[i,4] <- length(which(afs20[,i] > ISQGs[i,2]))
  ISQGs[i,5] <- length(which(afs20[,i] > ISQGs[i,3]))
}
# print relevant rows of the results data frame
print(ISQGs[c(12,15,18,19,30,32,38),2:5])
rm(ISQGs) # tidy up
