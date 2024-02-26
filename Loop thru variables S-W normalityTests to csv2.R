# script authored by andrew rate (C) the university of western australia 2016
# ratey.at.uwa@gmail.com
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
export results to a csv file for Excel (if desired)
write.csv(transf_results, file = "transformations.csv", row.names = FALSE)
# remove temporary objects
# to keep R workspace tidy
rm(list=c("names.of.cols","pt1","sw0","sw1","sw2","i"))
# end code