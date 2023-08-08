multi_SW_test <- function(data, 
                          key = TRUE){
  require(car)
  cn <- NCOL(data)
  names.of.cols <- colnames(testdata)
  transf_results <- data.frame("Variable"=1:cn, 
                             "W_orig"=1:cn,"p_orig"=1:cn,
                             "W_log_tr"=1:cn, "p_log_tr"=1:cn,
                             "W_pow_tr"=1:cn,"p_pow_tr"=1:cn, 
                             "Pow_term"=1:cn,"RoundPT"=1:cn)
for (i in 1:cn) {
  # generate and print test statistics
  sw0 <- shapiro.test(data[, i])
  sw1 <- shapiro.test(log10(data[, i]))
  pt1 <- powerTransform(data[, i])
  sw2 <- shapiro.test((data[, i]) ^ as.vector(pt1$lambda))
  transf_results[i,] <- c(names.of.cols[i], 
                          signif(sw0$statistic, 4),
                          signif(sw0$p.value, 4),
                          signif(sw1$statistic, 4),
                          signif(sw1$p.value, 4),
                          signif(sw2$statistic, 4),
                          signif(sw2$p.value, 4), 
                          signif(as.vector(pt1$lambda), 4), 
                          signif(as.vector(pt1$roundlam), 4))
  }
  if(key == TRUE) cat("Variable  = variable name\nW_orig    = Test statistic, raw data\np_orig    = p-value statistic, raw data (low p-value = not normally distributed)\nW_log_tr  = Test statistic with log transformed data\np_log_tr  = p-value with log transformed data: note how often a log transformation works\nW_pow_tr  = Test statistic with the optimal power transformation\np_pow_tr  = p-value with the power transformed data\nPow_term  = the actual power term: note often close to zero = log will work well\nRoundPT   = the acceptable rounded power term\n\n")
  return(transf_results)
}
