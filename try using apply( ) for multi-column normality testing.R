require(car)

names.of.cols <- names(sv2017)

c1 <- 9
cn <- 36

# explain first line of code
S_W_raw <- matrix(as.numeric(unlist(apply(sv2017[,9:36],2,shapiro.test))),ncol=4,byrow = T)[,1:2]
# row.names(S_W_raw) <- names(sv2017[,c1:cn])
# S_W_raw
# transf_results
S_W_log <- matrix(as.numeric(unlist(apply(log10(sv2017[,9:36]),2,shapiro.test))),ncol=4,byrow = T)[,1:2]
# row.names(S_W_log) <- names(sv2017[,c1:cn])
# S_W_log

powTerm <- rep(NA, length(c1:cn)); S_W_pow <- cbind(powTerm,powTerm) ; j <- 0
for (i in c1:cn) {
  j <- j+1
  powTerm[j] <- powerTransform(sv2017[, i])$lambda
  S_W_pow[j,] <- as.numeric(shapiro.test(sv2017[,i]^powTerm[j])[1:2])
}
😎☕🍫
transf_results <- data.frame(Variable = names(sv2017[,c1:cn]),
                             W_orig = S_W_raw[,1], p_orig = S_W_raw[,2], 
                             W_log_tr = S_W_log[,1], p_log_tr = S_W_log[,2], 
                             W_pow_tr = S_W_pow[,1], p_pow_tr = S_W_pow[,2], 
                             Pow_term = powTerm)
