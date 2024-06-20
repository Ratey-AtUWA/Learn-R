#' @title Tabulate output from pairwise.adonis2 function for pairwise PERMANOVA
#'
#' @description Generate a data frame with the upper triangle populated with 
#' p-values from PERMANOVA pairwise comparisons. Optionally replace p-values 
#' greater than alpha with the string "ns".
#'
#' @param x Output of a call to `pairwise.adonis2()` written by Pedro Martinez Arbizu
#' at https://github.com/Ratey-AtUWA/eDNA/raw/master/FUN_pairwise_adonis2.R
#' (must be specified; no default).
#'
#' @param ns.repl A logical value which if TRUE (the default) will convert p-values
#' less than `alpha` to the string "ns".
#'
#' @param alpha The numeric value of the p-value threshold for (non)acceptance of
#' the alternate hypothesis for each pairwise comparison.
#'
#' @return A data frame having n-1 columns and n-1 rows (where n is the number of levels 
#' in the factor from the preceding call to `pairwise.adonis2()`), representing the matrix 
#' of pairwise p-values.
#'
#' @export

plainPW2 <- function(x, ns.repl=TRUE, alpha=0.05){
  tablout <- data.frame(Pair=rep(NA, length(x)-1),
                        P_value=rep(NA, length(x)-1))
  for(i in 2:length(x)){
    tablout[i-1,] <- c(names(x)[i],
                       as.data.frame(x[names(x)[i]])[1,5])
    }
  n0 <- (ceiling(sqrt(length(x)*2)))-1
  ptable <- as.data.frame(matrix(rep(NA, n0^2), ncol = n0))
  colnames(ptable) <- c(1:n0)+1
  r0 <- 1 ; rn <- n0
  for(i in 1:n0){
    ptable[i,] <- c(rep(NA,n0-((rn-r0)+1)),as.numeric(c(tablout[r0:rn,2])))
    r0 <- rn+1
    rn <- rn+(n0-i)
    }
  if(ns.repl==TRUE){
    for(i in 1:ncol(ptable)){
      ptable[which(ptable[, i] > alpha), i] <- "ns"
      }
    }
  return(ptable)
}
