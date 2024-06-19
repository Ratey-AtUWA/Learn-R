# function inputs output object from Pedro Martinez Arbizu's pairwise.adonis2()
# code at https://github.com/pmartinezarbizu/pairwiseAdonis
plainPW2 <- function(PWobj){
  tablout <- data.frame(Pair=rep(NA, length(PWobj)-1),
                        P_value=rep(NA, length(PWobj)-1))
  for(i in 2:length(PWobj)){
    tablout[i-1,] <- c(names(PWobj)[i],
                       as.data.frame(PWobj[names(PWobj)[i]])[1,5])
    }
  n0 <- (ceiling(sqrt(length(PWobj)*2)))-1
  ptable <- as.data.frame(matrix(rep(NA, n0^2), ncol = n0))
  colnames(ptable) <- c(1:n0)+1
  r0 <- 1 ; rn <- n0
  for(i in 1:n0){
    ptable[i,] <- c(rep(NA,n0-((rn-r0)+1)),as.numeric(c(tablout[r0:rn,2])))
    r0 <- rn+1
    rn <- rn+(n0-i)
    }
  return(ptable)
}
