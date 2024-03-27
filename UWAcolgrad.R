#' @title Gradational palettes based on UWA colours
#'
#' @description Generate a vector of n hex coded colours in one of two versions;
#' a uniform gradient version (default) and an original more saturated version 
#' which is closer to the UWA standard colours.
#'
#' @param np An integer specifying the number of colours to be generated  
#'   (default = 10).
#' @param alpha A value between 0 and 1 specifying the transparency applied to 
#'   the colours (0 = fully transparent, 1, = fully opaque, default = 1).
#' @param saturated A logical value (default is FALSE) which specifies whether 
#'   the UWA standard saturated palette will be generated.
#' @param verbose A logical value (default is FALSE) which specifies whether 
#'   to output a message about the colour character string vector created.
#'
#' @return A vector of np colors (as hexadecimal strings).
#' @export
#'
#' @examples
#' UWAcolgrad(16)
#' palette(c("black", UWAcolgrad(9))) # yields a 10 colour palette
#'
UWAcolgrad <- function(np=10, alpha=1, saturated=FALSE, verbose=FALSE){
  require(R.utils)
  unsat <- c("#00091A","#31517d","#ac8e4b","#E6BF68","#ffffff")
  sat <- c("#000b1f","#103f8f","#DAAA00","#E4BB5E","#ffffff")
  if(saturated==FALSE){
    unsat <- paste0(unsat,intToHex(alpha*255))
    UWAgrad <- colorRampPalette(unsat, alpha=TRUE)(np)
    if(verbose==TRUE){
      cat("You have made a UWA R palette with",np,"uniform-gradient colours...\n")
    }
    return(UWAgrad)
  } else {
    sat <- paste0(sat,intToHex(alpha*255))
    UWAgrad <- colorRampPalette(sat, alpha=TRUE)(np)
    if(verbose==TRUE){
      cat("You have made a UWA R palette with",np,"standard colours...\n")
    }
    return(UWAgrad)
  }
}
