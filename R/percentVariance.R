#' Plot percent variance explained 
#' 
#' Based on ggplot, prettier than the basic graphics
#' 
#' @param u list with the SVD components: u, v, d matrices.
#' It assumes that d is already transformed to reflect the percent
#' variance. Use fs function to generate the SVD
#' @param plot.title plot title
#' 
#' @seealso fs


plotVariance <- function(u, plot.title = NULL){
  temp <- data.frame(variance = u$d, index = 1:ncol(u$v))
  if(!is.null(plot.title)){
    p <- ggplot(temp, aes(index, variance)) +
      geom_point(shape = 20) +
      scale_y_continuous(limits = c(0,0.1), breaks = seq(0,0.1,0.01)) +
      ggtitle(plot.title)
  }
  else{
    p <- ggplot(temp, aes(index, variance)) +
      geom_point(shape = 20) +
      scale_y_continuous(limits = c(0,0.1), breaks = seq(0,0.1,0.01))
  }
  return(p)
}