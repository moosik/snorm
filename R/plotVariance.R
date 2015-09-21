#' Plot percent variance explained
#'
#' Based on ggplot, prettier than the basic graphics
#'
#' @param u list with the SVD components: u, v, d matrices.
#' It assumes that d is already transformed to reflect the percent
#' variance. Use fs function to generate the SVD
#' @param plot.title plot title
#'
#' @return ggplot object
#'
#' @seealso fs
#'
#' @export


plotVariance <- function(u, plot.title = NULL){
  temp <- data.frame(variance = u$d, index = 1:ncol(u$v))
  p <- ggplot2::ggplot(temp, aes(index, variance)) +
    ggplot2::geom_point(shape = 20) +
    ggplot2::scale_y_continuous(limits = c(0,0.1), breaks = seq(0,0.1,0.01))
  if(!is.null(plot.title)){
    p <- p + ggplot2::ggtitle(plot.title)
  }
  return(p)
}
