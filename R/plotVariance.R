#' Plot percent variance explained
#'
#' Based on ggplot2 graphics library, prettier than the basic graphics
#'
#' @param d a vector containing the values for the variance explained by each
#'    singular vector
#' @param plot.title plot title
#'
#' @return ggplot object
#'
#' @seealso \code{\link{fs}}
#'
#' @export


plotVariance <- function(d, plot.title = NULL){
  temp <- data.frame(variance = d, index = seq_len(length(d)))
  p <- ggplot2::ggplot(temp, aes(index, variance)) +
    ggplot2::geom_point(shape = 20) +
    ggplot2::scale_y_continuous(limits = c(0,0.1), breaks = seq(0,0.1,0.01))
  if(!is.null(plot.title)){
    p <- p + ggplot2::ggtitle(plot.title)
  }
  return(p)
}
