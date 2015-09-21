#' Plots data outliers based on first eigen vector
#'
#' The function takes the eigen vector provided, orders it's values,
#' takes the minimum and the maximum, finds the corresponding columns
#' in the provided data matrix and creates a scatter plot of these
#' vectors. Adds a line with intercept 0 and the slope 1. This kind of
#' plot is generally produced for the first eigen vector.
#'
#' @param data a matrix for which an SVD was computed
#' @param pc vector corresponding to an eigen vector of the SVD
#'
#' @return a ggplot object
#'
#' @seealso \code{\link{fs}}
#'
#' @export



matrixOutliers <- function(data, pc){
  if(!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  p <- ggplot2::ggplot(data, aes_q(as.name(names(data)[order(pc)[1]]), as.name(names(data)[order(-pc)[1]]))) +
    ggplot2::geom_point(alpha = 0.05) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::ggtitle("Arrays corresponding to the most extreme\npoints of the first eigenvector")
  return(p)
}

