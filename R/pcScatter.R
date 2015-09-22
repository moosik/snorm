#' Create a scatter plot between two eigen vectors colored by a variable
#'
#'
#' This function combines eigen vectors with the
#' metadata, creates a scatter plot (ggplot2 graphics) between two eigenvectors and colors
#' the points by a variable present in the metadata. The metadata and the
#' matrix of the eigen vectors are matched by row names.
#'
#' @param eigens a matrix with the eigen vectors
#' @param metadata a data frame with the variable of interest (points color), must have row and column names
#' @param pc.to.plot a numeric vector with two elements indicating which two
#' eigen vectors should be used for a scatter plot
#' @param variable a string indicating a name of the variable in
#' the metadata by which the points of the scatter plot should be colored
#' @param title string for the plot title. Default is NULL - no plot title
#'
#' @return
#'   \describe{
#'    \item{plot}{ggplot2 plot object}
#'    \item{eigen.meta}{data frame of the metadata merged with the matrix of the eigen vectors}
#'  }
#'
#' @seealso \code{\link{fs}}
#'
#' @export


pcScatter <- function(eigens, metadata, pc.to.plot, variable, title = NULL){
  # Merge the matrix of eigenvectors and the metadata by their row names
  eigen.meta <- merge(eigens, metadata, by.x = 0, by.y = 0)
  pc1 <- paste("Eigen", pc.to.plot[1], sep = "")
  pc2 <- paste("Eigen", pc.to.plot[2], sep = "")
  p <- ggplot2::ggplot(eigen.meta,
              aes_string(pc1, pc2, color = variable)) +
    ggplot2::geom_point(pch = 20)
  if(!is.null(title)){
    p <- p + ggplot2::ggtitle(title)
  }
  return(list(plot = p, eigen.meta = eigen.meta))
}
