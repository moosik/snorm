#' Plot a scatter plot between two eigenvectors colored by a variable
#' 
#' 
#' This function combines eigenvectors with the
#' metadata and creates a scatter plot between 2 eigenvectors and colors
#' them by the third variable present in the metadata. Matching of the
#' metadata and the PCs is also executed
#' 
#' @param eigens a matrix with eigenvectors
#' @param metadata a data frame with the variable, row names and column names
#' must be present
#' @param pc.to.plot a vector with two elements indicating for which two eigenvector
#' the plot should be created
#' @param variable by which the points of the scatter plot should be colored
#' 
#' @return plot and the merged pc/metadata data frame
#' @seealso fs


pcScatter <- function(eigens, metadata, pc.to.plot, variable, title = NULL){
  # Merge the matrix of eigenvectors and the metadata by their row names
  eigen.meta <- merge(eigens, metadata, by.x = 0, by.y = 0)
  pc1 <- paste("Eigen", pc.to.plot[1], sep = "")
  pc2 <- paste("Eigen", pc.to.plot[2], sep = "")
  p <- ggplot(eigen.meta, 
              aes_string(pc1, pc2, color = variable)) +
    geom_point(pch = 20)
  if(!is.null(title)){
    p <- p + ggtitle(title)
  }
  return(list(plot = p, eigen.meta = eigen.meta))
}
