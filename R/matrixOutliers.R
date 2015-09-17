#' Plots data outliers based on first eigenvector
#' 
#' @param data dataset for which an SVD was computed
#' @param pc vector corresponding to the eigenvector of the SVD
#' 
#' @return a ggplot object
#' 
#' require ggplot
#' 
#' @seealso fs


matrixOutliers <- function(data, pc){
  if(!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  p <- ggplot(data, aes_q(as.name(names(data)[order(pc)[1]]), as.name(names(data)[order(-pc)[1]]))) +
    geom_point(alpha = 0.05) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    ggtitle("Arrays corresponding to the most extreme\npoints of the first eigenvector")
  return(p)
}

