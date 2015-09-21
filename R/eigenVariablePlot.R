#' Plot an eigenvector vs a variable
#'
#' The function select an approproate plot to show a relationship between
#' an eigenvector and a variable from the metadata. If the variable is numeric
#' or integer a scatterplot will be produced, if the variable is a factor or
#' a character a boxplot will be produced. The eigenvector will be plotted on
#' the y axis a and the variable from the metadata will be plotted on the x
#' axis. The plot is based on ggplot.
#'
#' @param pcs a matrix with eigen vectors, V matrix of an SVD
#' @param variables a data frame with clinical/technical variables pertaining to the data
#' @param PC number of PC to plot, default is 1 (first)
#' @param var.name name of the variable from \code{variable} data frame
#' @param title plot title
#'
#' @return ggplot object
#'
#'
#' @seealso code{\link{fs}}
#'
#' @export



eigenVariablePlot <- function(pcs, variables, PC = 1, var.name, title = NULL){
  colnames(pcs) <- paste("PC", 1:ncol(pcs), sep = "")
  requested.pc <- paste("PC", PC, sep = "")
  df.plot <- data.frame(pcs[, PC], variables[, var.name])
  colnames(df.plot) <- c(requested.pc, var.name)
  # Scatter plot if a variable is numeric or integer
  if(identical(class(df.plot[, var.name]), "numeric") | identical(class(df.plot[, var.name]), "integer")){
    p <- ggplot(df.plot, aes_string(var.name, requested.pc)) +
      geom_point(shape = 20, size = 3, alpha = 0.6)
  }
  # Boxplot of the variable is a character or a factor
  else if (identical(class(df.plot[, var.name]), "factor") | identical(class(df.plot[, var.name]), "character")){
    if (identical(class(df.plot[, var.name]), "character")){
      df.plot[, var.name] <- as.factor(df.plot[, var.name])
    }
    p <- ggplot(df.plot, aes_string(var.name, requested.pc)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  # Add title to the plot
  if(!is.null(title)){
    p <- p + ggtitle(title)
  }
  return(p)
}
