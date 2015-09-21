#' Calculate an SVD on a matrix
#'
#' Use fast.svd from corpcor library to calculate an SVD
#'  of a matrix. The data before the calculation is centered. The D
#'  matrix is transformed to show percent variance explained.
#'
#'  The function is a slight modification of the function created by Brig Mecham.
#'
#'  @reference \url{https://github.com/Sage-Bionetworks/mGenomics/blob/master/R/fs.R}
#'
#'  @import corpcor
#'
#'  @param x data matrix
#'
#'  @return a list with U, V and D matrices. Column names of V get names: EigenN,
#'  where N is the number of the eigenvector. Row names of V get the column names
#'  of the input data matrix. Column names of U get the column names of the input
#'  data matrix.
#'
#'  @export


fs <- function(x)
  {
    if(!identical(class(x), "matrix")){
      stop("X must be a matrix")
    }
    else{
      u <- corpcor::fast.svd(t(scale(t(x), scale = FALSE)), tol = 0)
      # Transform D to percent variance
      u$d <- u$d^2/sum(u$d^2)
      # Add names to the U and the V matrices
      colnames(u$v) <- paste("Eigen", 1:ncol(x), sep = "")
      rownames(u$v) <- colnames(x)
      colnames(u$u) <- colnames(x)
      return(u)
    }
  }


