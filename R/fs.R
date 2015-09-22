#' Calculate an SVD on a matrix
#'
#' Use \code{\link[corpcor]{fast.svd}} from \code{corpcor} library to calculate an SVD
#'  of a matrix. The data before the calculation is centered. The D
#'  matrix is transformed to show percent variance explained. The missing or
#'  infinite numbers should be removed or imputed in the matrix prior to the
#'  calculation.
#'
#'  The function is a slight modification of the function created by Brig Mecham.
#'
#'  @references \href{https://github.com/Sage-Bionetworks/mGenomics/blob/master/R/fs.R}{mGenomics}
#'
#'  @import corpcor
#'
#'  @param x data matrix
#'
#'  @return
#'  \describe{
#'    \item{u}{a matrix with the corresponding left singular vectors. Column
#'    names of the matrix are the column names of the input data matrix \code{x}.}
#'    \item{v}{a matrix with the corresponding right singular vectors. We refer
#'    to them as eigen vectors in the documentation for all other functions. The matrix
#'    has column names in the following format "EigenN", where N is the number of the
#'    corresponding vector. The row names are the same as the column names of the
#'    input data matrix \code{x}.}
#'    \item{d}{a vector containing the values for the variance explained by each
#'    singular vector}
#'
#'  }
#'
#'  @export
#'
#'  @seealso \code{\link[corpcor]{fast.svd}}


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


