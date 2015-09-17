#' Calculate fast.svd on a matrix
#' 
#' 
#' Use fast.svd from corpcor library to calculate the SVD composition 
#'  of a matrix. The data before the calculation is centered.
#'  
#'  The function is based on Brig Mecham's code.
#'  
#'  @reference \url{https://github.com/Sage-Bionetworks/mGenomics/blob/master/R/fs.R}
#'  
#'  @import corpcor
#'  
#'  @param x matrix
#'  
#'  @return a list i
#'  
#'  
#'  @export


fs <-
  function (x) 
  {
    if(!identical(class(x), "matrix")){
      stop("X must be a matrix")
    }
    else{
      u <- fast.svd(t(scale(t(x), scale = FALSE)), tol = 0)
      u$d <- u$d^2/sum(u$d^2)
      # Add names to the u and v matrices
      colnames(u$v) <- paste("Eigen", 1:ncol(x), sep = "")
      rownames(u$v) <- colnames(x)
      colnames(u$u) <- colnames(x)
      return(u)
    }
  }


