#' Pearson correlation test between two numeric variables, catch errors and warnings
#'
#' Given names of two numeric variables \code{x1} and \code{x2} in a data frame \code{df}
#' calculate the P value for the Pearson's product moment correlation coefficient.
#' Use \code{tryCatch} to collect warnings and errors.
#'
#' @param x1 a character string indicating the name of a numeric variable in a data frame \code{df}
#' @param x2 a character string indicating the name of another numeric variable in a data frame \code{df}
#' @param df data frame
#' @param cor.tes.method a character string indicating which correlation coefficient
#' is to be used for the test. Default is "pearson"
#'
#' @return
#'  \describe{
#'    \item{p.value}{P value or the error/warning message}
#'    \item{statistic}{Test statistic or the error/warning message}
#'    \item{test.type}{String indicating the test type: pearson correlation}
#'  }
#'
#' @seealso \code{\link[stats]{cor.test}}



carefulPearson <- function(x1, x2, df, cor.test.method = "pearson"){
  if(identical(x1, x2)){
    return(list(p.value = "variable 1 = variable 2",
                statistic = "variable 1 = variable 2",
                test.type = paste(cor.test.method, "correlation", sep = " ")))
  }
  else{
    test.res <- tryCatch(cor.test(df[, x1], df[, x2], method = cor.test.method),
                         error = function(e) return(e),
                         warning = function(w) return(w))
    if(inherits(test.res, "simpleError") | inherits(test.res, "simpleWarning")){
      return(list(p.value = test.res$message,
                  statistic = test.res$message,
                  test.type = paste(cor.test.method, "correlation", sep = " ")))
    }
    else{
      return(list(p.value = test.res$p.value,
                  statistic = test.res$statistic,
                  test.type = paste(cor.test.method, "correlation", sep = " ")))
    }
  }
}
