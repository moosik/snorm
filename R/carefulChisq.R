#' Calculate Chi-square test between two variables, catch errors and warnings
#'
#' Given names of two variables \code{x1} and \code{x2} in a data frame \code{df}
#' calculate the P value for the Chi-square test between them. Use \code{tryCatch}
#' to collect warnings and errors. Monte-Carlo simulation is performed by default
#' for more accurate P values.
#'
#' @param x1 name of one factor variable in a data frame \code{df}
#' @param x2 name of another factor variable in a data frame \code{df}
#' @param df data frame
#' @param sim.p.value if it TRUE (default) P value is computed for a Monte carlo test
#'
#' @return
#'  \describe{
#'    \item{p.value}{P value or the error/warning message}
#'    \item{statistic}{Test statistic or the error/warning message}
#'    \item{test.type}{String indicating the test type: chi-square test,
#'    monte carlo simulation}
#'  }
#'
#' @seealso \code{\link[stats]{chisq.test}}


carefulChisq <- function(x1, x2, df, sim.p.value = TRUE){
  if(identical(x1, x2)){
    return(list(p.value = "variable 1 = variable 2",
                statistic = "variable 1 = variable 2",
                test.type = "chi-square test, monte carlo simulation"))
  }
  else{
    test.res <- tryCatch(chisq.test(df[, x1], df[, x2], simulate.p.value = sim.p.value),
                         error = function(e) return(e),
                         warning = function(w) return(w))
    if(inherits(test.res, "simpleError") | inherits(test.res, "simpleWarning")){
      return(list(p.value = test.res$message,
                  statistic = test.res$message,
                  test.type = "chi-square test, monte carlo simulation"))
    }
    else{
      return(list(p.value = test.res$p.value,
                  statistic = test.res$statistic,
                  test.type = "chi-square test, monte carlo simulation"))
    }
  }
}
