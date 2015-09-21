#' Calculate Kruskal-Wallis test between two variable, catch errors and warning
#'
#' Given names of a numeric variable \code{x1} and \code{x2} in a data frame \code{df}
#' calculate the P value for the Chi-Square test between them. Use \code{tryCatch}
#' to collect warnings and errors. Monte-Carlo simulation is performed by default
#' for more accurate P values.
#'
#' @param num.vec.ind name of a numeric variable in a data frame \code{df}
#' @param group.vec.ind name of a factor variable in a data frame \code{df}
#' @param df data frame
#'
#' @return
#'  \describe{
#'    \item{"p.value"}{"P value or the error/warning message"}
#'    \item{"statistic"}{"Test statistic or the error/warning message"}
#'    \item{"test.type"}{"String indicating the test type: kruskal-wallis rank sum test"}
#'  }
#'
#' @seealso \code{\link[stats]{kruskal.test}}

carefulKruskal <- function(num.vec.ind, group.vec.ind, df){
  test.res <- tryCatch(kruskal.test(df[, num.vec.ind] ~ df[, group.vec.ind]),
                       error = function(e) return(e),
                       warning = function(w) return(w))
  if(inherits(test.res, "simpleError") | inherits(test.res, "simpleWarning")){
    return(list(p.value = test.res$message,
                statistic = test.res$message,
                test.type = "kruskal-wallis rank sum test"))
  }
  else{
    return(list(p.value = test.res$p.value,
                statistic = test.res$statistic,
                test.type = "kruskal-wallis rank sum test"))
  }
}
