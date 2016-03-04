#' Calculate P value for F test from the anova table, catch errors and warnings
#'
#' Given names of a numeric \code{x1} and a factor \code{x2} variables in a data
#' frame \code{df} calculate the P value for the F test from the anova table
#' applied to linear regression.
#' Use \code{tryCatch} to collect warnings and errors.
#'
#' @param resp.vec.ind index of the numeric response variable in a data frame \code{df}
#' @param exp.vec.ind index of the explanatory variable in a data frame \code{df}
#' @param df data frame
#'
#' @return
#'  \describe{
#'    \item{p.value}{P value or the error/warning message}
#'    \item{statistic}{Test statistic or the error/warning message}
#'    \item{test.type}{String indicating the test type: anova F test}
#'  }
#'
#' @seealso \code{\link[stats]{anova}},

carefulLM <- function(resp.vec.ind, exp.vec.ind, df){
  test.res <- tryCatch(anova(lm(df[,resp.vec.ind] ~ df[, exp.vec.ind], na.action = na.omit)),
                       error = function(e) return(e),
                       warning = function(w) return(w))
  if(inherits(test.res, "simpleError") | inherits(test.res, "simpleWarning")){
    return(list(p.value = NA,
                statistic = NA,
                test.type = test.res$message))
  }
  else{
    return(list(p.value = test.res$"Pr(>F)"[1],
                statistic = test.res$"F value"[1],
                test.type = "anova F test"))
  }
}
