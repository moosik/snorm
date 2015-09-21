#' ANOVA overall P value for a linear model
#'
#' The function calculates ANOVA overall P value for a linear
#' model where y is a numeric vector (an eigen vector
#' calculated in an SVD) and x is a dependent variable from
#' the metadata. The function loops over the columns of the
#' metadata
#'
#'  @param vec vector of the response variable (an eigen vector)
#'  @param df data frame with the metadata
#'
#'  @return a vector of P values for each model fitted where
#'   an independent variable is an eigen vector and the
#'   dependent variable is one of the variables from the metdata
#'   data frame
#'




anovaDF <- function(vec, df){
  if(!identical(length(vec), nrow(df))){
    stop("Vector length should be the same as the number of rows in a data frame")
  }
  else{
    return(sapply(df, function(x) tryCatch({anova(lm(vec ~ x))$"Pr(>F)"[1]},
                                           error = function(e) {return(NA)},
                                           warning = function(w) {return(NA)})))
  }
}
