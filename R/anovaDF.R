#' @title Calculate p value for a linear regression
#' 
#' @description The function calculates P value for F test
#'  statistic the intercept in a model is not 
#'  zero. 
#'  
#'  @param vec vector of the response variable
#'  @param df data frame of the dependent variables
#'  
#'  @return a vector of P values for each model fitted where
#'   input vector is the response variable and dependent 
#'   variable is on the of the columns in the provided
#'   data frame
#'  
#'  @seealso code{\link{eigenVariable}}
#'  
#'  @export


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
