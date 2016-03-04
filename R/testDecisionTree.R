#' Decide what correlation test to perform and obtain statistics
#'
#' Given the names of two variable \code{ind1} and \code{ind2} in a data frame
#' \code{df}
#' decide which test should be performed to calculate the relationship
#' between these variables. The decision is based on their respective classes.
#' Class pairs and the corresponing
#' tests: factor/factor - Chi-square (with or without a Monte-Carlo simulation),
#' numeric/numeric and numeric/factor - anova F test
#'
#'
#' @param ind1 a string to indicate the name of the first variable
#' @param ind2 a string to indicate the name of the second variable
#' @param df a data frame with the variables
#' @param chisq.p.val.sim boolean to indicate whether the simulation should
#' be performed for the Chi-square test. Default is TRUE
#'
#'
#' @return
#'  \describe{
#'    \item{p.value}{P value for the test}
#'    \item{statistic}{Test statistic}
#'    \item{test.type}{Type of the test performed: anova F test or Chi-square test}
#'  }
#'
#'
#'
#' @seealso \code{\link{variablesRelation}}, \code{\link{carefulChisq}}, \code{\link{carefulLM}}



testDecisionTree <- function(ind1, ind2, df){
  if (identical(class(df[,ind1]), "numeric") & identical(class(df[,ind2]), "numeric")){
    # if both columns are numeric do anova F test
    test.res <- carefulLM(ind1, ind2, df)
  }
  else if (identical(class(df[,ind1]), "factor") & identical(class(df[,ind2]), "factor")){
    # if both are factors then do a chi-square test
    test.res <- carefulChisq(ind1, ind2, df)
  }
  else if (identical(class(df[,ind1]), "factor") & identical(class(df[,ind2]), "numeric")){
    # first is a factor, second is numeric do anova F test
    test.res <- carefulLM(ind2, ind1, df)
  }
  else if (identical(class(df[,ind1]), "numeric") & identical(class(df[,ind2]), "factor")){
    # first is numeric, second is a factor do anova F test
    test.res <- carefulLM(ind1, ind2, df)
  }
  else{
    test.res <- list("p.value" = "types of variables are not recognized",
                     "statistic" = "types of variables are not recognized",
                     "test.type" = "no test")
  }
  return(test.res)
}
