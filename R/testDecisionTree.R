#' Decide what correlation test to perform and obtain statistics
#'
#' Given the names of two variable \code{ind1} and \code{ind2} in a data frame
#' \code{df}
#' decide which test should be performed to calculate the relationship
#' between the variables based on their classes. Class pairs and the corresponing
#' tests: factor/factor - Chi-square (with or without a Monte-Carlo simulation).
#' numeric/numeric - Pearson correlation, numeric/factor - Kruskal-Wallis test.
#'
#' @return
#'  \describe{
#'    \item{"p.value"}{"P value for the test"}
#'    \item{"statistic"}{"Test statistic"}
#'    \item{"test.type"}{"Type of the test performed: Chi-Square, Pearson or Kruskal-Wallis"}
#'  }
#'
#' @seealso \code{\link{variablesRelation}}, \code{\link{carefulChisq}}, \code{\link{carefulKruskal}},
#' \code{\link{carefulPearson}}



testDecisionTree <- function(ind1, ind2, df, chisq.p.val.sim = TRUE, cor.method  = "pearson"){
  if (identical(class(df[,ind1]), "numeric") & identical(class(df[,ind2]), "numeric")){
    # if both columns are numeric do the correlation test
    test.res <- carefulPearson(ind1, ind2, df, cor.test.method = cor.method)
  }
  else if (identical(class(df[,ind1]), "factor") & identical(class(df[,ind2]), "factor")){
    # if both are factors then do a chi-square test
    test.res <- carefulChisq(ind1, ind2, df, sim.p.value = chisq.p.val.sim)
  }
  else if (identical(class(df[,ind1]), "factor") & identical(class(df[,ind2]), "numeric")){
    # first is a factor, second is numeric
    test.res <- carefulKruskal(ind2, ind1, df)
  }
  else if (identical(class(df[,ind1]), "numeric") & identical(class(df[,ind2]), "factor")){
    # first is numeric, second is a factor
    test.res <- carefulKruskal(ind1, ind2, df)
  }
  else{
    test.res <- list("p.value" = "types of variables are not recognized",
                     "statistic" = "types of variables are not recognized",
                     "test.type" = "no test")
  }
  return(test.res)
}