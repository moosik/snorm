#' Calculate all pairwise variable relation
#'
#' Given a data frame the function will calculate pairwise correlations
#' between all columns. This function is usually used to understand if any of the
#' technical variables in the data are correlated with the biological variables
#' (to avoid loosing the signal during data normalization).
#' The following tests are used: between a factor and a numeric
#' variable - Kruskal Wallis test, between two numeric variables - Pearson correlation,
#' between two factor variables - Chi-square test. If simulate.p.val = TRUE then
#' the p-value for the Chi-square test is computed for a Monte Carlo test with
#' monte.carlo.reps replicates. We suggest removing any columns ID columns from the
#' data prior to using this function (definition of an ID column: number of levels
#' is the same as the number of rows in the data frame)
#'
#' @param df data frame for which relationship between columns needs to be calculated
#' @param simulate.p.val if it is TRUE (default) then the P values for the Chi-square
#' test are computed for a Monte-carlo test with monte.carlo.reps
#' @param monte.carlo.reps number of replicates for the Monte-Carlo test
#'
#' @return
#'  \describe{
#'    \item{"plot"}{"ggplot object with the tile plot where two colors are used to
#'    highlight significant relationships (alpha \leq to 0.05)"}
#'    \item{"table"}{"a data frame with all pairwise tests: variable 1 name,
#'    variable 2 name, P value, test statistic, test name"}
#'  }
#'
#' @export
#'
#' @seealso chisq.test, kruskal.test, cor.test



variablesRelation <- function(df, simulate.p.val = TRUE, monte.carlo.reps = 2000){
  # Remove all rows with missing values
  df <- na.omit(df)
  df <- droplevels(df)
  # Make all pairs of columns to be compared:
  var.grid <- expand.grid(seq_len(ncol(df)), seq_len(ncol(df)))
  res <- vector("list", length(nrow(var.grid)))
  for(i in seq_len(nrow(var.grid))){
    res[[i]] <- testDecisionTree(var.grid[i,1], var.grid[i, 2], df)
  }
  # Prepare the plot
  res.pvalues <- suppressWarnings(as.numeric(unlist(sapply(res, "[", 1))))
  res.pval.df <- data.frame(var1 = colnames(df)[var.grid[,1]],
                            var2 = colnames(df)[var.grid[,2]],
                            pvalue = cut(res.pvalues, breaks = c(-0.00000000001,0.05,1)))
  p <- ggplot(res.pval.df, aes(var1, var2, fill = pvalue)) +
    geom_tile(color = "white") +
    scale_fill_brewer(palette = "Set1", name = "P value\nsignificance",
                      na.value = "grey70", labels = c("[0, 0.05]", "(0.05,1]")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    xlab("") +
    ylab("") +
    ggtitle("Correlation between the variables")
  # Prepare a table with the actual P values, errors and warnings if any tests performed
  res.table <- data.frame(var1 = colnames(df)[var.grid[,1]],
                          var2 = colnames(df)[var.grid[,2]],
                          test.pvalue = unlist(sapply(res, "[", 1)),
                          test.estimate = unlist(sapply(res, "[", 2)),
                          test.type = unlist(sapply(res, "[", 3)))
  return(list(plot = p, table = res.table))
}
