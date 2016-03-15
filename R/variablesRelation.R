#' Calculate all pairwise variable relations
#'
#' Given a data frame the function will calculate pairwise correlations
#' between all columns. This function is usually used to understand if any of the
#' technical variables in the data are correlated with the biological variables
#' (to avoid loosing the signal during data normalization).
#' The following tests are used: between a factor and a numeric
#' variable or between two numeric variables - anova F test,
#' between two factor variables - Chi-square test. If simulate.p.val = TRUE then
#' the p-value for the Chi-square test is computed for a Monte Carlo test with
#' monte.carlo.reps replicates. We suggest removing any columns ID columns from the
#' data prior to using this function (definition of an ID column: number of levels
#' is the same as the number of rows in the data frame)
#'
#' @param df data frame for which relationship between columns needs to be calculated
#' @param multiple.testing method to use for multiple testing adjustment, currently
#' supported methods are Benjamini & Hochberg's FDR ("fdr"), Benjamini & Yekutieli ("BY")
#' or Storey's q-value ("q")
#'
#' @return
#'  \describe{
#'    \item{plot}{ggplot object with the tile plot where two colors are used to
#'    highlight significant relationships (alpha is less or equal to 0.05)}
#'    \item{table}{a data frame with all pairwise tests: variable 1 name,
#'    variable 2 name, P value, test statistic, test name, adjusted P value using
#'    one of methods}
#'  }
#'
#' @export



variablesRelation <- function(df, multiple.testing = c("fdr", "BY", "q")){
  method <- match.arg(multiple.testing)
  if(identical(class(df), "matrix")){
    df <- as.data.frame(df)
  }
  if(identical(colnames(df), NULL)){
    colnames(df) <- paste0("col", 1:ncol(df))
  }
  # Make all pairs of columns to be compared:
  var.grid <- expand.grid(seq_len(ncol(df)), seq_len(ncol(df)))
  res <- vector("list", nrow(var.grid))
  for(i in seq_len(nrow(var.grid))){
    res[[i]] <- testDecisionTree(var.grid[i, 1], var.grid[i, 2], df)
  }
  # Prepare the plot
  res.pvalues <- suppressWarnings(as.numeric(unlist(sapply(res, "[", 1))))
  # Adjust using FDR
  res.pvalues.adj <- switch(method,
    fdr = {
      p.adjust(res.pvalues, method = "fdr")
      },
    BY = {
      p.adjust(res.pvalues, method = "BY")
      },
    q = {
      qvalue(res.pvalues)$qvalues
      }
  )
  # Prepare a table with the actual P values, errors and warnings if any tests performed

  res.table <- data.frame(var1 = colnames(df)[var.grid[,1]],
                          var2 = colnames(df)[var.grid[,2]],
                          pvalue = res.pvalues,
                          pvalue.adj = res.pvalues.adj,
                          test.estimate = unlist(sapply(res, "[", 2)),
                          test.type = unlist(sapply(res, "[", 3)))
  p <- ggplot(res.table, aes(var1, var2, fill = cut(pvalue.adj,
                                                    breaks = c(0,0.05,1),
                                                    include.lowest = TRUE))) +
    geom_tile(color = "white") +
    scale_fill_brewer(palette = "Set1", name = "P value\nsignificance", na.value = "grey70") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    xlab("") +
    ylab("") +
    ggtitle("P values for the associations between variables\nadjusted for multiple testing")
  return(list(plot = p, table = res.table))
}
