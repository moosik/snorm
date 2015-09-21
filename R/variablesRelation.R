#'
#'
#'
#' remove bad columns! (generally id columns)
#' make a more intelligent table result: no repeated tests, fewer significant digits
#' Possibly separate results from errors (easier to sort?)
#'



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
  #res <- apply(var.grid, 1, function(x) testDecisionTree(x, df))
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
  # Prepare a table with the actual P values, errors and warnings if any and the tests performed
  res.table <- data.frame(var1 = colnames(df)[var.grid[,1]],
                          var2 = colnames(df)[var.grid[,2]],
                          test.pvalue = unlist(sapply(res, "[", 1)),
                          test.estimate = unlist(sapply(res, "[", 2)),
                          test.type = unlist(sapply(res, "[", 3)))
  return(list(plot = p, table = res.table))
}
