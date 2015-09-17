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
                  test.type = "chi-square test, monte carlo simulatio"))
    }
    else{
      return(list(p.value = test.res$p.value, 
                  statistic = test.res$statistic, 
                  test.type = "chi-square test, monte carlo simulation"))
    }   
  }
}



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



carefulPearson <- function(x1, x2, df, cor.test.method = "pearson"){
  if(identical(x1, x2)){
    return(list(p.value = "variable 1 = variable 2", 
                statistic = "variable 1 = variable 2", 
                test.type = paste(cor.test.method, "correlation", sep = " ")))
  }
  else{
    test.res <- tryCatch(cor.test(df[, x1], df[, x2], method = cor.test.method),
             error = function(e) return(e),
             warning = function(w) return(w))
    if(inherits(test.res, "simpleError") | inherits(test.res, "simpleWarning")){
      return(list(p.value = test.res$message, 
                  statistic = test.res$message, 
                  test.type = paste(cor.test.method, "correlation", sep = " ")))
    }
    else{
      return(list(p.value = test.res$p.value, 
                  statistic = test.res$statistic, 
                  test.type = paste(cor.test.method, "correlation", sep = " ")))
    }
  }
}