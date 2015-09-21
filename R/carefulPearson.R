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
