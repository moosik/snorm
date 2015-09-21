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
