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
