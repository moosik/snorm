#' summary table for the metadata: clinical and technical information
#' if numeric: min, median, mean, max, # of NAs, type
#' if factor: number of levels, then min-median-mean-max for the distribution of the cases
#' in each level, # of NAs
#' If character, then it is converted to a factor and the same thing is performed
#' 
#' 
summaryTable <- function(data.var){
  if (identical(class(data.var), "numeric") | identical(class(data.var), "integer")){
    var.class <- class(data.var)
    var.min <- round(min(data.var, na.rm = TRUE), 2)
    var.mean <- round(mean(data.var, na.rm = TRUE), 2)
    var.median <- round(median(data.var, na.rm = TRUE), 2)
    var.max <- round(max(data.var, na.rm = TRUE), 2)
    var.na <- length(which(is.na(data.var)))
    var.levels <- NA
    res <- c(var.class, var.levels, var.min, var.mean, var.median, var.max, var.na)
    names(res) <- c("variable.type", "categories", "min", "mean", "median", "max", "missing.values")
    return(res)
  }
  else if (identical(class(data.var), "factor") | identical(class(data.var), "character")){
    if (identical(class(data.var), "character")) {
      var.class <- class(data.var)
      data.var <- as.factor(data.var)
    }
    else{
      var.class <- class(data.var)
    }
    var.levels <- length(levels(data.var))
    var.na <- length(which(is.na(data.var)))
    data.dist <- as.numeric(table(data.var))
    var.min <- round(min(data.dist, na.rm = TRUE), 2)
    var.mean <- round(mean(data.dist, na.rm = TRUE), 2)
    var.median <- round(median(data.dist, na.rm = TRUE), 2)
    var.max <- round(max(data.dist, na.rm = TRUE), 2)
    res <- c(var.class, var.levels, var.min, var.mean, var.median, var.max, var.na)
    names(res) <- c("variable.type", "categories", "min", "mean", "median", "max", "missing.values")
    return(res)
  }
}