#' Single variable summary for the metadata
#'
#'
#' Function takes a variable and creates a series of summary statistics based
#' on its class. If the vector is numeric or integer, the function provides the minimum,
#' median, mean, maximum, number of missing values and the type of the variable.
#' If the class is factor it provides the number of levels, and the statistics for
#' the number of cases as defined by each level: the minimum, the maximum,
#' the median, the mean, the maximum and the number of missing cases. If the class
#' is character then it is converted to factor and the statistics and the same
#' statistics are given.
#'
#' @param data.var a vector of integer, numeric, character of factor class
#'
#' @seealso summaryTable
#'
#' @return a named vector with seven elements: type of the variable, number of
#' categories, minimum, mean, median, maximum, number of missing values


variableSummary <- function(data.var){
  # For the numeric and integer class variables calculate the stats
  if (identical(class(data.var), "numeric") | identical(class(data.var), "integer")){
    # Get the class of the variable
    var.class <- class(data.var)
    # Minimum
    var.min <- round(min(data.var, na.rm = TRUE), 2)
    # Mean
    var.mean <- round(mean(data.var, na.rm = TRUE), 2)
    # Median
    var.median <- round(median(data.var, na.rm = TRUE), 2)
    # Maximum
    var.max <- round(max(data.var, na.rm = TRUE), 2)
    # How many missing values
    var.na <- length(which(is.na(data.var)))
    # instead of the number of levels we will have NA in the resulting column
    var.levels <- NA
    res <- c(var.class, var.levels, var.min, var.mean, var.median, var.max, var.na)
    names(res) <- c("variable.type", "categories", "min", "mean", "median", "max", "missing.values")
    return(res)
  }
  # if the variable is a factor or a character (convert character to a factor)
  else if (identical(class(data.var), "factor") | identical(class(data.var), "character")){
    if (identical(class(data.var), "character")) {
      # get the class
      var.class <- class(data.var)
      # convert character to a factor
      data.var <- as.factor(data.var)
    }
    else{
      var.class <- class(data.var)
    }
    # Calculate the statistics:
    # How many levels we have
    var.levels <- length(levels(data.var))
    # How many missing values a vector has
    var.na <- length(which(is.na(data.var)))
    # Get the number of cases as defined by each level and all further
    # statistics calculate using this vector
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
