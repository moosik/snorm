#' Summarize each column of a data frame
#'
#' Function takes a data frame and creates a series of summary statistics for each
#' column of the data frame based on its class. If a column is numeric or integer,
#' the function provides the minimum,
#' median, mean, maximum, the number of the missing values and the class.
#' If the class is factor it provides the number of levels, and the statistics for
#' the number of cases as defined by each level: the minimum, the maximum,
#' the median, the mean, the maximum and the number of missing cases. If the class
#' is character then it is converted to factor and the statistics and the same
#' statistics are given.
#'
#' @return a data frame with seven columns: type of the variable, number of
#' categories, minimum, mean, median, maximum, number of missing values. Row names
#' of the data frame are the column names of the input data frame
#'
#' @export

summaryTable <- function(df){
 return(do.call("rbind", lapply(df, summaryTable)))
}
