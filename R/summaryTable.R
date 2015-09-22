#' Summarize each column of a data frame
#'
#' Function takes a data frame and creates a series of summary statistics for each
#' column. The type of statistics returned depends on the class of the column.
#' If the column is numeric or integer,
#' the function provides a minimum,
#' median, mean, maximum and a number of missing cases and a class of the variable.
#' If the class is a factor it provides the number of levels, and the statistics for
#' the number of cases as defined by each level: the minimum,
#' the median, the mean, the maximum and the number of missing cases. If the class
#' is a character it is then converted to a factor and the same
#' statistics are given.
#'
#' @param df a data frame for columns of which the summary statistics are to
#' be calculated
#'
#' @return a data frame with seven columns: type of the variable, number of
#' categories (when a factor or a character), minimum, mean, median, maximum,
#' number of the missing cases. Row names
#' of the data frame are the column names of the input data frame.
#'
#' @export

summaryTable <- function(df){
 return(do.call("rbind", lapply(df, summaryTable)))
}
