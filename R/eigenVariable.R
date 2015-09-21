#' @title Association of eigen vectors with variables
#'
#' @description This function calculates P values for association of any number
#'  of eigen vectors with the metadata variables.
#'  Method: anova(lm(eigen vector ~ variable))$"Pr(>F)"[1]
#'
#'
#' @param pcs a matrix with eigen vectors, i.e V matrix of an SVD
#' @param tech.vars data frame with the metadata variables
#'
#' @return
#'   \describe{
#'    \item{result}{Matrix with the P values where columns are the eigen vectors and
#'    the rows are the variables from the metadata}
#'    \item{plot}{ggplot object for a tile plot reflecting significant associations}
#'  }
#'
#' @examples
#' u <- matrix(runif(120), ncol = 3)
#' var.test <- data.frame(batch = sample(c("a", "b"), 40, replace = TRUE),
#'                        age = sample(50:85, size = 40, replace = TRUE),
#'                        amp.plate = sample(c("A1", "B1", "C1", "D1", "E1"), 40, replace = TRUE),
#'                        height = sample(60:72, 40, replace = TRUE))
#' res <- eigenVariable(u, var.test)
#' print(res$result)
#' plot(res$plot)
#'
#' @export

eigenVariable <- function(pcs, tech.vars){
  result <- as.data.frame(apply(pcs, 2, function(x) anovaDF(x, tech.vars)))
  colnames(result) <- paste("PC", seq_len(ncol(pcs)), sep = "")
  k <- rownames(result)
  result <- transform(result, vars = k)
  result.long <- reshape2::melt(result, id.vars = "vars")
  result.long <- na.omit(result.long)
  result.long <- transform(result.long, pval = cut(value, c(0,0.05,1)))
  p <- ggplot2::ggplot(result.long, aes(variable, vars, fill = pval)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::xlab("eigen vectors") +
    ggplot2::ylab("variables") +
    ggplot2::ggtitle("Relationship between data eigen vectors\nand technical and clinical variables")
  rownames(result) <- NULL
  return(list(result = result, plot = p))
}

