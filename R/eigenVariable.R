#' @title Association of eigen genes with variables
#' 
#' @description This function calculates P values for association of any number
#'  of eigen vectors with any number of technical and biological variables. 
#'  Method: anova(lm(eigengene ~ variable))$"Pr(>F)"[1]
#'
#' @author Vitalina Komashko \email{vitalina@@gmail.com}
#' 
#' @import ggplot2
#' @import reshape2
#' 
#' @param pcs a matrix with eigen vectors, i.e u$v[, 1:n]
#' @param tech.vars a data frame with technical and biological variables
#' 
#' @return list, where first element is the matrix with the P values, and
#'  the second element is a ggplot object showing P values
#'  
#' @examples
#' u <- matrix(runif(120), ncol = 3)
#' var.test <- data.frame(batch = sample(c("a", "b"), 40, replace = TRUE), 
#'                        age = sample(50:85, size = 40, replace = TRUE),
#'                        amp.plate = sample(c("A1", "B1", "C1", "D1", "E1"), 40, replace = TRUE),
#'                        height = sample(60:72, 40, replace = TRUE))
#' res <- eigenVariable(u, var.test)
#' print(res$result)
#' plot(res$p)
#' 
#' @seealso code{\link{anovaDF}}
#' @export

eigenVariable <- function(pcs, tech.vars){
  result <- as.data.frame(apply(pcs, 2, function(x) anovaDF(x, tech.vars)))
  colnames(result) <- paste("PC", seq_len(ncol(pcs)), sep = "")
  k <- rownames(result)
  result <- transform(result, vars = k)
  result.long <- melt(result, id.vars = "vars")
  result.long <- na.omit(result.long)
  result.long <- transform(result.long, pval = cut(value, c(0,0.05,1)))
  p <- ggplot(result.long, aes(variable, vars, fill = pval)) + 
    geom_tile(color = "white") + 
    xlab("eigen vectors") +
    ylab("variables") +
    ggtitle("Relationship between data eigen vectors\nand technical and clinical variables")
  rownames(result) <- NULL
  return(list(result = result, plot = p))
}

