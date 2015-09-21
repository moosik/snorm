#' this function automatically makes a ggplot to
#' show correlation between a principal component
#' and a variable
#' Provide number for the PC, provide the name of the variable
#' Result is a plot
#' require ggplot2



varsInfluence <- function(pcs, variables, PC = 1, var.name){
  colnames(pcs) <- paste("PC", 1:ncol(pcs), sep = "")
  requested.pc <- paste("PC", PC, sep = "")
  df.plot <- data.frame(pcs[, PC], variables[, var.name])
  colnames(df.plot) <- c(requested.pc, var.name)
  if(identical(class(df.plot[, var.name]), "numeric") | identical(class(df.plot[, var.name]), "integer")){
    p <- ggplot(df.plot, aes_string(var.name, requested.pc)) +
      geom_point(shape = 20, size = 3, alpha = 0.6)
  }
  else if (identical(class(df.plot[, var.name]), "factor") | identical(class(df.plot[, var.name]), "character")){
    if (identical(class(df.plot[, var.name]), "character")){
      df.plot[, var.name] <- as.factor(df.plot[, var.name])
    }
    p <- ggplot(df.plot, aes_string(var.name, requested.pc)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  return(p)
}
