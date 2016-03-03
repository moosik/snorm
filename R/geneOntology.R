#' Obtain Gene Ontology (GO) results for a gene list
#'
#'
#' This function performs Gene Ontology analysis on a list of gene. It obtains
#' MF, BP and CC categories for a gene list, calculates enrichment P value and
#' multiple testing correction based on Bonferroni method. Finally for each MF,
#' BP and CC it selects only the categories for which corrected P value is less
#' or equal to 0.05. Returns a data frame with GO categories.
#'
#' @param data a vector of ENTREZ Gene IDs.
#' @param background a vector of the subset of genes which are used as the universe
#' for the hypergeometric calculation, also ENTREZ Gene IDs.
#' @param annotation - a string giving the name of the annotation data package for
#' the chip used to generate the data. See \code{\link[GOstats]{GOHyperGParams-class}}.
#'
#' @return data frame object with GO results. Columns:
#'
#' @seealso \code{\link{fs}}
#'
#' @export


gostats <- function(data, background, annotation){
  paramsMF <- new("GOHyperGParams", geneIds = data,
                  universeGeneIds = background, annotation = annotation,
                  ontology = "MF", pvalueCutoff = 1,
                  conditional = FALSE, testDirection = "over")
  hgOver <- hyperGTest(paramsMF)
  k <- p.adjust(pvalues(hgOver), method="bonferroni")
  results.MF <- data.frame(Ontology = "MF", summary(hgOver, pvalue = 0.1),
                           P.Bonferroni = k[pvalues(hgOver) <= 0.1])
  results.MF <- results.MF[results.MF$P.Bonferroni <= 0.05,]
  colnames(results.MF)[2] <- "GOID"
  paramsBP <- new("GOHyperGParams", geneIds = data,
                  universeGeneIds = background, annotation = annotation,
                  ontology = "BP", pvalueCutoff = 1,
                  conditional = FALSE, testDirection = "over")
  hgOver <- hyperGTest(paramsBP)
  k <- p.adjust(pvalues(hgOver), method="bonferroni")
  results.BP <- data.frame(Ontology = "BP", summary(hgOver, pvalue = 0.1),
                           P.Bonferroni = k[pvalues(hgOver) <= 0.1])
  results.BP <- results.BP[results.BP$P.Bonferroni <= 0.05,]
  colnames(results.BP)[2] <- "GOID"
  paramsCC <- new("GOHyperGParams", geneIds = data,
                  universeGeneIds = background, annotation = annotation,
                  ontology = "CC", pvalueCutoff = 1,
                  conditional = FALSE, testDirection = "over")
  hgOver <- hyperGTest(paramsCC)
  k <- p.adjust(pvalues(hgOver), method="bonferroni")
  results.CC <- data.frame(Ontology = "CC", summary(hgOver, pvalue = 0.1),
                           P.Bonferroni = k[pvalues(hgOver) <= 0.1])
  results.CC <- results.CC[results.CC$P.Bonferroni <= 0.05,]
  colnames(results.CC)[2] <- "GOID"
  results <- rbind(results.MF, results.BP, results.CC)
  results <- results[order(results$P.Bonferroni), ]
  return(results)
}
