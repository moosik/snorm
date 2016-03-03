#' snorm: Collection of functions to aid data exploration prior to variable adjustment.
#'
#' The snorm package provides three categories of functions:
#' functions related to the description of the metadata variables, singular value
#' decomposition of the data matrix and relationship between the eigenvectors and the
#' variables of the metadata.
#'
#' @section Functions related to the description of the metadata variables:
#' Function \code{\link{summaryTable}} provides a summary description for every
#' variable (column) in the metadata (statistics such as mean, median, maximum, mean
#' and the number of missing cases are provided). Function \code{\link{variablesRelation}}
#' provides a table with correlation P values and statistic estimates for each pair
#' of the variables in the metadata. This helps to understand whether any technical
#' variables are related to the variables of interest.
#'
#' @section Functions related to the SVD of a matrix:
#' Function \code{\link{fs}} performs Singular Value Decomposition (SVD) on a
#' data matrix (using \code{\link[corpcor]{fast.svd}} function) and
#' transforms the matrix D to reflect percent variance explained. Function
#' \code{\link{plotVariance}} plots the percent variance explained. Function
#' \code{\link{matrixOutliers}} creates a scatterplot of the matrix
#' columns corresponding to the most extreme values of the first eigen
#' vector of the SVD.
#'
#' @section Eigen vectors and their relationship with the metadata variables:
#' Function \code{\link{eigenVariable}} calculates an overall ANOVA P value for
#' the relationship between the eigen vectors and the variables in the
#' metadata. Function \code{\link{eigenVariablePlot}} plots a variable from the
#' metadata against an eigen vector. Function \code{\link{pcScatter}} creates a
#' scatter plot of the first two eigen vectors and colors the point by a variable
#' from the metadata. Function \code{\link{geneOntology}} creates a basic unit
#' to obtain MF, BP and CC categories from GO for a gene list.
#'
#' @docType package
#' @name snorm
#' @import ggplot2
#' @import reshape2
#' @import corpcor
#' @import GOstats

NULL

#> NULL
