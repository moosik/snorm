snorm: an R package with functions to add supervised  exploration of data structure

Description

The snorm package provides three categories of functions: functions related to the description of the metadata variables, singular value decomposition of the data matrix and relationship between the eigenvectors and the variables of the metadata.

Functions related to the description of the metadata variables

Function summaryTable provides a summary description for every variable (column) in the metadata (statistics such as mean, median, maximum, mean and the number of missing cases are provided). Function variablesRelation provides a table with correlation P values and statistic estimates for each pair of the variables in the metadata. This helps to understand whether any technical variables are related to the variables of interest.

Functions related to the SVD of a matrix

Function fs performs Singular Value Decomposition (SVD) on a data matrix (using fast.svd function) and transforms the matrix D to reflect percent variance explained. Function plotVariance plots the percent variance explained. Function matrixOutliers creates a scatterplot of the matrix columns corresponding to the most extreme values of the first eigen vector of the SVD.

Eigen vectors and their relationship with the metadata variables

Function eigenVariable calculates an overall ANOVA P value for the relationship between the eigen vectors and the variables in the metadata. Function eigenVariablePlot plots a variable from the metadata against an eigen vector. Function pcScatter creates a scatter plot of the first two eigen vectors and colors the point by a variable from the metadata.
