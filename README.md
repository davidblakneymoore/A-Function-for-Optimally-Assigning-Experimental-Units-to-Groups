# A Function for Optimally Assigning Experimental Units to Groups

This repository contains the code for an R function that will optimally assign experimental units to groups.

This function takes 6 arguments (the first 3 are required):

Correlation_Coefficients: a numeric vector containing the correlation coefficients to be analyzed

Numbers_of_Observations: a numeric or an integer vector containing the numbers of observations that went in to each of the corresponding correlation coefficients

Identifiers: a character or a factor vector containing names to identify each corresponding correlation coefficient

Data_Frame: an optional data frame to include such that column names can be supplied for the first three arguments (the data frame that these columns are from should be provided for this Data_Frame argument)

Alpha = 0.05: a value of alpha against which significance can be tested (the default is 0.05)

Control_for_Experimentwise_Error = TRUE: an argument specifying whether or not this function should give conservative estimates (by holding the experimentwise error rate at the given value of alpha) or liberal estimates (by using the given value of alpha for each pairwise comparison); the default, TRUE, holds the experimentwise error rate at alpha and calculates the comparisonwise error rate based on the number of pairwise comparisons

Though this function only uses base R functions, it was heavily inspired by the 'agricolae' package, particularly the orderPvalue() and lastC() functions. Thank you Felipe de Mendiburu!
