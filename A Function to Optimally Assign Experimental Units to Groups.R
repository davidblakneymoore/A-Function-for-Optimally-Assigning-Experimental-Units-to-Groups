
# A Function to Optimize Group Assignments by Minimizing the Variability in
# Group Means and Standard Deviations of One or More Variables

# David Moore

# Dec. 12th, 2021


# Explanation

# To assign experimental units to treatments, it might be worthwhile to ensure
# that means or standard deviations (or both) of some variables that define
# experimental units are as equal as possible between treatment groups. This
# function takes measurements from potential experimental units and assigns
# treatment groups that equalize means, standard deviations, and both means and
# standard deviations as much as possible.

# Furthermore, if you have more potential experimental units than you need for
# your study or experiment, this function will optimally determine which
# experimental units best equalize means and standard deviations based on the
# number of treatment groups you'll have and the number of experimental units
# you'll have in each group.

# This function can take more than one measurement variable into account to
# determine the optimal combination.

# This function uses the 'comboGroups' function from the 'RcppAlgos' package in
# line 125. I hope this function and this package do not change.

# This function takes 8 arguments. The first, the second, the fourth, and the
# fifth are required.

# 'Identifiers' is a vector containing the names of the potential experimental
# units.

# '...' is a numeric vector, or are numeric vectors, containing the
# measurements that will be used to optimally assign groups.

# 'Data_Frame' is an optional data frame to include such that column names can
# be supplied for the 'Identifiers' and '...' arguments. The data frame that
# these columns are from should be provided for this 'Data_Frame' argument.

# 'Number_of_Groups' is the number of treatment groups you wish to have.

# 'Number_of_Items_in_Each_Group' is the number of experimental units you wish
# to have in each treatment group. This function assumes that each treatment
# group contains the same number of experimental units.

# 'Variable_Weights = rep(1, ncol(cbind(...)))' are the weights given to each
# of the provided variables in the function. The default value,
# 'rep(1, ncol(cbind(...)))', ensures that each variable is weighed equally.

# 'Mean_Weight = 1' is the weight given to the mean for each variable. If it is
# preferable that means are less variable than standard deviations, you may opt
# to make the value of this argument greater than the value of the subsequent
# argument. The default value, '1', assigns a weight of 1 to means.

# 'Standard_Deviation_Weight = 1' is the weight given to the standard deviation
# for each variable. If it is only necessary to consider and minimize the
# variability in group means - if variability in standard deviations can be
# ignored - you may opt to assign the value of 0 to this argument. The default
# value, '1', assigns a weight of 1 to standard deviations.


# The Function

Optimizing_Group_Assignments <- function (Identifiers, ..., Data_Frame, Number_of_Groups, Number_of_Items_in_Each_Group, Variable_Weights = rep(1, ncol(cbind(...))), Mean_Weight = 1, Standard_Deviation_Weight = 1) {
  
  # Load the Required Package
  
  if (!require(RcppAlgos)) {
    install.packages('RcppAlgos')
  }
  library(RcppAlgos)
  
  
  # Format the Inputs
  
  Identifiers_Name <- deparse(substitute(Identifiers))
  if (!missing(Data_Frame)) {
    Identifiers <- Data_Frame[deparse(substitute(Identifiers))]
    Measurements <- sapply(substitute(c(...)), deparse)[-1]
    Data_Frame <- data.frame(Identifiers, Data_Frame[Measurements])
  } else if (missing(Data_Frame)) {
    Data_Frame <- data.frame(Identifiers, ...)
  }
  Identifiers <- Data_Frame[, 1]
  Variable_Names <- colnames(Data_Frame[, 2:ncol(Data_Frame)])
  
    
  # Meet Some Initial Conditions

  if (length(Variable_Weights) != length(Variable_Names)) {
    stop ("'Variable_Weights' must contain as many numbers as there are measurement variables being considered.")
  }
  if (!is.numeric(Variable_Weights) | any(Variable_Weights < 0)) {
    stop ("All variable weights must be numeric and non-negative.")
  }
  if (!all(sapply(Data_Frame[, 2:ncol(Data_Frame)], is.numeric))) {
    stop ("The measurement variables you provide must be numeric.")
  }
  if (!is.numeric(Number_of_Groups)) {
    stop ("'Number_of_Groups' must be numeric.")
  }
  if (!is.numeric(Number_of_Items_in_Each_Group)) {
    stop ("'Number_of_Items_in_Each_Group' must be numeric.")
  }
  if (length(Identifiers) < (Number_of_Groups * Number_of_Items_in_Each_Group)) {
    stop ("You don't have enough potential experimental units to have that many groups with that many experimental units in each group.")
  }
  if (length(Identifiers) != length(unique(Identifiers))) {
    stop ("Some of the identifiers you provided are the same.")
  }


  # Generate All Possible Combinations

  Combinations <- as.list(as.data.frame(combn(Identifiers, Number_of_Groups * Number_of_Items_in_Each_Group)))


  # Generate All Possible Group Assignments From These Combinations

  Possible_Groups <- lapply(Combinations, function (x) {
    RcppAlgos::comboGroups(x, Number_of_Groups)
  })
  Possible_Groups <- lapply(Possible_Groups, function (x) {
    colnames(x) <- gsub("Grp", "Group_", colnames(x))
    x
  })
  List_of_Possible_Groups <- lapply(Possible_Groups, function (x) {
    lapply(as.list(as.data.frame(t(x))), function (y) {
      split(y, colnames(x))
    })
  })
  List_of_Possible_Groups <- lapply(List_of_Possible_Groups, function (w) {
    lapply(w, function (x) {
      lapply(x, function (y) {
        z <- Data_Frame[Data_Frame[, Identifiers_Name] %in% y, ]
        z[, Identifiers_Name] <- y
        z[, c(which(colnames(z) == Identifiers_Name), which(colnames(z) != Identifiers_Name))]
      })
    })
  })
  
    
  # Calculate the Means and the Standard Deviations
  
  Means_and_Standard_Deviations <- lapply(List_of_Possible_Groups, function (x) {
    lapply(x, function (y) {
      sapply(y, function (z) {
        Means <- sapply(z[, 2:ncol(z)], mean)
        names(Means) <- paste0("Mean_", names(Means))
        Standard_Deviations <- sapply(z[, 2:ncol(z)], sd)
        names(Standard_Deviations) <- paste0("Standard_Deviation_", names(Standard_Deviations))
        c(Means, Standard_Deviations)
      })
    })
  })
  Maximum_Means_and_Standard_Deviations <- lapply(Means_and_Standard_Deviations, function (x) {
    lapply(x, function (y) {
      apply(as.data.frame(t(as.matrix(y))), 2, max)
    })
  })
  Maximum_Means_and_Standard_Deviations <- lapply(Maximum_Means_and_Standard_Deviations, function (x) {
    as.data.frame(do.call('rbind', x))
  })
  Maximum_Means_and_Standard_Deviations <- as.data.frame(do.call('rbind', Maximum_Means_and_Standard_Deviations))
  Maximum_Means_and_Standard_Deviations <- sapply(Maximum_Means_and_Standard_Deviations, max)
  Relativized_Means_and_Standard_Deviations <- lapply(Means_and_Standard_Deviations, function (x) {
    lapply(x, function (y) {
      z <- y / Maximum_Means_and_Standard_Deviations
      rownames(z) <- paste0("Relativized_", rownames(z))
      z
    })
  })
  Average_Relativized_Means_and_Standard_Deviations <- lapply(Relativized_Means_and_Standard_Deviations, function (x) {
    lapply(x, function (y) {
      apply(y, 1, mean)
    })
  })


  # Determine the Variabilities in Means and in Standard Deviations for Each
  # Combination

  Relativized_Mean_and_Standard_Deviation_Sums_of_Squares <- mapply(function (a, b) {
    mapply(function (p, q) {
      rowSums((p - q) ^ 2)
    }, p = a, q = b, SIMPLIFY = F)
  }, a = Relativized_Means_and_Standard_Deviations, b = Average_Relativized_Means_and_Standard_Deviations, SIMPLIFY = F)
  Relativized_Mean_and_Standard_Deviation_Sums_of_Squares <- lapply(Relativized_Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    lapply(x, function (y) {
      names(y) <- paste0(names(y), "_Sums_of_Squares")
      y
    })
  })


  # Weigh the Means, the Standard Deviations, and the Variables

  Weighted_Relativized_Mean_and_Standard_Deviation_Sums_of_Squares <- lapply(Relativized_Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    lapply(x, function (y) {
      y[grep("^Relativized_Mean", names(y))] <- y[grep("^Relativized_Mean", names(y))] * Mean_Weight
      y[grep("^Relativized_Standard_Deviation", names(y))] <- y[grep("^Relativized_Standard_Deviation", names(y))] * Standard_Deviation_Weight
      for (i in seq_len(length(Variable_Weights))) {
        y[union(grep(paste0("^Relativized_Mean_", Variable_Names[i], "_Sums_of_Squares"), names(y)), grep(paste0("^Relativized_Standard_Deviation_", Variable_Names[i], "_Sums_of_Squares"), names(y)))] <- y[union(grep(paste0("^Relativized_Mean_", Variable_Names[i], "_Sums_of_Squares"), names(y)), grep(paste0("^Relativized_Standard_Deviation_", Variable_Names[i], "_Sums_of_Squares"), names(y)))] * Variable_Weights[i]
      }
      y
    })
  })
  Total_Relatived_and_Weighted_Sums_of_Squares <- lapply(Weighted_Relativized_Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    sapply(x, sum)
  })


  # Determine Which Combination is Optimal

  Minimum_Total_Relatived_and_Weighted_Sums_of_Squares <- data.frame(Position = sapply(Total_Relatived_and_Weighted_Sums_of_Squares, which.min), Total_Relatived_and_Weighted_Sum_of_Squares = sapply(Total_Relatived_and_Weighted_Sums_of_Squares, min))
  rownames(Minimum_Total_Relatived_and_Weighted_Sums_of_Squares) <- NULL


  # The Optimal Combination

  list(Optimal_Combination = List_of_Possible_Groups[[which.min(Minimum_Total_Relatived_and_Weighted_Sums_of_Squares$Total_Relatived_and_Weighted_Sum_of_Squares)]][[Minimum_Total_Relatived_and_Weighted_Sums_of_Squares$Position[which.min(Minimum_Total_Relatived_and_Weighted_Sums_of_Squares$Total_Relatived_and_Weighted_Sum_of_Squares)]]], Means_and_Standard_Deviations_of_the_Optimal_Combination = Means_and_Standard_Deviations[[which.min(Minimum_Total_Relatived_and_Weighted_Sums_of_Squares$Total_Relatived_and_Weighted_Sum_of_Squares)]][[Minimum_Total_Relatived_and_Weighted_Sums_of_Squares$Position[which.min(Minimum_Total_Relatived_and_Weighted_Sums_of_Squares$Total_Relatived_and_Weighted_Sum_of_Squares)]]])
}


# Test the Function Out

# Let's try to use this function on some made-up data. Let's pretend we have
# 14 potential study trees and we want to have 3 treatment groups and 4
# experimental units in each group. Therefore, we'll only need 12 trees, but
# it will be nice to have extra trees as options so that we can optimize how
# similar means and standard deviations of tree diameters and tree heights are
# between groups. We'll use both tree diameter and tree height as the
# measurement variables. We'll try to minimize the variability in means and
# standard deviations of these two variables across treatment groups.

# Be patient - this code will probably take a while to run.


# Generate Some Practice Data

Tree_Numbers <- as.character(1:14)
Diameters <- c(10, 12, 13, 13, 14, 15, 16, 18, 22, 23, 24, 26, 25, 26)
Heights <- c(45, 55, 53, 42, 44, 44, 46, 57, 58, 55, 53, 58, 60, 62)
Tree_Data <- data.frame(Tree_Number = Tree_Numbers, Diameter = Diameters, Height = Heights)


# Use the Function

Optimizing_Group_Assignments(Identifiers = Tree_Number, Diameter, Height, Data_Frame = Tree_Data, Number_of_Groups = 3, Number_of_Items_in_Each_Group = 4, Variable_Weights = c(1, 1), Mean_Weight = 2, Standard_Deviation_Weight = 1)

# Here is the output from the preceding line of code:

# $Optimal_Combination
# $Optimal_Combination$Group_1
# Identifiers Diameters Heights
# 1            1        10      45
# 8            8        18      57
# 10          10        23      55
# 13          13        25      60
#
# $Optimal_Combination$Group_2
# Identifiers Diameters Heights
# 2            2        12      55
# 7            7        16      46
# 11          11        24      53
# 14          14        26      62
#
# $Optimal_Combination$Group_3
# Identifiers Diameters Heights
# 3            3        13      53
# 5            5        14      44
# 9            9        22      58
# 12          12        26      58
#
#
# $Means_and_Standard_Deviations
# Group_1   Group_2   Group_3
# Mean_Diameters               19.000000 19.500000 18.750000
# Mean_Heights                 54.250000 54.000000 53.250000
# Standard_Deviation_Diameters  6.683313  6.608076  6.291529
# Standard_Deviation_Heights    6.500000  6.582806  6.601767
