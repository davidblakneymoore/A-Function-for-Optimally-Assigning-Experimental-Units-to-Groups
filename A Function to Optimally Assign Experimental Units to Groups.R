
# A Function to Optimize Group Assignments by Minimizing the Variability in
# Group Means and Standard Deviations of One or More Variables

# David Moore

# Dec. 13th, 2021


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

# This function rescales each measurement variable to a standard normal
# distribution by subtracting the colun mean from each measurement and then by
# dividing by the column standard deviation. By rescaling the measurements,
# it's possible to compare mean and standard deviation variability between
# groups between measurements.

# This function uses the 'comboGroups' function from the 'RcppAlgos' package in
# line 185. I hope this function and this package do not change.

# This function takes 8 arguments. The first, the second, the fourth, and the
# fifth arguments are required.

# 'Identifiers' is a vector containing the names of the potential experimental
# units.

# '...' is a vector, or are vectors, containing the  measurements that will be
# used to optimally assign groups. Although this argument will typically
# contain numeric data, this argument could contain one or more characer or
# factor vector or vectors - dummy variables will be created for categorical
# variables so that these categories can be optimally split up between groups
# as well.

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
  Measurements <- Data_Frame[, 2:ncol(Data_Frame)]
  
    
  # Meet Some Initial Conditions

  if (length(Variable_Weights) != ncol(Measurements)) {
    stop ("'Variable_Weights' must contain as many numbers as there are measurement variables being considered.")
  }
  if (!is.numeric(Variable_Weights) | any(Variable_Weights < 0)) {
    stop ("All variable weights must be numeric and non-negative.")
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
  
  
  # Continue Formatting the Inputs
  
  Variable_Metadata <- data.frame(Variable = colnames(Measurements), Weight = Variable_Weights)
  Parameter_Metadata <- data.frame(Parameter = c("Mean", "Standard_Deviation"), Weight = c(Mean_Weight, Standard_Deviation_Weight))
  Categorical_Measurements <- as.data.frame(Measurements[, sapply(Measurements, function (x) {
    is.character(x) | is.factor(x)
  })])
  colnames(Categorical_Measurements) <- colnames(Measurements)[sapply(Measurements, function (x) {
    is.character(x) | is.factor(x)
  })]
  Numeric_Measurements <- as.data.frame(Measurements[, sapply(Measurements, function (x) {
    !is.character(x) & !is.factor(x)
  })])
  colnames(Numeric_Measurements) <- colnames(Measurements)[sapply(Measurements, function (x) {
    !is.character(x) & !is.factor(x)
  })]
  Number_of_Unique_Categories <- NULL
  for (i in seq_len(ncol(Categorical_Measurements))) {
    Number_of_Unique_Categories[i] <- length(unique(Categorical_Measurements[, i]))
  }
  Total_Number_of_Unique_Categories <- sum(Number_of_Unique_Categories)
  Numeric_Variable_Weights <- Variable_Weights[which(sapply(Measurements, function (x) {
    !is.character(x) & !is.factor(x)
  }))]
  Categorical_Variable_Weights <- Variable_Weights[which(sapply(Measurements, function (x) {
    is.character(x) | is.factor(x)
  }))]
  Categorical_Variable_Weights <- unlist(mapply(rep, (Categorical_Variable_Weights / Number_of_Unique_Categories), Number_of_Unique_Categories))
  Dummy_Variables <- as.data.frame(matrix(NA, ncol = Total_Number_of_Unique_Categories, nrow = nrow(Data_Frame)))
  k <- 1
  for (i in seq_len(ncol(Categorical_Measurements))) {
    for (j in seq_len(length(unique(Categorical_Measurements[, i])))) {
      Dummy_Variables[, k] <- ifelse(Categorical_Measurements[, i] == unique(Categorical_Measurements[, i])[j], 1, 0)
      colnames(Dummy_Variables)[k] <- paste0(colnames(Categorical_Measurements)[i], "_", unique(Categorical_Measurements[, i])[j])
      k <- k + 1
    }
  }
  Measurements <- data.frame(Numeric_Measurements, Dummy_Variables)
  Variable_Weights <- c(Numeric_Variable_Weights, Categorical_Variable_Weights)
  Variable_Names <- colnames(Measurements)
  Data_Frame <- data.frame(Identifiers, Measurements)
  colnames(Data_Frame)[1] <- Identifiers_Name
  
  
  # Rescale the Data by Column to Facilitate Comparisons Between Columns
  
  Rescaled_Data <- as.data.frame(lapply(Data_Frame[, 2:ncol(Data_Frame)], function (x) {
    (x - mean(x)) / sd(x)
  }))
  Rescaled_Data[, Identifiers_Name] <- Data_Frame[, Identifiers_Name]
  Rescaled_Data <- Rescaled_Data[, c(which(colnames(Rescaled_Data) == Identifiers_Name), which(colnames(Rescaled_Data) != Identifiers_Name))]
  
  
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
  Original_List_of_Possible_Groups <- lapply(List_of_Possible_Groups, function (w) {
    lapply(w, function (x) {
      lapply(x, function (y) {
        z <- Data_Frame[Data_Frame[, Identifiers_Name] %in% y, ]
        z[, Identifiers_Name] <- y
        z[, c(which(colnames(z) == Identifiers_Name), which(colnames(z) != Identifiers_Name))]
      })
    })
  })
  Rescaled_List_of_Possible_Groups <- lapply(List_of_Possible_Groups, function (w) {
    lapply(w, function (x) {
      lapply(x, function (y) {
        z <- Rescaled_Data[Rescaled_Data[, Identifiers_Name] %in% y, ]
        z[, Identifiers_Name] <- y
        z[, c(which(colnames(z) == Identifiers_Name), which(colnames(z) != Identifiers_Name))]
      })
    })
  })
  
  
  # Calculate the Rescaled Means and the Standard Deviations
  
  Rescaled_Means_and_Standard_Deviations <- lapply(Rescaled_List_of_Possible_Groups, function (x) {
    lapply(x, function (y) {
      sapply(y, function (z) {
        Means <- sapply(z[, 2:ncol(z)], mean)
        names(Means) <- paste0("Rescaled_Mean_", names(Means))
        Standard_Deviations <- sapply(z[, 2:ncol(z)], sd)
        names(Standard_Deviations) <- paste0("Rescaled_Standard_Deviation_", names(Standard_Deviations))
        c(Means, Standard_Deviations)
      })
    })
  })
  
  Average_Rescaled_Means_and_Standard_Deviations <- lapply(Rescaled_Means_and_Standard_Deviations, function (x) {
    lapply(x, function (y) {
      apply(y, 1, mean)
    })
  })
  
  
  # Determine the Variabilities in Rescaled Means and in Standard Deviations for
  # Each Combination
  
  Rescaled_Mean_and_Standard_Deviation_Sums_of_Squares <- mapply(function (a, b) {
    mapply(function (p, q) {
      rowSums((p - q) ^ 2)
    }, p = a, q = b, SIMPLIFY = F)
  }, a = Rescaled_Means_and_Standard_Deviations, b = Average_Rescaled_Means_and_Standard_Deviations, SIMPLIFY = F)
  Rescaled_Mean_and_Standard_Deviation_Sums_of_Squares <- lapply(Rescaled_Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    lapply(x, function (y) {
      names(y) <- paste0(names(y), "_Sums_of_Squares")
      y
    })
  })
  
  
  # Weigh the Means, the Standard Deviations, and the Variables
  
  Weighted_Rescaled_Mean_and_Standard_Deviation_Sums_of_Squares <- lapply(Rescaled_Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    lapply(x, function (y) {
      y[grep("^Rescaled_Mean", names(y))] <- y[grep("^Rescaled_Mean", names(y))] * Mean_Weight
      y[grep("^Rescaled_Standard_Deviation_", names(y))] <- y[grep("^Rescaled_Standard_Deviation_", names(y))] * Standard_Deviation_Weight
      for (i in seq_len(length(Variable_Weights))) {
        y[union(grep(paste0("^Rescaled_Mean", Variable_Names[i], "_Sums_of_Squares"), names(y)), grep(paste0("^Rescaled_Standard_Deviation_", Variable_Names[i], "_Sums_of_Squares"), names(y)))] <- y[union(grep(paste0("^Rescaled_Mean", Variable_Names[i], "_Sums_of_Squares"), names(y)), grep(paste0("^Rescaled_Standard_Deviation_", Variable_Names[i], "_Sums_of_Squares"), names(y)))] * Variable_Weights[i]
      }
      y
    })
  })
  Total_Weighted_Rescaled_Sums_of_Squares <- lapply(Weighted_Rescaled_Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    sapply(x, sum)
  })
  
  
  # Determine Which Combination is Optimal
  
  Minimum_Total_Weighted_Rescaled_Sums_of_Squares <- data.frame(Position = sapply(Total_Weighted_Rescaled_Sums_of_Squares, which.min), Total_Weighted_Rescaled_Sum_of_Squares = sapply(Total_Weighted_Rescaled_Sums_of_Squares, min))
  rownames(Minimum_Total_Weighted_Rescaled_Sums_of_Squares) <- NULL
  
  
  # The Optimal Combination
  
  Optimal_Combination <- Original_List_of_Possible_Groups[[which.min(Minimum_Total_Weighted_Rescaled_Sums_of_Squares$Total_Weighted_Rescaled_Sum_of_Squares)]][[Minimum_Total_Weighted_Rescaled_Sums_of_Squares$Position[which.min(Minimum_Total_Weighted_Rescaled_Sums_of_Squares$Total_Weighted_Rescaled_Sum_of_Squares)]]]
  Optimal_Combination <- lapply(Optimal_Combination, function(x) {
    rownames(x) <- NULL
    x
  })
  Means_and_Standard_Deviations_of_the_Optimal_Combination <- as.data.frame(sapply(Optimal_Combination, function (z) {
    Means <- sapply(z[, 2:ncol(z)], mean)
    names(Means) <- paste0("Mean_", names(Means))
    Standard_Deviations <- sapply(z[, 2:ncol(z)], sd)
    names(Standard_Deviations) <- paste0("Standard_Deviation_", names(Standard_Deviations))
    c(Means, Standard_Deviations)
  }))
  Means_and_Standard_Deviations_of_the_Optimal_Combination$Variable <- gsub("Mean_|Standard_Deviation_", "", rownames(Means_and_Standard_Deviations_of_the_Optimal_Combination))
  Parameters <- unlist(mapply(function (x, y) {
    sub(y, "", x)
  }, x = rownames(Means_and_Standard_Deviations_of_the_Optimal_Combination), y = paste0("_", gsub("Mean_|Standard_Deviation_", "", rownames(Means_and_Standard_Deviations_of_the_Optimal_Combination))), SIMPLIFY = F))
  names(Parameters) <- NULL
  Means_and_Standard_Deviations_of_the_Optimal_Combination$Parameter <- Parameters
  rownames(Means_and_Standard_Deviations_of_the_Optimal_Combination) <- NULL
  Means_and_Standard_Deviations_of_the_Optimal_Combination <- Means_and_Standard_Deviations_of_the_Optimal_Combination[, c(which(colnames(Means_and_Standard_Deviations_of_the_Optimal_Combination) == "Variable"), which(colnames(Means_and_Standard_Deviations_of_the_Optimal_Combination) == "Parameter"), grep("Group", colnames(Means_and_Standard_Deviations_of_the_Optimal_Combination)))]
  list(Optimal_Combination = Optimal_Combination, Means_and_Standard_Deviations_of_the_Optimal_Combination = Means_and_Standard_Deviations_of_the_Optimal_Combination, Variable_Metadata = Variable_Metadata, Parameter_Metadata = Parameter_Metadata)
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
Crown_Classes <- c("Intermediate", "Codominant", "Codominant", "Intermediate", "Codominant", "Codominant", "Codominant", "Dominant", "Codominant", "Codominant", "Codominant", "Dominant", "Dominant", "Dominant")
Tree_Data <- data.frame(Tree_Number = Tree_Numbers, Diameter = Diameters, Height = Heights, Crown_Class = Crown_Classes)


# Use the Function

Optimizing_Group_Assignments(Identifiers = Tree_Number, Diameter, Height, Crown_Class, Data_Frame = Tree_Data, Number_of_Groups = 3, Number_of_Items_in_Each_Group = 4, Variable_Weights = c(1, 1, 1), Mean_Weight = 2, Standard_Deviation_Weight = 1)

# Here is the output from the preceding line of code:

# $Optimal_Combination
# $Optimal_Combination$Group_1
#   Tree_Number Diameter Height Crown_Class_Intermediate Crown_Class_Codominant Crown_Class_Dominant
# 1           2       12     55                        0                      1                    0
# 2           7       16     46                        0                      1                    0
# 3          11       24     53                        0                      1                    0
# 4          14       26     62                        0                      0                    1
# 
# $Optimal_Combination$Group_2
#   Tree_Number Diameter Height Crown_Class_Intermediate Crown_Class_Codominant Crown_Class_Dominant
# 1           3       13     53                        0                      1                    0
# 2           6       15     44                        0                      1                    0
# 3           9       22     58                        0                      1                    0
# 4          12       26     58                        0                      0                    1
# 
# $Optimal_Combination$Group_3
#   Tree_Number Diameter Height Crown_Class_Intermediate Crown_Class_Codominant Crown_Class_Dominant
# 1           5       14     44                        0                      1                    0
# 2           8       18     57                        0                      0                    1
# 3          10       23     55                        0                      1                    0
# 4          13       25     60                        0                      0                    1
# 
# 
# $Means_and_Standard_Deviations_of_the_Optimal_Combination
#                    Variable          Parameter   Group_1   Group_2    Group_3
# 1                  Diameter               Mean 19.500000 19.000000 20.0000000
# 2                    Height               Mean 54.000000 53.250000 54.0000000
# 3  Crown_Class_Intermediate               Mean  0.000000  0.000000  0.0000000
# 4    Crown_Class_Codominant               Mean  0.750000  0.750000  0.5000000
# 5      Crown_Class_Dominant               Mean  0.250000  0.250000  0.5000000
# 6                  Diameter Standard_Deviation  6.608076  6.055301  4.9665548
# 7                    Height Standard_Deviation  6.582806  6.601767  6.9761498
# 8  Crown_Class_Intermediate Standard_Deviation  0.000000  0.000000  0.0000000
# 9    Crown_Class_Codominant Standard_Deviation  0.500000  0.500000  0.5773503
# 10     Crown_Class_Dominant Standard_Deviation  0.500000  0.500000  0.5773503
# 
# $Variable_Metadata
#      Variable Weight
# 1    Diameter      1
# 2      Height      1
# 3 Crown_Class      1
# 
# $Parameter_Metadata
#            Parameter Weight
# 1               Mean      2
# 2 Standard_Deviation      1
