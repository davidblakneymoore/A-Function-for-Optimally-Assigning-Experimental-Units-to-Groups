
# A Function to Optimize Group Assignments by Minimizing the Variability in
# Group Means and Standard Deviations

# David Moore

# Dec. 7th, 2021


# Explanation

# To assign experimental units to treatments, it might be worthwhile to ensure
# that means or standard deviations (or both) of some variable that defines
# experimental units are as equal as possible between treatment groups. This
# function takes measurements from potential experimental units and assigns
# treatment groups that equalize means, standard deviations, and both means and
# standard deviations as much as possible.

# Furthermore, if you have more potential experimental units than you need for
# your study or experiment, this function will optimally determine which
# experimental units best equalize means and standard deviations based on the
# number of treatment groups you'll have and the number of experimental units
# you'll have in each group.

# This function uses the 'comboGroups' function from the 'RcppAlgos' package in
# line 82. I hope this function and this package don't change.


# The Function

Optimizing_Group_Assignments <- function (Identifiers, Measurements, Data_Frame, Number_of_Groups, Number_of_Items_in_Each_Group, Optimization_Method = "Both") {
  
  # Load the Required Package
  
  if (!require(RcppAlgos)) {
    install.packages('RcppAlgos')
  }
  library(RcppAlgos)
  
  
  # Meet Some Initial Conditions
  
  if (!is.numeric(Measurements)) {
    stop ("'Measurements' must be numeric.")
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
  if (!(Optimization_Method %in% c("Mean", "Standard Deviation", "Both"))) {
    stop ("'Optimization_Method' must be either 'Mean', 'Standard Deviation', or 'Both'.")
  }
  
  
  # Format the Input
  
  Name_of_Identifiers_to_Return <- substitute(Identifiers)
  Name_of_Measurements_to_Return <- substitute(Measurements)
  if (!missing(Data_Frame)) {
    Identifiers <- as.character(Data_Frame[[deparse(substitute(Identifiers))]])
    Measurements <- as.numeric(Data_Frame[[deparse(substitute(Measurements))]])
  } else if (missing(Data_Frame)) {
    Identifiers <- as.character(Identifiers)
    Measurements <- as.numeric(Measurements)
  }
  Number_of_Observations_Used <- Number_of_Groups * Number_of_Items_in_Each_Group
  
  
  # Generate All Possible Combinations
  
  Combinations <- as.list(as.data.frame(combn(Identifiers, Number_of_Observations_Used)))
  
  
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
        z <- Measurements[Identifiers %in% y]
        data.frame(Identifiers = y, Measurements = z)
      })
    })
  })
  
  
  # Calculate Means and Standard Deviations
  
  Means_and_Standard_Deviations <- lapply(List_of_Possible_Groups, function (x) {
    lapply(x, function (y) {
      sapply(y, function (z) {
        Mean <- mean(z$Measurements)
        Standard_Deviation <- sd(z$Measurements)
        c(Mean, Standard_Deviation)
      })
    })
  })
  Means_and_Standard_Deviations <- lapply(Means_and_Standard_Deviations, function (x) {
    lapply(x, function (y) {
      rownames(y) <- c("Mean", "Standard_Deviation")
      y
    })
  })
  Average_Means_and_Standard_Deviations <- lapply(Means_and_Standard_Deviations, function (x) {
    lapply(x, function (y) {
      apply(y, 1, mean)
    })
  })
  
  
  # Determine the Variabilities in Means and in Standard Deviations for Each Combination
  
  Mean_and_Standard_Deviation_Sums_of_Squares <- mapply(function (p, q) {
    mapply(function (x, y) {
      unlist(mapply(function (a, b) {
        sum((a - b) ^ 2)
      }, a = as.list(as.data.frame(t(x))), b = y, SIMPLIFY = F))
    }, x = p, y = q, SIMPLIFY = F)
  }, p = Means_and_Standard_Deviations, q = Average_Means_and_Standard_Deviations, SIMPLIFY = F)
  Mean_and_Standard_Deviation_Sums_of_Squares <- lapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    as.data.frame(t(as.data.frame(x)))
  })
  Mean_and_Standard_Deviation_Sums_of_Squares <- lapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    colnames(x) <- paste0(colnames(x), "_Sum_of_Squares")
    x
  })
  Mean_and_Standard_Deviation_Sums_of_Squares <- lapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
    x$Relativized_Mean_Sum_of_Squares <- (x$Mean_Sum_of_Squares - mean(x$Mean_Sum_of_Squares)) / mean(x$Mean_Sum_of_Squares)
    x$Relativized_Standard_Deviation_Sum_of_Squares <- (x$Standard_Deviation_Sum_of_Squares - mean(x$Standard_Deviation_Sum_of_Squares)) / mean(x$Standard_Deviation_Sum_of_Squares)
    x$Sum_of_the_Relativized_Mean_Sum_of_Squares_and_the_Relativized_Standard_Deviation_Sum_of_Squares <- x$Relativized_Mean_Sum_of_Squares + x$Relativized_Standard_Deviation_Sum_of_Squares
    x
  })
  
  
  # Determine the Minimum Variabilities and Generate the Final Output
  
  if (Optimization_Method == "Mean") {
    Combination_That_Minimizes_Variability_in_Means <- List_of_Possible_Groups[[which.min(sapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
      sapply(x, min)['Mean_Sum_of_Squares']
    }))]][[sapply(Mean_and_Standard_Deviation_Sums_of_Squares[[which.min(sapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
      sapply(x, min)['Mean_Sum_of_Squares']
    }))]], which.min)['Mean_Sum_of_Squares']]]
    Means_of_the_Combination_That_Minimizes_Variability_in_Means <- as.data.frame(sapply(Combination_That_Minimizes_Variability_in_Means, function (x) {
      mean(x$Measurements)
    }))
    colnames(Means_of_the_Combination_That_Minimizes_Variability_in_Means) <- "Mean"
    Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Means <- as.data.frame(sapply(Combination_That_Minimizes_Variability_in_Means, function (x) {
      sd(x$Measurements)
    }))
    colnames(Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Means) <- "Standard_Deviation"
    Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Means <- cbind(Means_of_the_Combination_That_Minimizes_Variability_in_Means, Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Means)
    Combination_That_Minimizes_Variability_in_Means <- lapply(Combination_That_Minimizes_Variability_in_Means, function (x) {
      setNames(x, c(Name_of_Identifiers_to_Return, Name_of_Measurements_to_Return))
    })
    list(Minimizing_Variability_in_Means = list(Combination_That_Minimizes_Variability_in_Means = Combination_That_Minimizes_Variability_in_Means, Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Means = Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Means))
  } else if (Optimization_Method == "Standard Deviation") {
    Combination_That_Minimizes_Variability_in_Standard_Deviations <- List_of_Possible_Groups[[which.min(sapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
      sapply(x, min)['Standard_Deviation_Sum_of_Squares']
    }))]][[sapply(Mean_and_Standard_Deviation_Sums_of_Squares[[which.min(sapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
      sapply(x, min)['Standard_Deviation_Sum_of_Squares']
    }))]], which.min)['Standard_Deviation_Sum_of_Squares']]]
    Means_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations <- as.data.frame(sapply(Combination_That_Minimizes_Variability_in_Standard_Deviations, function (x) {
      mean(x$Measurements)
    }))
    colnames(Means_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations) <- "Mean"
    Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations <- as.data.frame(sapply(Combination_That_Minimizes_Variability_in_Standard_Deviations, function (x) {
      sd(x$Measurements)
    }))
    colnames(Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations) <- "Standard_Deviation"
    Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations <- cbind(Means_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations, Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations)
    Combination_That_Minimizes_Variability_in_Standard_Deviations <- lapply(Combination_That_Minimizes_Variability_in_Standard_Deviations, function (x) {
      setNames(x, c(Name_of_Identifiers_to_Return, Name_of_Measurements_to_Return))
    })
    list(Minimizing_Variability_in_Standard_Deviations = list(Combination_That_Minimizes_Variability_in_Standard_Deviations = Combination_That_Minimizes_Variability_in_Standard_Deviations, Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations = Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Standard_Deviations))
  } else if (Optimization_Method == "Both") {
    Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations <- List_of_Possible_Groups[[which.min(sapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
      sapply(x, min)['Sum_of_the_Relativized_Mean_Sum_of_Squares_and_the_Relativized_Standard_Deviation_Sum_of_Squares']
    }))]][[sapply(Mean_and_Standard_Deviation_Sums_of_Squares[[which.min(sapply(Mean_and_Standard_Deviation_Sums_of_Squares, function (x) {
      sapply(x, min)['Sum_of_the_Relativized_Mean_Sum_of_Squares_and_the_Relativized_Standard_Deviation_Sum_of_Squares']
    }))]], which.min)['Sum_of_the_Relativized_Mean_Sum_of_Squares_and_the_Relativized_Standard_Deviation_Sum_of_Squares']]]
    Means_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations <- as.data.frame(sapply(Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations, function (x) {
      mean(x$Measurements)
    }))
    colnames(Means_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations) <- "Mean"
    Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations <- as.data.frame(sapply(Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations, function (x) {
      sd(x$Measurements)
    }))
    colnames(Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations) <- "Standard_Deviation"
    Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations <- cbind(Means_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations, Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations)
    Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations <- lapply(Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations, function (x) {
      setNames(x, c(Name_of_Identifiers_to_Return, Name_of_Measurements_to_Return))
    })
    list(Minimizing_Variability_in_Both_Means_and_Standard_Deviations = list(Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations = Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations, Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations = Means_and_Standard_Deviations_of_the_Combination_That_Minimizes_Variability_in_Both_Means_and_Standard_Deviations))
  }
}


# Let's try to use this function on some made-up data. Let's pretend we have
# 14 potential study trees and we want to have 3 treatment groups and 4
# experimental units in each group. Therefore, we'll only need 12 trees, but
# it will be nice to have extra trees as options so that we can optimize how
# similar means and standard deviations of tree diameters are between groups.


# Generate some practice data

Tree_Number <- as.character(1:14)
Diameter <- c(10, 12, 13, 13, 14, 15, 16, 18, 22, 23, 24, 26, 25, 22)
Tree_Data <- data.frame(Tree_Number = Tree_Number, Diameter = Diameter)


# Let's try the function out.

Optimizing_Group_Assignments(Identifiers = Tree_Number, Measurements = Diameter, Data_Frame = Tree_Data, Number_of_Groups = 3, Number_of_Items_in_Each_Group = 4, Optimization_Method = "Both")
