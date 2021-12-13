# A Function for Optimally Assigning Experimental Units to Groups

To assign experimental units to treatments, it might be worthwhile to ensure that means or standard deviations (or both) of a variable or of some variables that define experimental units are as equal as possible between treatment groups. This function takes measurements from potential experimental units and assigns treatment groups that equalize means, standard deviations, and both means and standard deviations as much as possible.

Furthermore, if you have more potential experimental units than you need for your study or experiment, this function will optimally determine which experimental units best equalize means and standard deviations based on the number of treatment groups you'll have and the number of experimental units you'll have in each group.

This function can take more than one measurement variable into account to determine the optimal combination.

This function uses the `comboGroups` function from the `RcppAlgos` package in line 112. I hope this function and this package do not change.

This function takes 8 arguments. The first, the second, the fourth, and the fifts are required.

`Identifiers` is a vector containing the names of the potential experimental units.

`...` is a vector, or are vectors, containing the  measurements that will be used to optimally assign groups. Although this argument will typically contain numeric data, this argument could contain one or more characer or factor vector or vectors. Dummy variables will be created for categorical variables so that these categories can be optimally split up between groups as well.

`Data_Frame` is an optional data frame to include such that column names can be supplied for the `Identifiers` and the `...` arguments. The data frame that these columns are from should be provided for this `Data_Frame` argument.

`Number_of_Groups` is the number of treatment groups you wish to have.

`Number_of_Items_in_Each_Group` is the number of experimental units you wish to have in each treatment group. This function assumes that each treatment group contains the same number of experimental units.

`Variable_Weights = rep(1, ncol(cbind(...)))` are the weights given to each of the provided variables in the function. The default value, 'rep(1, ncol(cbind(...)))', ensures that each variable is weighed equally.

`Mean_Weight = 1` is the weight given to the mean for each variable. If it is preferable that means are less variable than standard deviations, you may opt to make the value of this argument greater than the value of the subsequent argument. The default value, '1', assigns a weight of 1 to means.

`Standard_Deviation_Weight = 1` is the weight given to the standard deviation for each variable. If it is only necessary to consider and minimize the variability in group means - if variability in standard deviations can be ignored - you may opt to assign the value of 0 to this argument. The default value, '1', assigns a weight of 1 to standard deviations.

<b>Works Cited</b>

Wood, J. 2021. RcppAlgos: High Performance Tools for Combinatorics and Computational Mathematics. R package version 2.4.3. <https://cran.r-project.org/web/packages/RcppAlgos/>.
