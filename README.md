# A Function for Optimally Assigning Experimental Units to Groups

To assign experimental units to treatments, it might be worthwhile to ensure that means or standard deviations (or both) of some variable that defines experimental units are as equal as possible between treatment groups. This function takes measurements from potential experimental units and assigns treatment groups that equalize means, standard deviations, and both means and standard deviations of this variable as much as possible.

Furthermore, if you have more potential experimental units than you need for your study or experiment, this function will optimally determine which experimental units best equalize means and standard deviations based on the number of treatment groups you'll have and the number of experimental units you'll have in each group.

This function uses the `comboGroups` function from the `RcppAlgos` package. I hope this function and this package don't change.

This function takes 6 arguments, and all are required except for the third and the last.

`Identifiers` is a vector containing the names of the potential experimental units.

`Measurements` is a numeric vector containing the measurements that will be used to optimally assign groups.

`Data_Frame` is an optional data frame to include such that column names can be supplied for the first two arguments (the data frame that these columns are from should be provided for this Data_Frame argument).

`Number_of_Groups` is the number of treatment groups you wish to have.

`Number_of_Items_in_Each_Group` is the number of experimental units you wish to have in each treatment group (this function assumes that each treatment group contains the same number of experimental units).

`Optimization_Method = "Both"` is an argument specifying whether you wish to optimally assign experimental units to groups by ensuring means, standard deviations, or both means and standard deviations are as similar as possible between groups. The options are `"Mean"`, `"Standard Deviation"`, and `"Both"`. The default, `"Both"`, ensures that both means and standard deviations are as similar as possible between groups.

<b>Works Cited</b>

Wood, J. 2021. RcppAlgos: High Performance Tools for Combinatorics and Computational Mathematics. R package version 2.4.3. <https://cran.r-project.org/web/packages/RcppAlgos/>.
