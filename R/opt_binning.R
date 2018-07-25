#' Optimal binning of dataframe
#'
#' This function returns a dataframe with all variables that
#'  have not been excluded in the "exclude_variables" list, and that have more than the min_lvl
#'  amount of levels, in optimal bins to the target.
#' @param 
#' @keywords optimal binning, binning
#' @return 
#' @export
#' @examples
#' opt_binning()

opt_binning = function(data, target, exclude_variables = NULL, min_lvl=8, p=0.05){
  # Determine which columns are to be binned
  target_pos = which(colnames(data)==target)
  col_names = colnames(data[,-c(exclude_variables,target_pos)])
  
  bin_vars = vector(mode="character")
  for (col_name in col_names){
    if (length(table(data[,col_name])) > min_lvl && typeof(data[,col_name]) != "character"){
      bin_vars = append(bin_vars, col_name)
    }
  }
  # Initialize copy of original df
  result = data
  # Use smbinning package to get optimal cuts, then transform df
  for(bin_var in bin_vars){
    binning_result =     smbinning(df=data, y=target, x=bin_var, p)
    result[,bin_var] =   cut(data[,bin_var],
                             c(-Inf,binning_result$cuts, Inf),
                             labels = c(1:(length(binning_result$cuts) +1)))
  }
  return(result)
}