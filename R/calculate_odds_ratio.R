
#' Calculate Odds Ratio
#'
#' Calculates the odds ratio of two populations and returns either a data frame or a string containing the results.
#'
#' @param case.positive How many in the case population that are positive for the test; integer.
#' @param case.negative How many in the case population that are negative for the test; integer.
#' @param control.positive How many in the control population that are positive for the test; integer.
#' @param control.negative How many in the control population that are negative for the test; integer.
#' @param return_df Return a dataframe instead of a string? logical.
#' @export
#' @return returns a string or a dataframe.
#' @examples
#' # ADD_EXAMPLES_HERE
calculate.odds.ratio <- function(case.positive,case.negative,control.positive,control.negative,return_df = F){
  odds.ratio.base <- (case.positive*control.negative) / (control.positive*case.negative)
  upper.CI = exp(log(odds.ratio.base)+1.96*sqrt((1/case.positive)+(1/case.negative)+(1/control.positive)+(1/control.negative)))
  lower.CI = exp(log(odds.ratio.base)-1.96*sqrt((1/case.positive)+(1/case.negative)+(1/control.positive)+(1/control.negative)))

  calculated.stats <- paste("Odds Ratio:",odds.ratio.base,", 95% Confidence Interval:",lower.CI,"-",upper.CI)

  if(return_df){
    df <- data.frame(odds.ratio = odds.ratio.base,
                     upper.CI = upper.CI,
                     lower.CI = lower.CI)

    return(df)
  } else {return(calculated.stats)}

}


#calculate.odds.ratio(case.positive = 45,
#                     case.negative = 86,
#                     control.positive = 32,
#                     control.negative = 100,return_df = T)

