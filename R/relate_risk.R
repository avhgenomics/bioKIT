
#' Calculate Relative Risk
#'
#' Calculates the relative risk of two populations and returns either a data frame or a string containing the results.
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
calculate.relative.risk <- function(case.positive,case.negative,control.positive,control.negative,return_df = F){
  relative.risk.base <- (case.positive/ (case.positive+case.negative)) / (control.positive/ (control.positive+control.negative))

  case.n = case.positive+case.negative
  control.n = control.positive+control.negative
  upper.CI = exp(log(relative.risk.base)+1.96*sqrt((((case.n-case.positive)/case.positive)/case.n)+(((control.n-control.positive)/control.positive)/control.n)))
  lower.CI = exp(log(relative.risk.base)-1.96*sqrt((((case.n-case.positive)/case.positive)/case.n)+(((control.n-control.positive)/control.positive)/control.n)))

  calculated.stats <- paste("Relative Risk:",relative.risk.base,", 95% Confidence Interval:",lower.CI,"-",upper.CI)

  if(return_df){
    df <- data.frame(relative.risk = relative.risk.base,
                     upper.CI = upper.CI,
                     lower.CI = lower.CI)

    return(df)
  } else {return(calculated.stats)}

}


#calculate.relative.risk(case.positive = 9,
#                     case.negative = 41,
#                     control.positive = 20,
#                     control.negative = 29,return_df = T)

