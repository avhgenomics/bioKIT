
#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param buffers.df dataframe containing your buffer recipes
#' @param Experiment string to filter by experiment. ex: "PCR"
#' @param Buffer string to filter by buffer. ex: "6x loading dye"
#' @param amount_per_value a numeric value as part of the base recipe for 1 ml/sample. ex: 5, ex: df$amount.per.ml
#' @param amount_per_unit a string as part of the base recipe for 1 ml/sample. ex: "ul, ex: df$amount.unit
#' @param amount_needed a numeric value for how much of the buffer is needed.
#' @param amount_type the unit, as a string, for the amount needed
#' @param Experiment_col number of experiment col, if custom
#' @param Buffer_col number of buffer col, if custom
#' @param Reagent_col number of reagent col, if custom
#' @param Stock_conc_col number of stock concentration col, if custom
#' @param Stock_unit_col number of stock unit col, if custom
#' @param Final_conc_col number of final concentration col, if custom
#' @param Final_unit_col number of final unit col, if custom
#' @export
#' @return a dataframe with the calculations.
#' @examples
#' # ADD_EXAMPLES_HERE
calculate_reagents <- function(buffers.df = NULL,
                               Experiment = NULL,
                               Buffer = NULL,
                               amount_per_value = NULL,
                               amount_per_unit = NULL,
                               amount_needed = NULL,
                               amount_type = NULL,
                               Experiment_col = 1,Buffer_col = 2,Reagent_col = 3,Stock_conc_col = 4,Stock_unit_col = 5,Final_conc_col = 6,Final_unit_col = 7){
  buffers.df$calculated_amount <- paste(amount_per_value * amount_needed,amount_per_unit)
  calc_col_name <- paste("For",amount_needed,amount_type)
  colnames(buffers.df)[which(names(buffers.df) == "calculated_amount")] <- calc_col_name
  df <- buffers.df[which(buffers.df[,Experiment_col] == Experiment),]
  df <- df[which(df[,Buffer_col] == Buffer),]



  return(df)
}



#test.df <- read.csv(file = "../../hichip_buffers.csv",header = T,quote = "",stringsAsFactors = F)
#test.df
#calculate_reagents(test.df,Experiment = "HiChIP",Buffer = "Quench Mix",amount_per_value = test.df$amount.per.ml,amount_per_unit = test.df$amount.unit,amount_needed = 5,amount_type = "ml")
