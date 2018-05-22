
#' Calculate Buffers
#'
#' A simplified form of the other buffer calculation object; used to calculate the needed reagents necessary to make X of a buffer.
#'
#' @param buffer.df dataframe containing recipes.
#' @param amount_col String identifying the values / volumes
#' @param amount_needed how much? ex: 5
#' @param amount_needed_unit "ml","Samples", etc.
#' @param buffer.name filter for a specific buffer?  Takes a String.
#' @param buffer.col Default is "Buffer".  Supply this if you are filtering for a specific buffer.
#' @export
#' @import tidyverse
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
buffer_calculate <- function(buffer.df,amount_col,amount_needed = 1,amount_needed_unit = "mL",buffer.name = NULL,buffer.col = "Buffer"){
  new_col_name <- paste("For",amount_needed,amount_needed_unit)

  if(!is.null(buffer.name)){
    buffer.df <- buffer.df[which(buffer.df[,buffer.col] == buffer.name),]
  }

  buffer.df %>%
    separate(amount_col,into = c("amount.per","amount.unit"),sep = " ") %>%
    mutate(amount.per = paste(as.numeric(amount.per) * amount_needed,amount.unit)) %>%
    select(-amount.unit) -> buffer.df
  buffer_cols <- colnames(buffer.df)
  buffer_cols[match(x = "amount.per",table = buffer_cols)] <- new_col_name
  colnames(buffer.df) <- buffer_cols
  return(buffer.df)

}
