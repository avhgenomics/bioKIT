# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' bioKIT.unit.convert
#'
#' Converts the scale of the unit in a dataframe when calculating volumes / mass.
#'
#' @param df The dataframe holding the calculations.
#' @param value value represents the column with the total volume.  Ex: 1234 might be 1,234 ul.
#' @param unit the column name, as a string, that represents the unit of the value variable.
#'
#' @return returns a vector containing strings of the value with the attached unit.
#' @examples
#' # ADD_EXAMPLES_HERE
bioKIT.unit.convert <- function(df,value,unit){
  require(tidyverse)
  unit.ref <- data.frame(unit.base = c("pg","ug","mg","g","kg","pl","ul","ml","l"),
                         unit.code = c(1,2,3,4,5,6,7,8,9))

  df$original.unit.code <- as.numeric(match(df[,unit],table = unit.ref$unit.base))
  value_col = names(df) == value
  df %>%
    mutate(division_value = ifelse(df[,value_col] > 1000, df[,value_col] / 1000, df[,value_col]),
           division_unit = ifelse(df[,value_col] > 1000, original.unit.code + 1, original.unit.code)) -> df

  df$new_unit <- unit.ref[match(df$division_unit,table = unit.ref$unit.code),"unit.base"]
  df$processed <- paste(df$division_value,df$new_unit)
  #df <- df %>%
  #  select(-value,-unit,-original.unit.code,-division_unit)
  return(df$processed)
  #print(df)
  #print(stringr::str_split(string = df_unit,pattern = "g|l"))
  #paste(df_value,df_unit)
}



#test.df <- data.frame(value = c(1024,1564,900),
#                      unit = c("ul","ml","pg"),
#                      arbi = c("a","b","c"))
#test.df
#test.df$final <- unit.convert(df = test.df,value = "value",unit = "unit")
#test.df

