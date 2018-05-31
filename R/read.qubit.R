
#' Read Qubit CSV
#'
#' Read Qubit Results file.
#'
#' @param file File location. String
#' @param header Header? logical. T by default.
#' @param sep delimiter for import; String. "," by default
#' @param quote quote? String. "" by default.
#' @param skip How many rows to skip before reading.  Integer. 5 by default.
#' @importFrom utils read.csv
#' @export
#'
#' @return Returns a data frame.
#' @examples
#' #read.qubit_csv(file = "data-raw/Qubit2018-05-30_14-20-15_ChIP_CTCF_Pulldown.csv")

read.qubit_csv <- function(file,header = T,sep = ",",quote = "",skip = 5){
  df <- read.csv(file = file,header = header,sep = sep,quote = quote,skip = skip)

  df <- df[which(df[,2] != "NA"),which(colnames(df) != "X.2")]
  return(df)
}

