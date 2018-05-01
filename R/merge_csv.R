

#' Merge CSV Directory
#'
#' Takes a list.files object, then reads in each object and binds them.  Requires objects to be the same structure.
#'
#' @param csv_directory the object of the list.files function; full names suggested.
#' @param file.pattern a particular file object to search for. ex: "*.csv"
#' @param header do the files have a header?
#' @param sep what is the delimiter?
#' @param quote quote?
#' @param stringsAsFactors strings as factors?
#' @param fill fill NAs?
#' @param remove_char_quotes logical, adds a gsub to remove \" through regular expressions.
#' @export
#' @return Returns a df
#' @examples
#' # df <- merge_csv_dir(csv_directory = "./csv")
merge_csv_dir <- function(csv_directory,file.pattern = "*.csv",header = T, sep = ",",quote = "",stringsAsFactors = F,fill = T,remove_char_quotes = T){
  csv_files <- list.files(path = csv_directory,pattern = file.pattern,full.names = T)

  df <- do.call(rbind,lapply(csv_files,function(x) {read.csv(file = x,header = header,sep = sep,quote = quote,stringsAsFactors = stringsAsFactors,fill = fill)}))

  if(remove_char_quotes){
    df <- sapply(df, function(x) gsub("\"", "", x))
  }

  df <- as.data.frame(df)
  return(df)
}



