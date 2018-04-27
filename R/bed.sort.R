
#' Bed Sort
#'
#' Takes a bed file, sorts it by chromosome and start, then writes it to the output location.
#'
#' @param bed_file_path path to the unsorted bed file.
#' @param output_path path to the sorted bed file, the output.
#'
#' @return returns a bed file.
#' @examples
#' # bed.sort(bed_file_path = "R/locations.bed",output_path = "R/locations_sorted.bed")
bed.sort <- function(bed_file_path,output_path){
  bedfile <- read.delim(file = bed_file_path,header = F,sep = "\t",quote = "",stringsAsFactors = F)
  bedfile <- bedfile[order(bedfile[,1],bedfile[,2]),]
  #print(bedfile)
  write.table(x = bedfile,file = output_path,append = F,quote = F,sep = "\t",row.names = F,col.names = F)
  print(paste("wrote sorted bed file:",output_path))
}


