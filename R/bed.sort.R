bed.sort <- function(bed_file_path,output_path){
  bedfile <- read.delim(file = bed_file_path,header = F,sep = "\t",quote = "",stringsAsFactors = F)
  bedfile <- bedfile[order(bedfile[,1],bedfile[,2]),]
  #print(bedfile)
  write.table(x = bedfile,file = output_path,append = F,quote = F,sep = "\t",row.names = F,col.names = F)
}

#bed.sort(bed_file_path = "R/locations.bed",output_path = "R/locations_sorted.bed")

