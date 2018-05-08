
#' Make Cut Matrix
#'
#' Basic Wrapper for ATACTK: Makes a cut matrix based on the input criteria; currently returns a string with the command to run in terminal.
#'
#' @param aggregate.setting DESCRIPTION.
#' @param discrete.setting DESCRIPTION.
#' @param bins DESCRIPTION.
#' @param resolution DESCRIPTION.
#' @param bam_file DESCRIPTION.
#' @param bed_file DESCRIPTION.
#' @param output_file DESCRIPTION.
#' @export
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
make_cut_matrix_atactk <- function(aggregate.setting = T,discrete.setting = F, bins = c("1-100","180-1000"),resolution = 1,bam_file,bed_file,output_file){

  bins.string <- paste0(bins,collapse = " ")
  bins.string <- paste0("'(",bins.string," ",resolution,")'")
  print(bins.string)

  atactk.cmd <- paste("make_cut_matrix")
  if(aggregate.setting){
    atactk.cmd <- paste(atactk.cmd,"-a","-b")
  }
  if(discrete.setting){
    atactk.cmd <- paste(atactk.cmd,"-d","-b")
  }
  atactk.cmd <- paste(atactk.cmd,bins.string, "-p 8",bam_file,bed_file,"|","gzip -c >",output_file)

  return(atactk.cmd)
}

#make_cut_matrix_atactk(aggregate.setting = T,discrete.setting = F,resolution = 1,bam_file = "test",bed_file = "test",output_file = "outtest")
