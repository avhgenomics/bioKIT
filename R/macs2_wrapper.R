
#' macs2 callpeak
#'
#' Wrapper for macs2 peak calling
#'
#' @param treated_bam_file DESCRIPTION.
#' @param control_bam_file DESCRIPTION.
#' @param experiment_name DESCRIPTION.
#' @param output_folder DESCRIPTION.
#' @param file_format DESCRIPTION.
#' @param genome DESCRIPTION.
#' @param pvalue DESCRIPTION.
#' @param qvalue DESCRIPTION.
#' @param nomodel DESCRIPTION.
#' @param extsize DESCRIPTION.
#' @param run_as_sys DESCRIPTION.
#' @export
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
macs2_callpeak <- function(treated_bam_file,control_bam_file,experiment_name,output_folder,file_format = "AUTO",genome = "mm",pvalue = NULL, qvalue = 0.05,nomodel = T,extsize = NULL,run_as_sys = F){

  if(dir.exists(output_folder) == F){
    dir.create(path = output_folder)
  }
  callpeak.cmd <- paste("macs2 callpeak -t",treated_bam_file,"-c",control_bam_file,"--name",experiment_name,"--outdir",output_folder,"-f",file_format,"-g",genome)


  if(!is.null(pvalue)){
    callpeak.cmd <- paste(callpeak.cmd,"--pvalue",pvalue)
  } else {
    callpeak.cmd <- paste(callpeak.cmd,"--qvalue",qvalue)
  }

  if(nomodel){
    callpeak.cmd <- paste(callpeak.cmd,"--nomodel")
    if(!is.null(extsize)){
      callpeak.cmd <- paste(callpeak.cmd,"--extsize",extsize)
    }
  }

  if(run_as_sys){
    system(callpeak.cmd,wait = T)
  }
  else{
    return(callpeak.cmd)
  }
}

#macs2_callpeak(treated_bam_file = "test",control_bam_file = "test",experiment_name = "AAA",output_folder = "b/",nomodel = T,extsize = 200,run_as_sys = F)
