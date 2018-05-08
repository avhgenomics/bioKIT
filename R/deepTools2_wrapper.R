
#' dT2 computeMatrix
#'
#' Wrapper for deepTools2 computeMatrix
#'
#' @param bigwig_files DESCRIPTION.
#' @param bed_files DESCRIPTION.
#' @param scale_regions DESCRIPTION.
#' @param ref_point DESCRIPTION.
#' @param bin_size DESCRIPTION.
#' @param upstream_bp DESCRIPTION.
#' @param downstream_bp DESCRIPTION.
#' @param missingDataAsZero DESCRIPTION.
#' @param skipZeros DESCRIPTION.
#' @param blacklist_file DESCRIPTION.
#' @param output_gz DESCRIPTION.
#' @param outFileMatrix DESCRIPTION.
#' @param outSortedRegions DESCRIPTION.
#' @param run_as_sys DESCRIPTION.
#' @export
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
dT2_computeMatrix <- function(bigwig_files,bed_files,scale_regions = F,ref_point = "TSS",bin_size = 10,upstream_bp = 1,downstream_bp = 1,missingDataAsZero = T, skipZeros = T,blacklist_file = NULL,output_gz,outFileMatrix = NULL,outSortedRegions = NULL,run_as_sys = F){

  compute.cmd <- "computeMatrix"
  if(scale_regions){
    compute.cmd <- paste(compute.cmd,"scale-regions")
  } else {
    compute.cmd <- paste(compute.cmd,"reference-point --referencePoint",paste0("'",ref_point,"'"))
  }


  bw_string <- paste0(bigwig_files,collapse = " ")
  bed_string <- paste0(bed_files,collapse = " ")

  compute.cmd <- paste(compute.cmd,"-S",bw_string,"-R",bed_string,"-a",downstream_bp,"-b",upstream_bp,"-o",output_gz,"--binSize",bin_size)
  if(missingDataAsZero){
    compute.cmd <- paste(compute.cmd,"--missingDataAsZero")
  }
  if(skipZeros){
    compute.cmd <- paste(compute.cmd,"--skipZeros")
  }
  if(!is.null(outFileMatrix)){
    compute.cmd <- paste(compute.cmd,"--outFileNameMatrix",outFileMatrix)
  }
  if(!is.null(outSortedRegions)){
    compute.cmd <- paste(compute.cmd,"--outFileSortedRegions",outSortedRegions)
  }
  if(!is.null(blacklist_file)){
    compute.cmd <- paste(compute.cmd,"--blackListFileName",blacklist_file)
  }

  if(run_as_sys){
    system(compute.cmd,wait = T)
  } else {

    return(compute.cmd)
  }

}



#' dT2 plot Heatmap
#'
#' Wrapper for deepTools2 plotHeatmap command.
#'
#' @param gz_file DESCRIPTION.
#' @param output_file DESCRIPTION.
#' @param colorMap DESCRIPTION.
#' @param dpi DESCRIPTION.
#' @param run_as_sys DESCRIPTION.
#' @param outFileSortedRegions DESCRIPTION.
#' @param outFileNameMatrix DESCRIPTION.
#' @export
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
dT2_plotHeatmap <- function(gz_file,output_file,colorMap = "viridis",dpi = 600,run_as_sys = F,outFileSortedRegions = NULL,outFileNameMatrix = NULL){
  colorMap_string <- paste0("'",colorMap,"'")
  heatmap.cmd <- "plotHeatmap -m"
  heatmap.cmd <- paste(heatmap.cmd,gz_file,"-o",output_file,"--colorMap",colorMap_string,"--dpi",dpi)

  if(!is.null(outFileSortedRegions)){
    heatmap.cmd <- paste(heatmap.cmd,"--outFileSortedRegions",outFileSortedRegions)
  }
  if(!is.null(outFileNameMatrix)){
    heatmap.cmd <- paste(heatmap.cmd,"--outFileNameMatrix",outFileNameMatrix)
  }

  if(run_as_sys){
    system(heatmap.cmd,wait = T)
  } else {
    return(heatmap.cmd)
  }


}
