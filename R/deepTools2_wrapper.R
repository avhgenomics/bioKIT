
#' dT2 computeMatrix
#'
#' Wrapper for deepTools2 computeMatrix
#'
#' @param bigwig_files vector of bigwig filepaths
#' @param bed_files vector of bed filepaths
#' @param scale_regions use scale-regions mode?
#' @param ref_point if using reference-point mode; "TSS","TES",or "center"
#' @param bin_size size of the bins to use
#' @param upstream_bp how many bases upstream?
#' @param downstream_bp how many bases downstream
#' @param missingDataAsZero Treat missing data as zero?
#' @param skipZeros skip zeros?
#' @param blacklist_file use a blacklist file?
#' @param output_gz filepath to the output .gz file
#' @param outFileMatrix filepath to the output .tab file for underlying values
#' @param outSortedRegions filepath to a bed file for outputting sorted regions.
#' @param run_as_sys Return the command as a string or run as system
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
#' @param gz_file gz file from dT2_computeMatrix
#' @param output_file output file (.png, jpg, etc)
#' @param colorMap  The available options are: ‘Spectral’, ‘summer’, ‘coolwarm’, ‘Set1’, ‘Set2’, ‘Set3’, ‘Dark2’, ‘hot’, ‘RdPu’, ‘YlGnBu’, ‘RdYlBu’, ‘gist_stern’, ‘cool’, ‘gray’, ‘GnBu’, ‘gist_ncar’, ‘gist_rainbow’, ‘Wistia’, ‘CMRmap’, ‘bone’, ‘RdYlGn’, ‘spring’, ‘terrain’, ‘PuBu’, ‘spectral’, ‘gist_yarg’, ‘BuGn’, ‘bwr’, ‘cubehelix’, ‘YlOrRd’, ‘Greens’, ‘PRGn’, ‘gist_heat’, ‘Paired’, ‘hsv’, ‘Pastel2’, ‘Pastel1’, ‘BuPu’, ‘copper’, ‘OrRd’, ‘brg’, ‘gnuplot2’, ‘jet’, ‘gist_earth’, ‘Oranges’, ‘PiYG’, ‘YlGn’, ‘Accent’, ‘gist_gray’, ‘flag’, ‘BrBG’, ‘Reds’, ‘RdGy’, ‘PuRd’, ‘Blues’, ‘Greys’, ‘autumn’, ‘pink’, ‘binary’, ‘winter’, ‘gnuplot’, ‘RdBu’, ‘prism’, ‘YlOrBr’, ‘rainbow’, ‘seismic’, ‘Purples’, ‘ocean’, ‘PuOr’, ‘PuBuGn’, ‘nipy_spectral’, ‘afmhot’
#' @param dpi Image Resolution
#' @param run_as_sys Return the command as a string or run as system
#' @param outFileSortedRegions path to output bedfile for sorted regions
#' @param outFileNameMatrix path to output tab file for underlying data.
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




#' dT2 plot Profile
#'
#' Wrapper for deepTools2 plotProfile function.
#'
#' @param gz_file gz file from dT2_computeMatrix
#' @param output_file output file (.png, jpg, etc)
#' @param dpi Resolution of the image
#' @param run_as_sys Run the command as system.
#' @param outFileSortedRegions path to output bedfile for sorted regions
#' @param outFileNameData path to output tab file for underlying data.
#' @param perGroup calculate profiles as per group, rather than per sample.
#' @export
#' @return returns a command string, or runs the command and returns an image.
#' @examples
#' # ADD_EXAMPLES_HERE

dT2_plotProfile <- function(gz_file,output_file,dpi = 600,run_as_sys = F,outFileSortedRegions = NULL,outFileNameData = NULL,perGroup = F){
  profile.cmd <- "plotProfile -m"
  profile.cmd <- paste(profile.cmd,gz_file,"-o",output_file,"--dpi",dpi)

  if(!is.null(outFileSortedRegions)){
    profile.cmd <- paste(profile.cmd,"--outFileSortedRegions",outFileSortedRegions)
  }
  if(!is.null(outFileNameData)){
    profile.cmd <- paste(profile.cmd,"--outFileNameData",outFileNameData)
  }
  if(perGroup){

  }

  if(run_as_sys){
    system(profile.cmd,wait = T)
  } else {
    return(profile.cmd)
  }


}



#' dT2 bamCoverage
#'
#' Wrapper for deepTools2 bamCoverage; converts a bam file to a bigwig / bedgraph for use in visualization and analysis.
#'
#' @param bam_file DESCRIPTION.
#' @param output_file DESCRIPTION.
#' @param outFileFormat DESCRIPTION.
#' @param scaleFactor DESCRIPTION.
#' @param binSize DESCRIPTION.
#' @param region DESCRIPTION.
#' @param blacklist_file DESCRIPTION.
#' @param effectiveGenomeSize DESCRIPTION.
#' @param normalizeUsing DESCRIPTION.
#' @param smoothLength DESCRIPTION.
#' @param extendReads DESCRIPTION.
#' @param processors DESCRIPTION.
#' @param ignoreDuplicates DESCRIPTION.
#' @param run_as_sys DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
dT2_bamCoverage <- function(bam_file,output_file,outFileFormat = "bigwig",scaleFactor = NULL,binSize = 10,region = NULL,blacklist_file = NULL,effectiveGenomeSize = NULL, normalizeUsing = NULL,smoothLength = NULL,extendReads = NULL,processors = "max",ignoreDuplicates = T,run_as_sys = F){
  coverage.cmd <- paste("bamCoverage -b",bam_file,"-o",output_file,"-of",paste0("'",outFileFormat,"'"))

  if(!is.null(scaleFactor)){
    coverage.cmd <- paste(coverage.cmd,"--scaleFactor",scaleFactor)
  }
  if(!is.null(binSize)){
    coverage.cmd <- paste(coverage.cmd,"--binSize",binSize)
  }
  if(!is.null(region)){
    coverage.cmd <- paste(coverage.cmd,"--region",region)
  }
  if(!is.null(blacklist_file)){
    coverage.cmd <- paste(coverage.cmd,"--blackListFileName",blacklist_file)
  }
  if(!is.null(effectiveGenomeSize)){
    coverage.cmd <- paste(coverage.cmd,"--effectiveGenomeSize",effectiveGenomeSize)
  }
  if(!is.null(normalizeUsing)){
    coverage.cmd <- paste(coverage.cmd,"--normalizeUsing",normalizeUsing)
  }
  if(!is.null(smoothLength)){
    coverage.cmd <- paste(coverage.cmd,"--smoothLength",smoothLength)
  }
  if(!is.null(extendReads)){
    coverage.cmd <- paste(coverage.cmd,"--extendReads",extendReads)
  }
  if(!is.null(processors)){
    coverage.cmd <- paste(coverage.cmd,"--numberOfProcessors",processors)
  }
  if(ignoreDuplicates){
    coverage.cmd <- paste(coverage.cmd,"--ignoreDuplicates")
  }

  if(run_as_sys){
    system(coverage.cmd,wait = T)
  } else {
    return(coverage.cmd)
  }
}

#dT2_bamCoverage(bam_file = "test",output_file = "testout",ignoreDuplicates = T)
