
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


dT2_bamCoverage <- function(){}


Required arguments
--bam, -b 	BAM file to process
Output
--outFileName, -o
Output file name.
--outFileFormat, -of


Possible choices: bigwig, bedgraph

Output file type. Either “bigwig” or “bedgraph”.
Optional arguments
--scaleFactor 	The computed scaling factor (or 1, if not applicable) will be multiplied by this.
--MNase 	Determine nucleosome positions from MNase-seq data. Only 3 nucleotides at the center of each fragment are counted. The fragment ends are defined by the two mate reads. Only fragment lengthsbetween 130 - 200 bp are considered to avoid dinucleosomes or other artifacts. By default, any fragments smaller or larger than this are ignored. To over-ride this, use the –minFragmentLength and –maxFragmentLength options, which will default to 130 and 200 if not otherwise specified in the presence of –MNase. NOTE: Requires paired-end data. A bin size of 1 is recommended.
--Offset 	Uses this offset inside of each read as the signal. This is useful in cases like RiboSeq or GROseq, where the signal is 12, 15 or 0 bases past the start of the read. This can be paired with the –filterRNAstrand option. Note that negative values indicate offsets from the end of each read. A value of 1 indicates the first base of the alignment (taking alignment orientation into account). Likewise, a value of -1 is the last base of the alignment. An offset of 0 is not permitted. If two values are specified, then they will be used to specify a range of positions. Note that specifying something like –Offset 5 -1 will result in the 5th through last position being used, which is equivalent to trimming 4 bases from the 5-prime end of alignments. Note that if you specify –centerReads, the centering will be performed before the offset.
--filterRNAstrand


Possible choices: forward, reverse

Selects RNA-seq reads (single-end or paired-end) originating from genes on the given strand. This option assumes a standard dUTP-based library preparation.
--version 	show program’s version number and exit
--binSize, -bs 	Size of the bins, in bases, for the output of the bigwig/bedgraph file.
--region, -r 	Region of the genome to limit the operation to - this is useful when testing parameters to reduce the computing time. The format is chr:start:end, for example –region chr10 or –region chr10:456700:891000.
--blackListFileName, -bl
A BED or GTF file containing regions that should be excluded from all analyses. Currently this works by rejecting genomic chunks that happen to overlap an entry. Consequently, for BAM files, if a read partially overlaps a blacklisted region or a fragment spans over it, then the read/fragment might still be considered. Please note that you should adjust the effective genome size, if relevant.
--numberOfProcessors, -p
Number of processors to use. Type “max/2” to use half the maximum number of processors or “max” to use all available processors.
--verbose, -v 	Set to see processing messages.
Read coverage normalization options
--effectiveGenomeSize
The effective genome size is the portion of the genome that is mappable. Large fractions of the genome are stretches of NNNN that should be discarded. Also, if repetitive regions were not included in the mapping of reads, the effective genome size needs to be adjusted accordingly. A table of values is available here: http://deeptools.readthedocs.io/en/latest/content/feature/effectiveGenomeSize.html .
--normalizeUsing


Possible choices: RPKM, CPM, BPM, RPGC

Use one of the entered methods to normalize the number of reads per bin. By default, no normalization is performed. RPKM = Reads Per Kilobase per Million mapped reads; CPM = Counts Per Million mapped reads, same as CPM in RNA-seq; BPM = Bins Per Million mapped reads, same as TPM in RNA-seq; RPGC = reads per genomic content (1x normalization); Mapped reads are considered after blacklist filtering (if applied). RPKM (per bin) = number of reads per bin / (number of mapped reads (in millions) * bin length (kb)). CPM (per bin) = number of reads per bin / number of mapped reads (in millions). BPM (per bin) = number of reads per bin / sum of all reads per bin (in millions). RPGC (per bin) = number of reads per bin / scaling factor for 1x average coverage. This scaling factor, in turn, is determined from the sequencing depth: (total number of mapped reads * fragment length) / effective genome size. The scaling factor used is the inverse of the sequencing depth computed for the sample to match the 1x coverage. This option requires –effectiveGenomeSize. Each read is considered independently, if you want to only count one mate from a pair in paired-end data, then use the –samFlagInclude/–samFlagExclude options.
--exactScaling 	Instead of computing scaling factors based on a sampling of the reads, process all of the reads to determine the exact number that will be used in the output. This requires significantly more time to compute, but will produce more accurate scaling factors in cases where alignments that are being filtered are rare and lumped together. In other words, this is only needed when region-based sampling is expected to produce incorrect results.
--ignoreForNormalization, -ignore
A list of space-delimited chromosome names containing those chromosomes that should be excluded for computing the normalization. This is useful when considering samples with unequal coverage across chromosomes, like male samples. An usage examples is –ignoreForNormalization chrX chrM.
--skipNonCoveredRegions, --skipNAs
This parameter determines if non-covered regions (regions without overlapping reads) in a BAM file should be skipped. The default is to treat those regions as having a value of zero. The decision to skip non-covered regions depends on the interpretation of the data. Non-covered regions may represent, for example, repetitive regions that should be skipped.
--smoothLength 	The smooth length defines a window, larger than the binSize, to average the number of reads. For example, if the –binSize is set to 20 and the –smoothLength is set to 60, then, for each bin, the average of the bin and its left and right neighbors is considered. Any value smaller than –binSize will be ignored and no smoothing will be applied.
Read processing options
--extendReads, -e
This parameter allows the extension of reads to fragment size. If set, each read is extended, without exception. NOTE: This feature is generally NOT recommended for spliced-read data, such as RNA-seq, as it would extend reads over skipped regions. Single-end: Requires a user specified value for the final fragment length. Reads that already exceed this fragment length will not be extended. Paired-end: Reads with mates are always extended to match the fragment size defined by the two read mates. Unmated reads, mate reads that map too far apart (>4x fragment length) or even map to different chromosomes are treated like single-end reads. The input of a fragment length value is optional. If no value is specified, it is estimated from the data (mean of the fragment size of all mate reads).
--ignoreDuplicates
If set, reads that have the same orientation and start position will be considered only once. If reads are paired, the mate’s position also has to coincide to ignore a read.
--minMappingQuality
If set, only reads that have a mapping quality score of at least this are considered.
--centerReads 	By adding this option, reads are centered with respect to the fragment length. For paired-end data, the read is centered at the fragment length defined by the two ends of the fragment. For single-end data, the given fragment length is used. This option is useful to get a sharper signal around enriched regions.
--samFlagInclude
Include reads based on the SAM flag. For example, to get only reads that are the first mate, use a flag of 64. This is useful to count properly paired reads only once, as otherwise the second mate will be also considered for the coverage.
--samFlagExclude
Exclude reads based on the SAM flag. For example, to get only reads that map to the forward strand, use –samFlagExclude 16, where 16 is the SAM flag for reads that map to the reverse strand.
--minFragmentLength
The minimum fragment length needed for read/pair inclusion. This option is primarily useful in ATACseq experiments, for filtering mono- or di-nucleosome fragments.
--maxFragmentLength
The maximum fragment length needed for read/pair inclusion.
