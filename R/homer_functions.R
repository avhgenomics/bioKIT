
#' annotatePeaks.pl
#'
#' builds a command to run Homer's annotatePeaks.pl with a set of commonly used flags.
#'
#' @param bedfile The full path to the bed file
#' @param genome the genome to use; "mm10" is default.
#' @param specific_motifs Search for specific motifs? Takes a vector; adds the "-m" flag.
#' @param mscore Requires specific_motifs; returns log-odds score. logical.
#' @param mbed return specific locations of the motifs found.  argument takes the string of the full path to the output bed file.
#' @param center Center on a motif? argument takes the string of the full path to the output bed file.
#' @param annStats Returns a detailed set of annotation statistics. argument takes the string of the full path to the output txt file.
#' @param outputfile Full path to the annotated output text file.
#' @param run_as_sys run this command through system? False by default, returning the command string.
#' @param win_sys Set to False; this enables the system command to work properly on windows.
#'
#' @return By default, this returns a string of the compiled command.  If run_as_sys == T, this will run the command as well.
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' # annotatePeaks.pl(bedfile = "example.bed",genome = "mm10",outputfile = "annotated_output.txt")


annotatePeaks.pl <- function(bedfile,genome = "mm10",specific_motifs = NULL,mscore = F,mbed = NULL,center = NULL,annStats = NULL,outputfile,run_as_sys = F,win_sys = F){
  annotate.cmd <- paste("annotatePeaks.pl",bedfile,genome)

  if(!is.null(specific_motifs)){
    motif.set <- paste(specific_motifs,collapse = " ")
    annotate.cmd <- paste(annotate.cmd,"-m",motif.set)
    if(mscore == T){
      annotate.cmd <- paste(annotate.cmd,"-mscore")
    }
    if(!is.null(mbed)){
      annotate.cmd <- paste(annotate.cmd,"-mbed",mbed)
    }
  }

  if(!is.null(center)){
    annotate.cmd <- paste(annotate.cmd,"-center",center)
  }
  if(!is.null(annStats)){
    annotate.cmd <- paste(annotate.cmd,"-annStats",annStats)
  }


  annotate.cmd <- paste(annotate.cmd,">",outputfile)

  if(run_as_sys == T){
    if(win_sys == T){
      system("cmd.exe",input = annotate.cmd,wait = T)
    } else {
      system(annotate.cmd,wait = T)
    }
  } else {
    return(annotate.cmd)
    }


}


#' Split Motifs
#'
#' This takes an annotated txt file from annotatePeaks.pl (after loading as a df) and performs minor cleaning functions on the annotated motif set.  This currently works for 1 motif at a time.
#'
#' @param annotated_df the annotated df.
#' @param motif_col the name of the motif column as a string.
#' @param zero_based Are the locations 0-based or 1 based? logical.
#' @param filter_for_motifs Remove any peaks that do not contain a motif? Logical.
#' @param motif_label Write a label to fill in for the motif.
#' @param return_bedfile_format Return the motifs in a bed-style structure? Requires filter_for_motifs to be true.
#' @import tidyverse
#' @export
#' @return returns a df.
#' @examples
#' # ADD_EXAMPLES_HERE
#'
split_motifs <- function(annotated_df,motif_col,zero_based = F,filter_for_motifs = F,motif_label = NULL,return_bedfile_format = F){
  colnames(annotated_df)[1] = "PeakID"

  annotated_df %>%
    separate_rows(motif_col,sep = "\\)\\,") %>%
    separate(motif_col,into = c("motif_Distance","motif_sequence"),sep = "\\(") %>%
    separate(motif_sequence,into = c("motif_sequence","motif_strand","motif_conservation"),sep = ",") -> annotated_df


  annotated_df$motif_Distance <- as.numeric(annotated_df$motif_Distance)
  annotated_df$motif_Chr <- annotated_df$Chr
  annotated_df$motif_Start <- annotated_df$Start + annotated_df$motif_Distance
  if(zero_based == T){
    annotated_df$motif_Start <- annotated_df$motif_Start-1
  }
  annotated_df$motif_End <- annotated_df$motif_Start + nchar(annotated_df$motif_sequence)
  annotated_df[which(!is.na(annotated_df$motif_sequence)),"motif_label"] <- motif_label

  if(filter_for_motifs == T){
    annotated_df <- annotated_df[which(!is.na(annotated_df$motif_sequence)),]
    if(return_bedfile_format == T){
      annotated_df %>%
        mutate(motif_annotation = paste0(motif_sequence,";",motif_label)) %>%
        select(motif_Chr,motif_Start,motif_End,motif_strand,motif_annotation) -> annotated_df
    }
  }


  return(annotated_df)
}
