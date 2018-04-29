
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

