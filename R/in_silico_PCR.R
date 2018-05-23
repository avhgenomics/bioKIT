
#' in silico PCR
#'
#' Builds a request and parses the output of UCSC's in silico PCR.
#'
#' @param forward_primer string containing the forward primer sequence.
#' @param reverse_primer string containing the reverse primer sequence.
#' @param mRNA_primers Test the UCSC Genes assembly? False by default, tests the genome assembly.
#' @param genome_version mm10 by default, mm9 is also valid.
#' @param max_product_size max bp size for the product.  integer.
#' @param min_perfect_match minimum perfect match, typically left as is.
#' @param min_good_match minimum good match, typically left as is.
#' @param flip_reverse_primer flip the reverse primer? False by default.
#' @import rvest
#' @import xml2
#' @import tidyverse
#' @import stringr
#' @export
#' @return returns a string of the predicted amplification region(s).
#' @examples
#' #negative result
#' #in_silico_PCR(forward_primer = "AGCCTCCAAAACCAAGGTGA",reverse_primer = "ATCTGGTCAACCACCTCCTT")
#' #positive result
#'#in_silico_PCR(forward_primer = "TTCTCTGTCATGGAACCCTATTC",reverse_primer = "CAGGAGAGAGTAGTCCACAGTA")





in_silico_PCR <- function(forward_primer,reverse_primer,mRNA_primers = F,genome_version = "mm10",max_product_size = 4000,min_perfect_match = 15,min_good_match = 15,flip_reverse_primer = F){
  ucsc.url <- paste0("https://genome.ucsc.edu/cgi-bin/hgPcr?org=Mouse&db=",genome_version,"&wp_target=")
  if(mRNA_primers){
    ucsc.url <- paste0(ucsc.url,"mm10KgSeq10")
  } else {
    ucsc.url <- paste0(ucsc.url,"genome")
  }
  ucsc.url <- paste0(ucsc.url,"&wp_f=",forward_primer,"&wp_r=",reverse_primer,"&Submit=submit&wp_size=",max_product_size,"&wp_perfect=",min_perfect_match,"&wp_good=",min_good_match)
  if(flip_reverse_primer){
    ucsc.url <- paste0(ucsc.url,"&boolshad.wp_flipReverse=1")
  } else {
    ucsc.url <- paste0(ucsc.url,"&boolshad.wp_flipReverse=0")
  }

  url.link <- read_html(ucsc.url)

  url.link %>%
    html_nodes("pre") %>%
    html_text() %>%
    gsub(pattern = "\n",replacement = "",fixed = T) %>%
    str_extract(string = .,pattern = ">.+bp") %>%
    gsub(pattern = ">",replacement = "",fixed = T) -> pcr.product.region

  if(length(pcr.product.region) == 0){
    return("No predicted product")
  }
  if (length(pcr.product.region > 1) > 1){
    pcr.product.region <- paste(pcr.product.region,collapse = ";")
    return(pcr.product.region)
  } else {return(pcr.product.region)}

}
