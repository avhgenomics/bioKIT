#Hemocytometry-based cell count estimation.


#' count_cells
#'
#' count_cells() is a calculator for standard hemocytometry applications.
#'
#' @param sampleID Provides a label or name for the sample counted.
#' @param live The total number of live cells counted.
#' @param dead The total number of dead cells counted.
#' @param squares_counted How many of the outer corner-squares were counted, Default is 4.
#' @param sample_aliquot_vol The ul of sample taken for hemocytometry.
#' @param PBS_vol The ul of PBS or other solution that the sample aliquot is diluted with. Used to determine the dilution factor.
#' @param Trypan_blue_vol The ul of Trypan Blue, if used. A good starting point is usually 1:1 (sample_aliquot+PBS):Trypan Blue.
#' @param original_sample_vol How many ml is the original set of cells suspended in?
#'
#' @return returns a dataframe containing all of the calculations.
#' @examples
#' # count_cells(sampleID = "A",live = 300,dead = 10,squares_counted = 4,sample_aliquot_vol = 10,PBS_vol = 40,Trypan_blue_vol = 50,original_sample_vol = 6)
count_cells <- function(sampleID = NULL,live,dead = 0,squares_counted = 4,sample_aliquot_vol = 0,PBS_vol = 0,Trypan_blue_vol = 0,original_sample_vol){

  df <- data.frame(sampleID,live,dead,squares_counted,sample_aliquot_vol,PBS_vol,Trypan_blue_vol,original_sample_vol)
  df$total.cells.counted <- df$live + df$dead
  df$dilution.factor = (df$sample_aliquot_vol+df$PBS_vol+df$Trypan_blue_vol) / df$sample_aliquot_vol
  df$count.per.ml = (df$total.cells.counted*df$dilution.factor*10000) / df$squares_counted
  df$total.cells = df$count.per.ml*df$original_sample_vol
  df$viability =  paste(round((((df$live*df$dilution.factor*10000) / df$squares_counted) / df$count.per.ml)*100,2),"%")

  return(df)
}


