
#' In vitro Tox Experiment Planner
#'
#' Estimator for planning the number of plates and the conditions required, based on what is provided.
#'
#' @param cell.type Vector of strings containing the cell types.
#' @param treatment vector of strings containing the treatments.
#' @param time Vector of strings containing the treatment times.
#' @param bio_rep_number integer, how many biological replicates?
#' @export
#'
#' @return returns a dataframe.
#' @examples
#' # tox_in_vitro_planner(cell.type = c("HeLa","Other"),treatment = c("DMSO","compound A","compound B"),time = c("2 hours","4 hours"),bio_rep_number = 3)
tox_in_vitro_planner <- function(cell.type,treatment,time,bio_rep_number){
  sample_number = length(cell.type)*length(treatment)*length(time)*bio_rep_number
  df <- data.frame(cell.type = rep(cell.type,each = sample_number / length(cell.type)),
                   treatment = rep(treatment,each = bio_rep_number),
                   time = rep(time,each = length(treatment)*bio_rep_number),
                   bio_rep = rep(seq(1:bio_rep_number)))

  colnames(df) <- c("Cell Type","Treatments","Time","Bio. Replicate")

  return(df)
}


