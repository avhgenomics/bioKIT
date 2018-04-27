


#' Concentrate Cells
#'
#' concentrate_cells() is a simple function to estimate the volume of buffer required to resuspend cells at a certain density / ul.
#'
#' @param total_cell_number The total number of cells in a pellet or sample.
#' @param desired_conc_per_ul the density of cells per ul desired. ex: 10000 (10000 / ul).  Also, can be used creatively: 10000/20 would be 10000 cells per 20 ul.
#'
#' @return Returns a string that contains the volume required in ul.
#' @examples
#' # concentrate_cells(total_cell_number = 1000000,desired_conc_per_ul = 1000/5)
concentrate_cells <- function(total_cell_number,desired_conc_per_ul){
  required_ul = paste((total_cell_number/desired_conc_per_ul),"ul")
  return(required_ul)
}

