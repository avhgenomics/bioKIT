
#' Predict Cell Number
#'
#' Predicts number of cells in culture based on seeding density, known doubling time information, and hours of incubation.
#'
#' @param seeding_density DESCRIPTION.
#' @param doubling_time DESCRIPTION.
#' @param incubation_hours DESCRIPTION.
#' @param return_text_statement DESCRIPTION.
#' @export
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
predict_cell_number <- function(seeding_density,doubling_time,incubation_hours,return_text_statement = F){
  cell_number <- round(seeding_density*(2^(incubation_hours/doubling_time)),0)
  if(return_text_statement){
  return(paste("estimated cell population after",incubation_hours,"hours:",cell_number))
    } else { return(cell_number)}
}

#predict_cell_number(seeding_density = 500000,doubling_time = 19.03041,incubation_hours = 48)



#' Estimate Confluency
#'
#' Estimates the confluency of a cell culture dish with a known (or predicted cell number), a specific plate size, and the number of cells that are considered 100% confluent on that dish.
#'
#' @param cell_number DESCRIPTION.
#' @param plate_size DESCRIPTION.
#' @param custom_confluency DESCRIPTION.
#' @export
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
estimate_confluency <- function(cell_number,plate_size = "100mm",custom_confluency = NULL){
  if(!is.na(custom_confluency)){
    max_cells = custom_confluency
  } else {max_cells = cell_culture_guidelines[cell_culture_guidelines$dishes == plate_size,"cells_at_confluency"]}

  surface_area = cell_culture_guidelines[cell_culture_guidelines$dishes == plate_size,"surface_area_cm2"]
  cells_per_cm2_confluent = max_cells / surface_area

  current_cells_per_cm2 = cell_number / surface_area

  confluency = current_cells_per_cm2 / cells_per_cm2_confluent

  return(confluency)
}

#estimate_confluency(5984000,custom_confluency = 6000000)
