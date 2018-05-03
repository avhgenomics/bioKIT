
#' Calculate Growth Rate
#'
#' For cell culture.  Calculates the growth rate and doubling time of cells based on hours passed, seeding number, and current count.
#'
#' @param current_number number of cells counted at the end; integer.
#' @param starting_number number of cells seeded. integer.
#' @param time.hours number of hours that passed between seeding and collection; integer.
#' @export
#' @return returns a dataframe.
#' @examples
#' # ADD_EXAMPLES_HERE
#'
calculate_growth_rate <- function(current_number,starting_number,time.hours){
  growth_rate = log(current_number / starting_number) / time.hours

  dbltime = log(2)/growth_rate

  values.list <- data.frame(growth_rate = growth_rate,
                      doubling_time = dbltime)

  return(values.list)
}

#calculate_growth_rate(current_number = 100,starting_number = 50,time.hours = 18)


est_cells_from_confluency
