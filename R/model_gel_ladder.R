
#' Model Gel Ladder
#'
#' provides a model to predict base pair sizes from a ladder.
#'
#' @param ladder_bp_size size of the band. (100, 200, etc)
#' @param distance_migrated distance measurement for how far the band moved.
#' @param polynomial_number what polynomial to use for the model? default as 2.
#' @export
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
model_gel_ladder <- function(ladder_bp_size,distance_migrated,polynomial_number = 2){
  gel_model <- lm(ladder_bp_size ~ poly(distance_migrated,polynomial_number,raw = T))
  return(gel_model)
}


#ladder.set <- data.frame(ladder = c(100,200,300,400,500,600,700,800,900,1000,1500),
#                         distance.px = c(510,499,473,458,431,407,392,378,366,357,312))

#model <- model_gel_ladder(ladder_bp_size = ladder.set$ladder,distance_migrated = ladder.set$distance.px,polynomial_number = 2)
#broom::tidy(model)
#broom::glance(model)
