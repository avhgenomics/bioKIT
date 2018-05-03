


#' Define Gel Lanes
#'
#' This function uses the inflect function (bioKIT) to identify local minima in an agarose gel image.  From this set of minima, the middle of each lane can be defined.  This returns a range, expanded by a set of pixels on either side of the middle.
#'
#' @param gel_img an image object, created using the imager package.
#' @param inflect_threshold px threshold for calculating minima; integer, 25 by default.
#' @param expand_lanes how far in each direction from the midpoint should lanes be "expanded? 10 by default.
#' @param plot_signal Display the locations of the ranges? Useful for ensuring accuracy.
#' @export
#' @return Returns a dataframe of the calculated information.  Can also return a plot.
#' @examples
#' # ADD_EXAMPLES_HERE

define_gel_lanes <- function(gel_img,inflect_threshold = 25,expand_lanes = 10,plot_signal = T){

  gel.matrix <- as.matrix(gel.img)
  gel.sums.X <- data.frame(pixel_col = seq(1:dim(gel.matrix)[1]),signal_sum = apply(X = gel.matrix,MARGIN = 1,FUN = function(x){sum(x)}))

  gel.sums.x.inflect <- bioKIT::inflect(x = gel.sums.X$signal_sum,threshold = inflect_threshold)

  minima.df <- data.frame(minima_px.x = gel.sums.x.inflect$minima)
  minima.df$differences <- c(diff(gel.sums.x.inflect$minima),NA)
  minima.df$lane.middle <- round(minima.df$minima_px.x+(minima.df$differences/2),0)
  minima.df$lane.start <- minima.df$lane.middle - expand_lanes
  minima.df$lane.end <- minima.df$lane.middle + expand_lanes

  if(plot_signal){
    plot.lanes.x <- ggplot(gel.sums.X,aes(x=pixel_col,y=signal_sum))
    plot(plot.lanes.x+
           geom_line()+
           geom_vline(xintercept = minima.df$lane.start)+
           geom_vline(xintercept = minima.df$lane.end))
  }

  return(minima.df)
}


#define_gel_lanes(gel.img)
