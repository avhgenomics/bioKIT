


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

define_gel_lanes <- function(gel_img,x_inflect_threshold = 25,expand_lanes = 10,plot_signal = T){

  gel.matrix <- as.matrix(gel.img)
  gel.sums.X <- data.frame(colset = "X",pixel_col = seq(1:dim(gel.matrix)[1]),signal_sum = apply(X = gel.matrix,MARGIN = 1,FUN = function(x){sum(x)}))
  gel.sums.Y <- data.frame(colset = "Y",pixel_col = seq(1:dim(gel.matrix)[2]),signal_sum = apply(X = gel.matrix,MARGIN = 2,FUN = function(x){sum(x)}))
  gel.sums <- rbind(gel.sums.X,gel.sums.Y)
  gel.sums.x.inflect <- bioKIT::inflect(x = gel.sums.X$signal_sum,threshold = x_inflect_threshold)
  #gel.sums.y.inflect <- bioKIT::inflect(x = gel.sums.Y$signal_sum,threshold = y_inflect_threshold)

  x.minima.df <- data.frame(minima_px.x = gel.sums.x.inflect$minima)
  x.minima.df$differences <- c(diff(gel.sums.x.inflect$minima),NA)
  x.minima.df$lane.middle <- round(x.minima.df$minima_px.x+(x.minima.df$differences/2),0)
  x.minima.df$lane.start <- x.minima.df$lane.middle - expand_lanes
  x.minima.df$lane.end <- x.minima.df$lane.middle + expand_lanes
  x.minima.df$colset <- "X"

  if(plot_signal){
    plot.lanes <- ggplot(gel.sums,aes(x=pixel_col,y=signal_sum))
    plot(plot.lanes+
           geom_line()+
           geom_vline(data = x.minima.df,aes(xintercept = lane.start))+
           geom_vline(data = x.minima.df,aes(xintercept = lane.end))+
           facet_wrap(~colset,scales = "free",nrow = 2,ncol = 1))
  }

  return(x.minima.df)
}

#gel.img <- imager::load.image(file = "data_ignore/20180402_cutting_and_ligation_raw.jpg")

#lanes <- define_gel_lanes(gel_img = gel.img,x_inflect_threshold = 25,expand_lanes = 10,plot_signal = T)
