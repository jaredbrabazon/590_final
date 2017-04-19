#' Rotate cartesian coordinates
#' 
#' \code{polar_transform} rotates cartesian coordinates about the origin at a given degree
#' 
#' @param data a dataframe containing cartesian coordinates labeled x and y
#' @param rotation The desired value of rotation in degrees
#' @param plots if \code{plots = TRUE} a plot showing both the original x, y data and the rotated data (in green) is printed. If \code{plots = FALSE}, a plot is not printed.
#' @return A dataframe is printed to the console containing original cartesian (x, y) values and the new rotated (new.x, new.y) values. If \code{plots = TRUE} a plot showing both the original and new coordinates (colored green) is plotted
#' @examples 
#' polar_transform(data = dataframe, rotation = 45, plots = TRUE)
#' @export

# create polar_transform() to rotate cartesian coordinates by converting to polar and rotating a given degree and finally converting back to cartesian
# give option to view plots
polar_transform <- function(data, rotation = 0, plots = TRUE){
  
  #perform a polar transformation from equations. Used equations from https://www.siggraph.org/education/materials/HyperGraph/modeling/mod_tran/2drota.htm
  #x' = x cos f - y sin f
  #y' = y cos f + x sin f
  #where x' and y' are new values, x and y are old values, f is the angle of rotation
  
  #convert inputted degrees to radians
  rot <- rotation*pi/180 
  
  #perform the rotation
  polar_trans <- data %>%
    mutate(new.x = x*cos(rot)-y*sin(rot)) %>%
    mutate(new.y = x*sin(rot)+y*cos(rot)) %>%
    select(new.x, new.y, x, y)
  #print(polar_trans)
  
  #note that the rotated values are in green on the plot
  if(plots == TRUE){
    rot.plot <- ggplot(data = polar_trans, aes(x, y))+ geom_point()
    rot.plot.new <- geom_point(data = polar_trans, aes(x = new.x, y = new.y), color = "green")
    rot.plot + rot.plot.new +scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
  } else return(data.frame(polar_trans))
}
