#' Plots earthquakes of depth over a cross-section with given strike
#' 
#' \code{adj_beniplot} creates a plot of a cross-sectional transect in kilometers versus depth in kilometers. The user defines the strike of the fault allowing a perpendicular transect to the fault. This is useful in defining Wadati-Benioff Zones on subducting slabs
#' 
#' @param data a dataframe containing at least latitude and longitude of earthquakes as well as the magnitude and depth of the earthquakes. This function was originally designed to work with .csv data exported from https://earthquake.usgs.gov/earthquakes/search/.
#' @param strike The strike of the fault of interest (in degrees from north)
#' @param magnitude if \code{magnitude = TRUE} a plot showing a transect in km of points rotated to the degree set in \code{strike} on the x-axis. The y-axis includes the earthquake depths categorized in color and size relating depth and magnitude of events. If \code{magnitude = FALSE}, a plot of the transect in km and depth in km is printed showing an increase in depth with a gradation of color.
#' @return A plot of earthquakes is printed with an x-axis of the transect in kilometers and the y-axis is depth of earthquake in km. A gradation of color illustrates the increase in depth. If the default \code{magnitude = FALSE} is changed to \code{magnitude = TRUE}, then magnitude of events are shown in correlation with depth.
#' @examples 
#' adj_beniplot(data = chile, strike = 13, magnitude = TRUE)
#' @export

#create adj_beniplot() function to plot strike adjusted data
adj_beniplot <- function(data, strike = 0, magnitude = FALSE){
  
  #conversion from lat/long to km, from http://wiki.cbr.washington.edu/qerm/index.php/R/Converting_Geographic_Coordinates_to_Kilometers
  #note that it returns the projections about an origin
  lon0 <- mean(range(data$longitude))
  lat0 <- mean(range(data$latitude))
  
  rx <- 6371 * acos(sin(lat0 *pi/180)^2 + cos(lat0*pi/180)^2 * cos((lon0+.5)*pi/180 - (lon0-.5)*pi/180))
  ry <- 6371 * acos(sin((lat0 -.5)*pi/180) *  sin((lat0+.5)*pi/180) + cos((lat0-.5)*pi/180) * cos((lat0+.5)*pi/180))
  
  x <-(data$longitude - lon0)*rx
  y <-(data$latitude - lat0)*ry
  
  #add to query dataframe
  data$x <- x
  data$y <- y
  
  #perform a polar transformation from equations
  #perform matrix algebra on 
  #|cosf -sinf| * |x| = |newx|
  #|sinf  cosf|   |y| = |newy|
  #x' = x cos f - y sin f
  #y' = y cos f + x sin f
  #where x' and y' are new values, x and y are old values, f is the angle of rotation
  rot <- strike*pi/180
  polar_transform <- data %>%
    mutate(new.x = x*cos(rot)-y*sin(rot)) %>%
    mutate(new.y = y*cos(rot)+x*sin(rot)) %>%
    select(new.x, new.y, mag, depth)
  
  #plot the depth with cross-sectional km (perpendicular to strike)
  if(magnitude == FALSE){
  km.depth.strike <- ggplot(polar_transform, aes(x = new.x, y = depth))+
    geom_point(aes(colour = -depth))+
    scale_x_continuous(position = "top")+
    scale_y_continuous(trans = "reverse")+
    theme_minimal()+
    ylab("Depth (Km)")+
    xlab("Cross-section (Km)")+
    scale_color_continuous(high = "#99CC00", low = "#003300")
  print(km.depth.strike)
  warning("Be sure to input the strike (degrees from north) of the fault to obtain a truly cross-sectional plot perpendicular to the fault")
  
  #plot the depth by cross-sectional km highlighting earthquake magnitude (perpendicular to strike)
  } else {
  km.mag.strike <- ggplot(polar_transform, aes(x = new.x, y = depth, size = mag, color = mag))+
    geom_point(shape = 19)+
    scale_x_continuous(position = "top")+
    scale_y_continuous(trans = "reverse")+
    theme_minimal()+
    ylab("Depth (Km)")+
    xlab("Cross-section (Km)")+
    scale_color_continuous(low = "#FFCC00", high = "#CC0000")
  print(km.mag.strike)
  warning("Be sure to input the strike (degrees from north) of the fault to obtain a truly cross-sectional plot perpendicular to the fault")
  }
}  
