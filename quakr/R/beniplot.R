#' Plots earthquakes of depth over a cross-section perpendicular to north
#' 
#' \code{beniplot} creates a plot of a cross-sectional transect in either kilometers or longitude versus depth in kilometers. The reference of the transect is perpendicular to north. Therefore, if the fault is not perfectly oriented north-south, then it will be obliquely transected. Use \code{adj_beniplot} funtion to account for the strike of the fault. Outputs can be in longitude or kilometers and toggle between showing relative magnitude of events.
#' 
#' @param data a dataframe containing at least latitude and longitude of earthquakes as well as the magnitude and depth of the earthquakes. This function was originally designed to work with .csv data exported from https://earthquake.usgs.gov/earthquakes/search/
#' @param units Two units \code{"lon"} and \code{"km"} can be chosen. \code{"lon"} puts the transect in terms of longitude while \code{"km"} converts the longitude to kilometers.
#' @param magnitude if \code{magnitude = TRUE} a plot showing a transect in km on the x-axis with the y-axis including the earthquake depths categorized in color and size relating depth and magnitude of events is printed. If \code{magnitude = FALSE}, a plot of the transect in km and depth in km is printed showing an increase in depth with a gradation of color.
#' @return A plot of earthquakes is printed with an x-axis of the transect in kilometers and the y-axis is depth of earthquake in km. A gradation of color illustrates the increase in depth. If the default \code{magnitude = FALSE} is changed to \code{magnitude = TRUE}, then magnitude of events are shown in correlation with depth.
#' @examples 
#' beniplot(data = chile, units = "km", magnitude = TRUE)
#' beniplot(data = dataframe, units = "lon", magnitude = TRUE)
#' @export

# Create the beni_plot() function
beniplot <- function(data, units = "lon", magnitude = FALSE){
  
  #print plot of longitude and depth
  if(units == "lon" && magnitude == FALSE){
    lon.depth <- ggplot(data = data, aes(x = longitude, y = depth))+
      geom_point(aes(colour = -depth))+
      scale_x_continuous(position = "top")+
      scale_y_continuous(trans = "reverse")+
      theme_minimal()+
      ylab("Depth (km)")+
      xlab("Longitude")+
      scale_color_continuous(high = "#99CC00", low = "#003300")
    warning("You may be looking at your data obliquely. use adj_beniplot() to account for strike")
    print(lon.depth)
    
    #print plot of longitude and magnitude
  } else if(units == "lon" && magnitude == TRUE){
    lon.mag <- ggplot(data = data, aes(x = longitude, y = depth, size = mag, color = mag))+
      geom_point(shape = 19)+
      scale_x_continuous(position = "top")+
      scale_y_continuous(trans = "reverse")+
      theme_minimal()+
      ylab("Depth (km)")+
      xlab("Longitude")+
      scale_color_continuous(low = "#FFCC00", high = "#CC0000")
    warning("You may be looking at your data obliquely. use adj_beniplot() to account for strike")
    print(lon.mag)
    
    #print plot of km cross section and depth
  } else if(units == "km" && magnitude == FALSE){
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
    
    km.depth <- ggplot(data = data, aes(x = x, y = depth))+
      geom_point(aes(colour = -depth))+
      scale_x_continuous(position = "top")+
      scale_y_continuous(trans = "reverse")+
      theme_minimal()+
      ylab("Depth (Km)")+
      xlab("Cross-section (Km)")+
      scale_color_continuous(high = "#99CC00", low = "#003300")
    warning("You may be looking at your data obliquely. use adj_beniplot() to account for strike")
    print(km.depth)
    
    #print plot of km cross section and magnitude
  } else if(units == "km" && magnitude == TRUE){
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
    km.mag <- ggplot(data = data, aes(x = x, y = depth, size = mag, color = mag))+
      geom_point(shape = 19)+
      scale_x_continuous(position = "top")+
      scale_y_continuous(trans = "reverse")+
      theme_minimal()+
      ylab("Depth (Km)")+
      xlab("Cross-section (Km)")+
      scale_color_continuous(low = "#FFCC00", high = "#CC0000")
    warning("You may be looking at your data obliquely. use adj_beniplot() to account for strike")
    print(km.mag)
  }
  
}
