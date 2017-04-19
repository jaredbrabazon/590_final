#' Convert Lat/Lon to Kilometers
#' 
#' \code{geocoord_km} converts geocoordinates of latitude and longitude to kilometers. Projections of lat and lon are turned into (x,y) points centered around their mean in kilometers. Code from http://wiki.cbr.washington.edu/qerm/index.php/R/Converting_Geographic_Coordinates_to_Kilometers was used in making the function. See link for more information on calculation.
#' 
#' @param data a dataframe containing at least two vectors with geocoordinates labeled longitute and latitude
#' @return A dataframe is printed to the console containing new cartesian (x, y) values in kilometers along with the original latitude and longitude values.
#' @examples 
#' geocoord_km(data = dataframe)
#' @export

# create geocoord_km() to change a dataframe of lat and lon to x,y points centered around their mean in kilometers
# from http://wiki.cbr.washington.edu/qerm/index.php/R/Converting_Geographic_Coordinates_to_Kilometers
#outputs a dataframe of the km x y values with the lat long values
geocoord_km <- function(data){
  lon0 <- mean(range(data$longitude))
  lat0 <- mean(range(data$latitude))
  
  rx <- 6371 * acos(sin(lat0 *pi/180)^2 + cos(lat0*pi/180)^2 * cos((lon0+.5)*pi/180 - (lon0-.5)*pi/180))
  ry <- 6371 * acos(sin((lat0 -.5)*pi/180) *  sin((lat0+.5)*pi/180) + cos((lat0-.5)*pi/180) * cos((lat0+.5)*pi/180))
  
  x <-(data$longitude - lon0)*rx
  y <-(data$latitude - lat0)*ry
  
  #add to dataframe
  data$x <- x
  data$y <- y
  
  values <- data.frame(select(data, c(x, y, latitude, longitude)))
  #print(values)
}
