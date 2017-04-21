#' Create a map displaying earthquake data
#' 
#' \code{mapull} looks through the dataframe inputted and pulls a map from google depending on the latitude and longitude in the dataframe. Earthquake depths and magnitudes are plotted in plan view on the map. This function uses the ggmap package.
#' 
#' @param data a dataframe containing the following earthquake values labeled as follows: latitude, longitude, depth, magnitude (labeled as mag). This data can easily be exported as a .csv from https://earthquake.usgs.gov/earthquakes/search/. This function was made to work specifically for the exported USGS data, however, it can work with properly labeled dataframes.
#' @param magnitude If \code{magnitude = TRUE}, magnitude of events are plotted on top of the pulled google map. If \code{magnitude = FALSE}, then earthquakes are plotted on top of the google map with a gradation of color indicating increased depth.
#' @return A google map with bounds containing the latitude and longitude of the dataframe with either the depths or magnitude of the earthquakes in the dataframe plotted on top of the map.
#' @examples 
#' mapull(data = dataframe, magnitude = TRUE)
#' @references D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2. The R, Journal, 5(1), 144-161. URL, http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
#' @export

#create the mapull function
mapull <- function(data, magnitude = FALSE){
  
  #define the box of latitudes and longitudes
  k <- make_bbox(lon = longitude, lat = latitude, data = data)
  
  #pull the map
  zone <- get_map(location = k)
  
  #plot the map with corresponding USGS quake data showing a gradation in color with depth
  if(magnitude == FALSE){
  map <- ggmap(zone) + geom_point(data = data, aes(x = longitude, y = latitude, color = -depth))+ 
    scale_color_continuous(high = "#99CC00", low = "#003300")
  print(map)
  
  #plot the map with corresponding USGS quake data showing gradation in color and size with magnitude of event
  } else { 
    map <- ggmap(zone) + geom_point(data = data, aes(x = longitude, y = latitude, color = mag, size = mag))+ 
      scale_color_continuous(low = "#FFCC00", high = "#CC0000")
    print(map)
  }

}

