#' Local Reserve Map for Custom Stations
#'
#' Create a stylized reserve-level map of custom station locations for use with the reserve level reporting template
#'
#' @param stations chr string of the reserve stations to include in the map
#' @param x_loc num vector of x coordinates for \code{stations}
#' @param y_loc num vector of y coordinates for \code{stations}
#' @param bbox a bounding box associated with the reserve. Must be in the format of c(X1, Y1, X2, Y2)
#' @param shp SpatialPolygons object
#' @param station_labs logical, should stations be labeled? Defaults to \code{TRUE}
#' @param station_col chr vector of colors used to color station points. Defaults to 'black'.
#' @param lab_loc chr vector of 'R' and 'L', one letter for each station. if no \code{lab_loc} is specified then labels will default to the left.
#' @param scale_pos scale_pos where should the scale be placed? Options are 'topleft', 'topright', 'bottomleft', or 'bottomright'. Defaults to 'bottomleft'
#'
#' @import leaflet
#'
#' @importFrom ggthemes theme_map
#' @importFrom magrittr "%>%"
#' @importFrom maptools elide spRbind unionSpatialPolygons
#' @importFrom rgdal readOGR
#' @importFrom rlang .data
#' @importFrom sp CRS bbox proj4string spTransform
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Creates a stylized, reserve-level base map. The user can specify the reserve and stations to plot. The user can also specify a bounding box. For multi-component reserves, the user should specify a bounding box that highlights the component of interest.
#'
#' This function does not automatically detect conflicts between station labels. The \code{lab_loc} argument allows the user to specify "R" or "L" for each station to prevent labels from conflicting with each other.
#'
#' This function is intended to be used with \code{mapview::mapshot} to generate a png for the reserve-level report.
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return Returns a leaflet object
#'
#' @examples
#' ### set plotting parameters
#' stns <- c('custom stn 1', 'custom stn 2')
#' x_coords <- c(-121.735281, -121.750369)
#' y_coords <- c(36.850377, 36.806667)
#' shp_fl <- elk_spatial
#' bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
#' lab_dir <- c('L', 'R')
#' pos <- 'bottomleft'
#'
#' ### plot
#' res_custom_map(stations = stns, x_loc = x_coords, y_loc = y_coords,
#' bbox = bounding_elk, lab_loc = lab_dir, scale_pos = pos, shp = shp_fl)
#'
#' \donttest{
#' res_custom_map(stations = stns, x_loc = x_coords, y_loc = y_coords,
#' bbox = bounding_elk, lab_loc = lab_dir, scale_pos = pos,
#' shp = shp_fl, station_col = c('red', 'green'))
#' }
#'
res_custom_map <- function(stations
                           , x_loc
                           , y_loc
                           , bbox
                           , shp
                           , station_labs = TRUE
                           , station_col = NULL
                           , lab_loc = NULL
                           , scale_pos = 'bottomleft') {

  # check that a shape file exists
  if(class(shp) != 'SpatialPolygons')
    stop('shapefile (shp) must be SpatialPolygons object')

  # check that stations were specified
  if(is.null(stations))
    stop('No stations were specified. Specify stations to be mapped.')

  # check that length(lab_loc) = length(stations)
  if(!is.null(station_labs) && length(lab_loc) != length(stations))
    stop('Incorrect number of label location identifiers specified. R or L designation must be made for each station.' )

  # check that the bb has the right dimensions
  if(is.null(bbox))
    stop('Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  if(length(bbox) != 4)
    stop('Incorrect number of elements specified for bbox. Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')

  #check that stations, x_loc, and y_loc match
  if(length(stations) != length(x_loc))
    stop('An incorrect number of x coordinates were specified. One x coordinate must be specified for each station')
  if(length(stations) != length(y_loc))
    stop('An incorrect number of y coordinates were specified. One y coordinate must be specified for each station')

  if(is.null(station_col)) {
    station_col <- 'black'
  }

  # set map label styles
  label_style <- list(
    "box-shadow" = "none",
    "border-radius" = "5px",
    "font" = "bold 16px/1.5 'Helvetica Neue', Arial, Helvetica, sans-serif",
    "padding" = "1px 5px 1px 5px"
  )

  # generate location labels
  loc <- data.frame(abbrev = stations, Latitude = y_loc, Longitude = -1 * x_loc, color = station_col, stringsAsFactors = FALSE)

  # Determine if r and l labs exist
  if(!is.null(lab_loc)){
    if('L' %in% lab_loc){left_labs <- grep('L', lab_loc)}
    if('R' %in% lab_loc){right_labs <- grep('R', lab_loc)}
  } else {
    #default to left labels
    left_labs <- c(1:4)
  }

  # Plot map
  m <- leaflet(loc, options = leafletOptions(zoomControl = FALSE), width = 500, height = 500) %>%
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%  # Add default OpenStreetMap map tiles, CartoDB.Positron
    addPolygons(data = shp, weight = 2, color = '#B3B300', fillColor = 'yellow')

  if(exists('left_labs')){
    m <- m %>%
      addCircleMarkers(lng = ~Longitude[left_labs] * -1, lat = ~Latitude[left_labs], radius = 5
                       , weight = 0, fillOpacity = 1
                       , color = loc$color[left_labs]
                       , label = loc$abbrev[left_labs]
                       , labelOptions = labelOptions(noHide = station_labs
                                                     , direction = c('left')
                                                     , opacity = 1
                                                     , offset = c(-5, 0)
                                                     , style = label_style))
  }

  if(exists('right_labs')){
    m <- m %>%
      addCircleMarkers(lng = ~Longitude[right_labs] * -1, lat = ~Latitude[right_labs], radius = 5
                       , weight = 0, fillOpacity = 1
                       , color = loc$color[right_labs]
                       , label = loc$abbrev[right_labs]
                       , labelOptions = labelOptions(noHide = station_labs
                                                     , direction = c('right')
                                                     , opacity = 1
                                                     , offset = c(5, 0)
                                                     , style = label_style))
  }

  m <- m %>%
    addScaleBar(position = scale_pos) %>%
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

  return(m)
}
