#' Local Reserve Map of Seasonal Kendall Results
#'
#' Create a stylized reserve level map to display seasonal kendall results
#'
#' @param nerr_site_id chr string of the reserve to make, first three characters used by NERRS
#' @param stations chr string of the reserve stations to include in the map
#' @param bbox a bounding box associated with the reserve
#' @param shp shape file
#' @param station_labs logical, should stations be labeled? Defaults to \code{TRUE}
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
#' @details Creates a base map of the US with options for including AK, HI, and PR. The user can also
#' This function was developed from a blog post by Bob Rudis (https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/)
#'
#' @examples
#' \dontrun{
#'
#' ##Just the national map
#'
#' ## a compact reserve
#'
#' ## a multicomponent reserve (show two different bounding boxes)
#' }
#'
#' @return returns a leaflet object. This function is intended to be used with mapshot to generate a png
#' for the reserve level report

res_sk_map <- function(nerr_site_id, stations, bbox, shp, station_labs = T, lab_loc = NULL, scale_pos = 'bottomleft') {

  # check that a shape file exists
  # check that legnth(lab) = length(stations)
  # check that the bb has the right dimensions

  #This loc stuff should be abstracted out
  loc <- get('sampling_stations')
  loc <- loc[(loc$Station.Code %in% stations), ]
  loc$abbrev <- substr(loc$Station.Code, start = 4, stop = 5)

  # add some logic to determine if r and l labs exist
  left_labs <- grep('L', lab_loc)
  right_labs <- grep('R', lab_loc)
  # then parse out m and "addmarkers" only if the RHS label/LHS label exist

  # NOTE you can add in conditional colors. Add a factor #add a column that assigns HEX color
  m <- leaflet(loc, options = leafletOptions(zoomControl = FALSE), width = 500, height = 500) %>%
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%  # Add default OpenStreetMap map tiles, CartoDB.Positron
    addPolygons(data = shp, weight = 2, color = '#B3B300', fillColor = 'yellow') %>%
    # left labs
    addCircleMarkers(lng = ~Longitude[left_labs] * -1, lat = ~Latitude[left_labs], radius = 5
                     , weight = 0, fillOpacity = 1, color = '#444e65'
                     , label = loc$abbrev[left_labs]
                     , labelOptions = labelOptions(noHide = TRUE, direction = c('left'))
    ) %>%
    #right labs
    addCircleMarkers(lng = ~Longitude[right_labs] * -1, lat = ~Latitude[right_labs], radius = 5
                     , weight = 0, fillOpacity = 1, color = '#444e65'
                     , label = loc$abbrev[right_labs]
                     , labelOptions = labelOptions(noHide = TRUE, direction = c('right'))
    ) %>%
    addScaleBar(position = scale_pos) %>%
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

  return(m)
}
