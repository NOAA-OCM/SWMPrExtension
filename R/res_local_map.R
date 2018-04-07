#' Local Reserve Map
#'
#' Create a stylized reserve-level map for use with the reserve level reporting template
#'
#' @param nerr_site_id chr string of the reserve to make, first three characters used by NERRS
#' @param stations chr string of the reserve stations to include in the map
#' @param bbox a bounding box associated with the reserve. Must be in the format of c(X1, Y1, X2, Y2)
#' @param shp SpatialPolygons object
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
#' @return returns a leaflet object
#'
#' @examples
#' ## a compact reserve
#' ### set plotting parameters
#' stations <-
#' sampling_stations[(sampling_stations$NERR.Site.ID == 'elk'
#' & sampling_stations$Status == 'Active'), ]$Station.Code
#' to_match <- c('wq', 'met')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- elk_spatial
#' bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
#' lab_dir <- c('L', 'R', 'L', 'L', 'L')
#' labs <- c('ap', 'cw', 'nm', 'sm', 'vm')
#' pos <- 'bottomleft'
#'
#' ### plot
#' res_local_map('elk', stations = stns, bbox = bounding_elk,
#' lab_loc = lab_dir, scale_pos = pos, shp = shp_fl)
#'
#' \dontrun{
#' ## a multicomponent reserve (show two different bounding boxes)
#' ### set plotting parameters
#' stations <-
#' sampling_stations[(sampling_stations$NERR.Site.ID == 'cbm'
#' & sampling_stations$Status == 'Active'), ]$Station.Code
#' to_match <- c('wq', 'met')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- cbm_spatial
#' bounding_cbm_1 <- c(-77.393, 39.741, -75.553, 38.277)
#' bounding_cbm_2 <- c(-76.862006, 38.811571, -76.596508, 38.642454)
#' lab_dir <- c('L', 'R', 'L', 'L', 'L')
#' labs <- c('ap', 'cw', 'nm', 'sm', 'vm')
#' pos <- 'bottomleft'
#'
#' ### plot
#' res_local_map('cbm', stations = stns, bbox = bounding_cbm_1,
#' lab_loc = lab_dir, scale_pos = pos, shp = shp_fl)
#'
#' res_local_map('cbm', stations = stns, bbox = bounding_cbm_2,
#' lab_loc = lab_dir, scale_pos = pos, shp = shp_fl)
#'
#' }
#'
res_local_map <- function(nerr_site_id
                          , stations
                          , bbox
                          , shp
                          , station_labs = TRUE
                          , lab_loc = NULL
                          , scale_pos = 'bottomleft') {

  # check that a shape file exists
  if(class(shp) != 'SpatialPolygons')
    stop('shapefile (shp) must be SpatialPolygons object')

  # check that length(lab_loc) = length(stations)
  if(!is.null(station_labs) && length(lab_loc) != length(stations))
    stop('Incorrect number of label location identifiers specified. R or L designation must be made for each station.' )

  # check that the bb has the right dimensions
  if(is.null(bbox))
    stop('Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  if(length(bbox) != 4)
    stop('Incorrect number of elements specified for bbox. Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')

  # generate location labels
  loc <- get('sampling_stations')
  loc <- loc[(loc$Station.Code %in% stations), ]
  loc$abbrev <- toupper(substr(loc$Station.Code, start = 4, stop = 5))

  # Determine if r and l labs exist
  if(!is.null(lab_loc)){
    if('L' %in% lab_loc){left_labs <- grep('L', lab_loc)}
    if('R' %in% lab_loc){right_labs <- grep('R', lab_loc)}
  } else {
    #default to left labels
    left_labs <- c(1:4)
  }

  # order selected stations alphabetically
  loc <- loc[order(loc$Station.Code), ]

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
                       , labelOptions = labelOptions(noHide = station_labs, direction = c('left'), opacity = 1, textsize = '16px'))
  }

  if(exists('right_labs')){
    m <- m %>%
      addCircleMarkers(lng = ~Longitude[right_labs] * -1, lat = ~Latitude[right_labs], radius = 5
                       , weight = 0, fillOpacity = 1
                       , color = loc$color[right_labs]
                       , label = loc$abbrev[right_labs]
                       , labelOptions = labelOptions(noHide = station_labs, direction = c('right'), opacity = 1, textsize = '16px'))
  }

  m <- m %>%
    addScaleBar(position = scale_pos) %>%
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

  return(m)
}
