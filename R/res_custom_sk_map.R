#' Local Reserve Map With Seasonal Kendall Results for Custom Stations
#'
#' Create a stylized reserve-level map of seasonal kendall results from custom station locations for use with the reserve level reporting template
#'
#' @param stations chr string of the reserve stations to include in the map
#' @param x_loc num vector of x coordinates for \code{stations}
#' @param y_loc num vector of y coordinates for \code{stations}
#' @param sk_result vector of values denoting direction and significance of seasonal kendall results. Result should be c('inc', 'dec', 'insig') for sig. negative, no sig. results, and sig. positive result
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
#' @details Creates a stylized, reserve-level base map for displaying seasonal kendall results from \code{\link{sk_seasonal}}. The user can specify the reserve and stations to plot. The user can also specify a bounding box. For multi-component reserves, the user should specify a bounding box that highlights the component of interest.
#'
#' To display seasonal trends, the user must specify \code{c('inc', 'dec', 'insig')} for each station listed in the \code{stations} argument.
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return returns a leaflet object. This function is intended to be used with mapshot to generate a png
#' for the reserve level report
#'
#' @examples
#' ### set plotting parameters
#' stns <- c('custom stn 1', 'custom stn 2')
#' x_coords <- c(-121.735281, -121.750369)
#' y_coords <- c(36.850377, 36.806667)
#' shp_fl <- elk_spatial
#' bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
#' lab_dir <- c('R', 'L')
#' trnds <- c('inc', 'dec')
#' pos <- 'bottomleft'
#'
#' ### plot
#' res_custom_sk_map(stations = stns, x_loc = x_coords,
#' sk_result = trnds, y_loc = y_coords,
#' bbox = bounding_elk, lab_loc = lab_dir,
#' scale_pos = pos, shp = shp_fl)
#'
res_custom_sk_map <- function(stations
                              , x_loc
                              , y_loc
                              , sk_result = NULL
                              , bbox
                              , shp
                              , station_labs = TRUE
                              , lab_loc = NULL
                              , scale_pos = 'bottomleft') {

  # check that a shape file exists
  if(class(shp) != 'SpatialPolygons')
    stop('shapefile (shp) must be SpatialPolygons object')

  # check that sk results correspond to station results
  if(length(stations) != length(sk_result))
    stop('Incorrect number of seasonal kendall results specified.')

  # # check that length(lab_loc) = length(stations)
  # if(!is.null(station_labs) && length(lab_loc) != length(stations))
  #   stop('Incorrect number of label location identifiers specified. R or L designation must be made for each station.' )

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

  # generate location labels
  loc <- data.frame(abbrev = stations, Latitude = y_loc, Longitude = -1 * x_loc, stringsAsFactors = FALSE)

  # Determine if r and l labs exist
  if(!is.null(lab_loc)){
    if('L' %in% lab_loc){left_labs <- grep('L', lab_loc)}
    if('R' %in% lab_loc){right_labs <- grep('R', lab_loc)}
  } else {
    #default to left labels
    left_labs <- c(1:length(stations))
  }

  # set map label styles
  label_style <- list(
    "box-shadow" = "none",
    "border-radius" = "5px",
    "font" = "bold 16px/1.5 'Helvetica Neue', Arial, Helvetica, sans-serif",
    "padding" = "1px 5px 1px 5px"
  )

  # Determine the types of results
  if('inc' %in% sk_result){inc_icons <- grep('inc', sk_result)}
  if('dec' %in% sk_result){dec_icons <- grep('dec', sk_result)}
  if('insig' %in% sk_result){insig_icons <- grep('insig', sk_result)}

  # Plot map
  m <- leaflet(loc, options = leafletOptions(zoomControl = FALSE), width = 500, height = 500) %>%
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%  # Add default OpenStreetMap map tiles, CartoDB.Positron
    addPolygons(data = shp, weight = 2, color = '#B3B300', fillColor = 'yellow')

  if(exists('left_labs')){
    m <- m %>%
      addLabelOnlyMarkers(lng = ~Longitude[left_labs] * -1, lat = ~Latitude[left_labs]
                          , label = loc$abbrev[left_labs]
                          , labelOptions = labelOptions(noHide = station_labs
                                                        , direction = c('left')
                                                        , opacity = 1
                                                        , offset = c(-10, 0)
                                                        , style = label_style))
  }

  if(exists('right_labs')){
    m <- m %>%
      addLabelOnlyMarkers(lng = ~Longitude[right_labs] * -1, lat = ~Latitude[right_labs]
                          , label = loc$abbrev[right_labs]
                          , labelOptions = labelOptions(noHide = station_labs
                                                        , direction = c('right')
                                                        , opacity = 1
                                                        , offset = c(10, 0)
                                                        , style = label_style))
  }


  if(exists('inc_icons')){
    # create file path for icon image
    ico_loc <- system.file('extdata', 'arrow_inc.png', package = 'SWMPrExtension')

    # make icon
    icon_img <- makeIcon(iconUrl = ico_loc
                         , iconWidth = 30
                         , iconHeight = 30
                         , iconAnchorX = 15
                         , iconAnchorY = 15)

    # plot custom icon
    m <- m %>%
      addMarkers(lng = ~Longitude[inc_icons] * -1, lat = ~Latitude[inc_icons]
                 , icon = icon_img)
  }

  if(exists('dec_icons')){
    # create file path for icon image
    ico_loc <- system.file('extdata', 'arrow_dec.png', package = 'SWMPrExtension')

    # make icon
    icon_img <- makeIcon(iconUrl = ico_loc
                         , iconWidth = 30
                         , iconHeight = 30
                         , iconAnchorX = 15
                         , iconAnchorY = 15)

    # plot custom icon
    m <- m %>%
      addMarkers(lng = ~Longitude[dec_icons] * -1, lat = ~Latitude[dec_icons]
                 , icon = icon_img)
  }

  if(exists('insig_icons')){
    # create file path for icon image
    ico_loc <- system.file('extdata', 'bar_insig.png', package = 'SWMPrExtension')

    # make icon
    icon_img <- makeIcon(iconUrl = ico_loc
                         , iconWidth = 40
                         , iconHeight = 14
                         , iconAnchorX = 20
                         , iconAnchorY = 7)

    # plot custom icon
    m <- m %>%
      addMarkers(lng = ~Longitude[insig_icons] * -1, lat = ~Latitude[insig_icons]
                 , icon = icon_img)
  }

  m <- m %>%
    addScaleBar(position = scale_pos) %>%
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

  return(m)
}
