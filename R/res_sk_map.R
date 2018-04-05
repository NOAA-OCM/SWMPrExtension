#' Local Reserve Map With Seasonal Kendall Results
#'
#' Create a stylized reserve-level map of seasonal kendall results for use with the reserve level reporting template
#'
#' @param nerr_site_id chr string of the reserve to make, first three characters used by NERRS
#' @param stations chr string of the reserve stations to include in the map
#' @param sk_result vector of values denoting direction and significance of seasonal kendall results. Result should be \code{c('inc', 'dec', 'insig', 'insuff')} for significant positive, significant negative, no significant results, and insufficient data to calculate result.
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
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return returns a leaflet object. This function is intended to be used with mapshot to generate a png
#' for the reserve level report
#'
#' @examples
#' \dontrun{
#'
#' ## a compact reserve
#' ### set plotting parameters
#' stations <-
#' sampling_stations[(sampling_stations$NERR.Site.ID == 'elk'
#' & sampling_stations$Status == 'Active'), ]$Station.Code
#' to_match <- c('wq')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- elk_spatial
#' bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
#' pos <- 'bottomleft'
#' sk_res <- c('inc', 'dec', 'dec', 'insig')
#'
#' ### plot
#' res_sk_map('elk', stations = stns, sk_result = sk_res,
#' bbox = bounding_elk, scale_pos = pos, shp = shp_fl)
#'
#'
#' ## a multicomponent reserve (showing two different bounding boxes)
#' ### set plotting parameters
#' stations <-
#' sampling_stations[(sampling_stations$NERR.Site.ID == 'cbm'
#' & sampling_stations$Status == 'Active'), ]$Station.Code
#' to_match <- c('wq')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- cbm_spatial
#' bounding_cbm_1 <- c(-77.393, 39.741, -75.553, 38.277)
#' bounding_cbm_2 <- c(-76.862006, 38.811571, -76.596508, 38.642454)
#' pos <- 'bottomleft'
#' sk_res <- c('inc', 'dec', 'dec', 'insig')
#'
#' ### plot
#' res_sk_map('cbm', stations = stns, sk_result = sk_res, bbox = bounding_cbm_1,
#' scale_pos = pos, shp = shp_fl)
#'
#' res_sk_map('cbm', stations = stns, sk_result = sk_res, bbox = bounding_cbm_2,
#' scale_pos = pos, shp = shp_fl)
#'
#' }
#'
res_sk_map <- function(nerr_site_id, stations, sk_result = NULL, bbox, shp, station_labs = T, lab_loc = NULL, scale_pos = 'bottomleft') {

  # check that a shape file exists
  if(class(shp) != 'SpatialPolygons')
    stop('shapefile (shp) must be SpatialPolygons object')

  # check that sk results correspond to station results
  if(length(stations) != length(sk_result))
    stop('Incorrect number of seasonal kendall results specified.')

  # check that the bb has the right dimensions
  if(is.null(bbox))
    stop('Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  if(length(bbox) != 4)
    stop('Incorrect number of elements specified for bbox. Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')

  # generate location labels
  loc <- get('sampling_stations')
  loc <- loc[(loc$Station.Code %in% stations), ]
  loc$abbrev <- toupper(substr(loc$Station.Code, start = 4, stop = 5))
  loc$sk_result <- sk_result

  # Determine if r and l labs exist
  if(!is.null(lab_loc)){
    left_labs <- grep('L', lab_loc)
    right_labs <- grep('R', lab_loc)
  } else {
    #default to left labels
    left_labs <- rep('L', length(stations))
  }

  # order selected stations alphabetically
  loc <- loc[order(loc$Station.Code), ]

  # Determine the types of results
  if('inc' %in% loc$sk_result){inc_icons <- grep('inc', loc$sk_result)}
  if('dec' %in% loc$sk_result){dec_icons <- grep('dec', loc$sk_result)}
  if('insig' %in% loc$sk_result){insig_icons <- grep('insig', loc$sk_result)}
  if('insuff' %in% loc$sk_result){insuff_icons <- grep('insuff', loc$sk_result)}

  # Plot map
  m <- leaflet(loc, options = leafletOptions(zoomControl = FALSE), width = 500, height = 500) %>%
    addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%  # Add default OpenStreetMap map tiles, CartoDB.Positron
    addPolygons(data = shp, weight = 2, color = '#B3B300', fillColor = 'yellow')

  if(length(left_labs) > 0){
    m <- m %>%
      addLabelOnlyMarkers(lng = ~Longitude[left_labs] * -1, lat = ~Latitude[left_labs]
                          , label = loc$abbrev[left_labs]
                          , labelOptions = labelOptions(noHide = station_labs
                                                        , direction = c('left')
                                                        , opacity = 1
                                                        , textsize = '16px'
                                                        , offset = c(12, -15)))
  }

  if(length(right_labs) > 0){
    m <- m %>%
      addLabelOnlyMarkers(lng = ~Longitude[right_labs] * -1, lat = ~Latitude[right_labs]
                          , label = loc$abbrev[right_labs]
                          , labelOptions = labelOptions(noHide = station_labs
                                                        , direction = c('right')
                                                        , opacity = 1
                                                        , textsize = '16px'
                                                        , offset = c(12, -15))) #default offset is c(12, -15)
  }



  if(exists('inc_icons')){
    # create file path for icon image
    ico_loc <- system.file('extdata', 'arrow_inc.png', package = 'SWMPrExtension')

    # make icon
    icon_img <- makeIcon(iconUrl = ico_loc
                     , iconWidth = 30
                     , iconHeight = 40
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
                         , iconHeight = 40
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
                         , iconWidth = 30 #40
                         , iconHeight = 15
                         , iconAnchorX = 15 #20
                         , iconAnchorY = 7)

    # plot custom icon
    m <- m %>%
      addMarkers(lng = ~Longitude[insig_icons] * -1, lat = ~Latitude[insig_icons]
                 , icon = icon_img)
  }

  if(exists('insuff_icons')){
    # create file path for icon image
    ico_loc <- system.file('extdata', 'x_insuff.png', package = 'SWMPrExtension')

    # make icon
    icon_img <- makeIcon(iconUrl = ico_loc
                         , iconWidth = 25 #40
                         , iconHeight = 25
                         , iconAnchorX = 12.5
                         , iconAnchorY = 12.5)

    # plot custom icon
    m <- m %>%
      addMarkers(lng = ~Longitude[insuff_icons] * -1, lat = ~Latitude[insuff_icons]
                 , icon = icon_img)
  }



  m <- m %>%
    addScaleBar(position = scale_pos) %>%
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

  return(m)
}
