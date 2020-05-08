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
#' @importFrom rlang .data
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Creates a stylized, reserve-level base map for displaying seasonal kendall results from \code{\link{sk_seasonal}}. The user can specify the reserve and stations to plot. The user can also specify a bounding box. For multi-component reserves, the user should specify a bounding box that highlights the component of interest.
#'
#' To display seasonal trends, the user must specify \code{c('inc', 'dec', 'insig')} for each station listed in the \code{stations} argument.
#'
#'
#' @author Julie Padilla, Dave Eslinger
#'
#' @concept analyze
#'
#' @return returns a leaflet object. This function is intended to be used with mapshot to generate a png
#' for the reserve level report
#'
#' @examples
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
#' res_sk_map('elk', stations = stns, sk_result = sk_result,
#' bbox = bounding_elk, scale_pos = pos, shp = shp_fl)
#'
#' \donttest{
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
#' res_sk_map('cbm', stations = stns, sk_result = sk_result, bbox = bounding_cbm_1,
#' scale_pos = pos, shp = shp_fl)
#'
#' res_sk_map('cbm', stations = stns, sk_result = sk_result, bbox = bounding_cbm_2,
#' scale_pos = pos, shp = shp_fl)
#'
#' }
#'
res_sk_map <- function(nerr_site_id
                       , stations
                       , sk_result = NULL
                       , bbox
                       , shp
                       , station_labs = TRUE
                       , lab_loc = NULL
                       , scale_pos = 'bottomleft') {

  #------------------Uncomment for debugging------------------------------------
  library(SWMPrExtension)
  library(sf)
  library(dplyr)
  library(tmap)
  library(tmaptools)
  library(ggmap)
  # library(osmplotr)
  FIRST <- TRUE
  if(FIRST){
    ### DEBUG variables
    # Defaults
    station_labs = TRUE
    # from Example 1, a compact reserve
    stations <-
    sampling_stations[(sampling_stations$NERR.Site.ID == 'elk'
                       & sampling_stations$Status == 'Active'), ]$Station.Code
    to_match <- c('wq')
    stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
    shp_fl <- elk_spatial
    bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
    pos <- 'bottomleft'
    sk_res <- c('inc', 'dec', 'dec', 'insig')

    ### plot call, reassign variables
    ### res_sk_map('elk', stations = stns, sk_result = sk_result,
    ###            bbox = bounding_elk, scale_pos = pos, shp = shp_fl)
    nerr_sit_id <- 'elk'
    stations <- stns
    sk_result <- sk_res
    bbox <- bounding_elk
    lab_loc <- NULL
    scale_pos <- pos
    shp <- shp_fl
    } else {
    # ---------------------------------------------------------------------------
    # Second Example
    # Defaults
    station_labs <- TRUE
    scale_pos <- 'bottom_left'
    ## a multicomponent reserve (showing two different bounding boxes)
    ### set plotting parameters
    stations <-
      sampling_stations[(sampling_stations$NERR.Site.ID == 'cbm'
                         & sampling_stations$Status == 'Active'), ]$Station.Code
    to_match <- c('wq')
    stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
    shp_fl <- cbm_spatial
    bounding_cbm_1 <- c(-77.393, 39.741, -75.553, 38.277)
    bounding_cbm_2 <- c(-76.862006, 38.811571, -76.596508, 38.642454)
    pos <- 'bottomleft'
    sk_res <- c('inc', 'dec', 'dec', 'insig')

    ### plot
    # res_sk_map('cbm', stations = stns, sk_result = sk_result, bbox = bounding_cbm_1,
    #            scale_pos = pos, shp = shp_fl)
    #
    # res_sk_map('cbm', stations = stns, sk_result = sk_result, bbox = bounding_cbm_2,
    #            scale_pos = pos, shp = shp_fl)
    nerr_sit_id <- 'cbm'
    stations <- stns
    bbox <- bounding_cbm_2
    lab_loc <- NULL
    scale_pos <- pos
    shp <- shp_fl

    }
  #---------------end debugging-----------------------------------------------
  # check that a shape file exists
    if(class(shp) != 'SpatialPolygons') {
    if(class(shp) != 'sf') {
      stop('shapefile (shp) must be sf (preferred) or SpatialPolygons object')
    }
  } else {
    shp <- as(shp, "sf")   # convert SpatialPolygons to sf
  }

  # check that sk results correspond to station results
  if(length(stations) != length(sk_result))
    stop('Incorrect number of seasonal kendall results specified.')

  # check that the bb has the right dimensions
  if(is.null(bbox))
    stop('Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  if(length(bbox) != 4)
    stop('Incorrect number of elements specified for bbox. Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  # Get min-max bounding coordinates, and format bbox correctly:
  xmin <- min(bbox[c(1,3)])
  xmax <- max(bbox[c(1,3)])
  ymin <- min(bbox[c(2,4)])
  ymax <- max(bbox[c(2,4)])
  bbox <- c(xmin, ymin, xmax, ymax)

  # generate location labels
  loc <- get('sampling_stations')
  loc <- loc[(loc$Station.Code %in% stations), ]
  loc$abbrev <- toupper(substr(loc$Station.Code, start = 4, stop = 5))
  loc$sk_result <- sk_result

  # Default all labels to left and then change if there is location information
  loc$align <- -1.25
  if(!is.null(lab_loc))
    loc$align[lab_loc == 'R'] <- 1.25

  # # set map label styles
  # label_style <- list(
  #   "box-shadow" = "none",
  #   "border-radius" = "5px",
  #   "font" = "bold 16px/1.5 'Helvetica Neue', Arial, Helvetica, sans-serif",
  #   "padding" = "1px 5px 1px 5px"
  #   )

  # order selected stations alphabetically
  loc <- loc[order(loc$Station.Code), ]

  # Swap sign of longitudes, which seem to be positive in the data!
  loc$Longitude <- -loc$Longitude
  # convert location info to sf object
  # use lat/lon, WGS84 projection, EPSG:4326.
  loc_sf <- sf::st_as_sf(loc, coords = c("Longitude","Latitude"))
  sf::st_crs(loc_sf) <- 4326
  # Now transform into the projected web-mercator projection EPSG:3857
  #loc_sf <- sf::st_transform(loc_sf, 3857)

  # Determine the types of results
  if('inc' %in% loc$sk_result){inc_icons <- grep('inc', loc$sk_result)}
  if('dec' %in% loc$sk_result){dec_icons <- grep('dec', loc$sk_result)}
  if('insig' %in% loc$sk_result){insig_icons <- grep('insig', loc$sk_result)}
  if('insuff' %in% loc$sk_result){insuff_icons <- grep('insuff', loc$sk_result)}

  # Define vectors for the colors, shapes and sizes as needed:
  #   first and second entries are for states, "regular" and "highlighted,"
  #   respectively; third and forth entries are for reserve locations,
  #   "regular" and "highlighted" respecitvely;
  #   5 - 8 are for showing S-K trend results: 5 = increasing, 6 = decreasing,
  #   7 = insignificant, and 8 = insufficient data.
  # This convention holds for colors, shapes and size parameters. The order is
  #   consistent with the original order.

  fill_colors <-  c('#444E65', '#A3DFFF', '#247BA0', '#0a0a0a')
#  res_point_size <-   c(4.9,  4.9,  4.5, 4.2)
  res_point_size <-   c(5,5,5,5)
  res_point_shape <-  c(24, 25, 21, 13)

  # These are the codes for the fill color, size and shape legends.
  break_vals <- c("inc", "dec", "insig", "insuff")

  bg_map <- ggmap::get_stamenmap(bbox,
                                 maptype = "toner-lite",
                                 source = "stamen",
                                 zoom = 13,
                                 urlonly = FALSE)

  m <- ggmap(bg_map) +
    geom_sf(data = shp, aes(), inherit.aes = FALSE,
            fill = "yellow", col = '#B3B300', alpha = 0.3) +
    ggthemes::theme_map() +
#    geom_sf_text(data = loc_sf, aes(), inherit.aes = FALSE) +
    geom_sf(data = loc_sf, inherit.aes = FALSE,
            aes(color = .data$sk_result,
                fill = .data$sk_result,
                shape = .data$sk_result,
                size = .data$sk_result),
            show.legend = FALSE) +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals)


  return(m)
}
