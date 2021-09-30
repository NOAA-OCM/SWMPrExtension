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
#' @param zoom zoom level, 1-21 for stamen maps. Default is to autoscale based on bbox.
#' @param maptype stamen map type from ggmap::get_stamenmap.  One of c("terrain", "terrain-background", "terrain-labels", "terrain-lines", "toner", "toner-2010", "toner-2011", "toner-background", "toner-hybrid", "toner-labels", "toner-lines", "toner-lite", "watercolor")
#'
#' @importFrom ggmap get_stamenmap ggmap
#' @importFrom ggthemes theme_map
#' @importFrom magrittr "%>%"
#' @importFrom methods as
#' @importFrom rlang .data
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Creates a stylized, reserve-level base map for displaying seasonal kendall results from \code{\link{sk_seasonal}}. The user can specify the reserve and stations to plot. The user can also specify a bounding box. For multi-component reserves, the user should specify a bounding box that highlights the component of interest.
#'
#' To display seasonal trends, the user must specify \code{c('inc', 'dec', 'insig', 'insuff')} for each station listed in the \code{stations} argument.
#'
#'
#' @author Julie Padilla, Dave Eslinger
#'
#' @concept analyze
#'
#' @return returns a {ggplot} object.
#'
#' @examples
#' ## a compact reserve
#' ### set plotting parameters
#' stations <-
#' sampling_stations[(sampling_stations$NERR.Site.ID == 'elk'
#' & sampling_stations$Status == 'Active' & sampling_stations$isSWMP == "P"), ]$Station.Code
#' to_match <- c('wq')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- elk_spatial
#' bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
#' trnds <- c('inc', 'dec', 'dec', 'insig')
#'
#' ### plot
#' x <- res_sk_map('elk', stations = stns, sk_result = trnds,
#'                  bbox = bounding_elk, shp = shp_fl)
#'
#' \donttest{
#'
#' ### Higher zoom number gives more details, but may not be visible
#' x_13 <- res_sk_map('elk', stations = stns, sk_result = trnds,
#'                  bbox = bounding_elk, shp = shp_fl,
#'                  zoom = 13)
#'
#' ### Lower zoom number gives coarser text and fewer features
#' x_11 <- res_sk_map('elk', stations = stns, sk_result = trnds,
#'                  bbox = bounding_elk, shp = shp_fl,
#'                  zoom = 11)
#'
#' ### Different maptypes may be used.  All may not be available.
#' #    Note that zoom and maptype interact, so some experiemtation may be
#' #    required.
#' x_terrain <- res_sk_map('elk', stations = stns, sk_result = trnds,
#'                  bbox = bounding_elk, shp = shp_fl,
#'                  maptype = 'terrain')

#' ### A multicomponent reserve (showing two different bounding boxes)
#' #    set plotting parameters
#' stations <-
#' sampling_stations[(sampling_stations$NERR.Site.ID == 'cbm'
#' & sampling_stations$Status == 'Active' & sampling_stations$isSWMP == "P"), ]$Station.Code
#' to_match <- c('wq')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- cbm_spatial
#' bounding_cbm_1 <- c(-77.393, 39.741, -75.553, 38.277)
#' bounding_cbm_2 <- c(-76.8,  38.7, -76.62,  38.85)
#' trnds <- c('inc', 'dec', 'dec', 'insig')
#'
#' #   plot
#' y <- res_sk_map('cbm', stations = stns, sk_result = trnds, bbox = bounding_cbm_1,
#'                  shp = shp_fl)
#'
#' z <- res_sk_map('cbm', stations = stns, sk_result = trnds, bbox = bounding_cbm_2,
#'                  shp = shp_fl)
#' }

res_sk_map <- function(nerr_site_id
                       , stations
                       , sk_result = NULL
                       , bbox
                       , shp
                       , station_labs = TRUE
                       , lab_loc = NULL
                       , zoom = NULL
                       , maptype = 'toner-lite') {


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

  # Define vectors for the colors, shapes and sizes as needed:
  #   1 - 4 are for showing S-K trend results: 1 = increasing, 2 = decreasing,
  #   3 = insignificant, and 4 = insufficient data.
  # This convention holds for colors, shapes and size parameters. The order is
  #   consistent with the original order.

  fill_colors <-  c('#444E65', '#A3DFFF', '#247BA0', '#0a0a0a')
  res_point_size <-   c(8,8,8,8)
  res_point_shape <-  c(24, 25, 21, 13)

  # These are the codes for the fill color, size and shape legends.
  break_vals <- c("inc", "dec", "insig", "insuff")

  # Set background map zoom level automatically if not specified
  if(is.null(zoom)) {
    diag_size <- sqrt((xmax-xmin)^2 +(ymax-ymin)^2)
    zoom <- 14 - ceiling(sqrt(10*diag_size))
    print(paste("Zoom level calculated as", zoom, sep = " "))
  }
  print(paste("maptype is ",maptype))

  bg_map <- ggmap::get_stamenmap(bbox,
                                 maptype = maptype,
                                 source = "stamen",
                                 zoom = zoom,
                                 messaging = FALSE,
                                 epsg = 3785,
                                 urlonly = FALSE)

  m <- ggmap::ggmap(bg_map) +
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

  if(station_labs) {
    # Define lat/long for labels, based on stations, alignment, and bbox
    loc$lab_long <- loc$Longitude + 0.045* loc$align * (bbox[3] - bbox[1])
    loc$lab_lat <- loc$Latitude + 0.015 * (bbox[4] - bbox[2])

    # convert Labels info to sf object, use lat/lon, WGS84 projection, EPSG:4326.
    labels_sf <- loc %>%
      select(abbrev, lab_long, lab_lat) %>%
      sf::st_as_sf(coords = c("lab_long","lab_lat"))
    sf::st_crs(labels_sf) <- 4326

    m <- m +
      geom_sf_label(data = labels_sf, inherit.aes = FALSE,
                    aes(label = abbrev))
  }

  return(m)
}
