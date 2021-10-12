#' Local Reserve Map With Seasonal Kendall Results for Custom Stations
#'
#' Create a stylized reserve-level map of seasonal kendall results from custom station locations for use with the reserve level reporting template
#'
#' @param stations chr string of the reserve stations to include in the map
#' @param x_loc num vector of x coordinates for \code{stations}. East longitudes must be negative.
#' @param y_loc num vector of y coordinates for \code{stations}
#' @param sk_result vector of values denoting direction and significance of seasonal kendall results. Result should be c('inc', 'dec', 'insig') for sig. negative, no sig. results, and sig. positive result
#' @param bbox a bounding box associated with the reserve. Must be in the format of c(X1, Y1, X2, Y2)
#' @param shp {sf} data frame (preferred) or SpatialPolygons object
#' @param station_labs logical, should stations be labeled? Defaults to \code{TRUE}
#' @param lab_loc chr vector of 'R' and 'L', one letter for each station. if no \code{lab_loc} is specified then labels will default to the left.
## #' @param scale_pos a vector of x and y values for scalebar location, *e.g.*, `c( "left", "bottom")`, the default.  Enter `scale_pos = NULL` for none. See `help(tm_scale_bar` for additional options.
#' @param zoom zoom level, 1-21 for OpenStreetMaps maps. Default is to autoscale based on bbox. Higher numbers give more detail.
#' @param maptype stamen map type from OpenStreetMap::openmap. Theoretically one of c(“terrain”, “terrain-background”, “terrain-labels”, “terrain-lines”, “toner”, “toner-2010”, “toner-2011”, “toner-background”, “toner-hybrid”, “toner-labels”, “toner-lines”, “toner-lite”, “watercolor”).
#'
#' @importFrom ggmap ggmap
#' @importFrom magrittr "%>%"
#' @importFrom methods as
#' @importFrom osmdata add_osm_features opq osmdata_sf
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_bbox st_crs st_transform
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
#' @return returns a {ggplot} object
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
#'
#' ### Low zoom and default maptype plot (for CRAN testing, not recommended)
#' #    Lower zoom number gives coarser text and fewer features
#' x_low <- res_custom_sk_map(stations = stns, x_loc = x_coords,
#'                   sk_result = trnds, y_loc = y_coords,
#'                   bbox = bounding_elk, lab_loc = lab_dir,
#'                   shp = shp_fl, zoom = 10)
#' \donttest{
#' ### Default zoom and maptype plot
#' x_def <- res_custom_sk_map(stations = stns, x_loc = x_coords,
#'                   sk_result = trnds, y_loc = y_coords,
#'                   bbox = bounding_elk, lab_loc = lab_dir,
#'                   shp = shp_fl)
#'
#' ### Higher zoom number gives more details, but may not be visible
#' x_14 <- res_custom_sk_map(stations = stns, x_loc = x_coords,
#'                   sk_result = trnds, y_loc = y_coords,
#'                   bbox = bounding_elk, lab_loc = lab_dir,
#'                   shp = shp_fl, zoom = 14)
#'
#'
#' ### Different maptypes may be used.  All may not be available.
#' #   Note that zoom and maptype interact, so some experimentation may be
#' #   required.
#' x_terrain <- res_custom_sk_map(stations = stns, x_loc = x_coords,
#'                   sk_result = trnds, y_loc = y_coords,
#'                   bbox = bounding_elk, lab_loc = lab_dir,
#'                   shp = shp_fl, maptype = 'stamen-terrain')
#' }
#
res_custom_sk_map <- function(stations
                                   , x_loc
                                   , y_loc
                                   , sk_result = NULL
                                   , bbox
                                   , shp
                                   , station_labs = TRUE
                                   , lab_loc = NULL
                                   , zoom = NULL
                          , maptype = "toner-lite") {

  # define local variables  to remove `check()` warnings
  abbrev <- lab_long <- lab_lat <- NULL

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

  # # check that length(lab_loc) = length(stations)
  # if(!is.null(station_labs) && length(lab_loc) != length(stations))
  #   stop('Incorrect number of label location identifiers specified. R or L designation must be made for each station.' )

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

  #check that stations, x_loc, and y_loc match
  if(length(stations) != length(x_loc))
    stop('An incorrect number of x coordinates were specified. One x coordinate must be specified for each station')
  if(length(stations) != length(y_loc))
    stop('An incorrect number of y coordinates were specified. One y coordinate must be specified for each station')

  # generate location labels
  loc <- data.frame(abbrev = stations, Latitude = y_loc, Longitude = x_loc,
                    sk_result = sk_result, stringsAsFactors = FALSE)

  # Default all labels to left and then change if there is location information
  loc$align <- -1.25
  if(!is.null(lab_loc))
    loc$align[lab_loc == 'R'] <- 1.25

  # If longitudes are positive and print warning
  if(sum(loc$Longitude > 0) > 0) {
    # loc$Longitude[loc$Longitude > 0] <- -loc$Longitude[loc$Longitude > 0]
    warning("Positive longitudes given, please double check")
  }
  # convert location info to sf object
  # use lat/lon, WGS84 projection, EPSG:4326.
  loc_sf <- sf::st_as_sf(loc, coords = c("Longitude","Latitude"))
  sf::st_crs(loc_sf) <- 4326

  # Define vectors for the colors, shapes and sizes as needed:
  #   1 - 4 are for showing S-K trend results: 1 = increasing, 2 = decreasing,
  #   3 = insignificant, and 4 = insufficient data.
  # This convention holds for colors, shapes and size parameters. The order is
  #   consistent with the original order.

  # These are the codes for the fill color, size and shape legends.
  break_vals <- c("inc", "dec", "insig", "insuff")
  fill_colors <-  c('#444E65', '#A3DFFF', '#247BA0', '#0a0a0a')
  res_point_size <-   c(6, 6, 6, 9)
  res_point_shape <-  c(24, 25, 21, 13)

  master_key <- as.data.frame(cbind(break_vals, fill_colors, res_point_size, res_point_shape))

  needed_keys <- left_join(loc, master_key, by = c("sk_result" = "break_vals"))

  use_shape <- unique(as.integer(needed_keys$res_point_shape))
  use_color <- unique(needed_keys$fill_color)
  # use_size  <- unique(needed_keys$res_point_size)

  # Set background map zoom level automatically if not specified
  if(is.null(zoom)) {
    diag_size <- sqrt((xmax-xmin)^2 +(ymax-ymin)^2)
    zoom <- 14 - ceiling(sqrt(10*diag_size))
    print(paste("Zoom level calculated as", zoom, sep = " "))
  }
  print(paste("maptype is ",maptype))

  bg_map <- base_map(bbox, crs = st_crs(shp),
                     maptype = maptype,
                     zoom = zoom)
  m <- bg_map +
    geom_sf(data = shp, aes(), inherit.aes = FALSE,
            fill = "yellow", col = '#B3B300', alpha = 0.3) +
    ggthemes::theme_map() +
    #    geom_sf_text(data = loc_sf, aes(), inherit.aes = FALSE) +
    geom_sf(data = loc_sf, inherit.aes = FALSE,
            aes(color = .data$sk_result,
                fill = .data$sk_result,
                shape = .data$sk_result,
                size = .data$sk_result),
            stroke = 2,
            show.legend = FALSE) +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals)

    # tmap::tm_shape(shp) +
    # tmap::tm_polygons(lwd = 2, col = 'yellow', alpha = 0.3,
    #                   border.col = '#B3B300', border.alpha = 0.8) +
    # tm_shape(loc_sf) +
    # tmap::tm_symbols(size = 1.5,
    #                  col = "sk_result",
    #                  # border_col = "sk_result",
    #                  shape = "sk_result",
    #                  shapes = use_shape,
    #                  palette = use_color,
    #                  legend.col.show = FALSE,
    #                  legend.shape.show = FALSE)

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
    # m <- m +
    #   tmap::tm_text(text = "abbrev", xmod = "align", just = c("center","top"),
    #                 bg.color = 'white', bg.alpha = 0.75,
    #                 fontface = "bold")
  }
   m <- m +
    coord_sf(
      xlim = c(bbox[1], bbox[3]),
      ylim = c(bbox[2], bbox[4]),
      expand = FALSE,
      crs = st_crs(shp),
      default_crs = NULL,
      datum = sf::st_crs(4326),
      # label_graticule = waiver(),
      # label_axes = waiver(),
      lims_method = c("cross", "box", "orthogonal", "geometry_bbox"),
      ndiscr = 100,
      default = FALSE,
      clip = "on"
    )

  return(m)
}
