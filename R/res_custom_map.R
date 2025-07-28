#' Local Reserve Map for Custom Stations
#'
#' Create a stylized reserve-level map of custom station locations for use with the reserve level reporting template
#'
#' @param stations chr string of the reserve stations to include in the map
#' @param x_loc num vector of x coordinates for \code{stations}
#' @param y_loc num vector of y coordinates for \code{stations}
#' @param bbox a bounding box associated with the reserve. Must be in the format
#'   of c(X1, Y1, X2, Y2)
#' @param shp {sf} data frame (preferred) or SpatialPolygons object
#' @param station_labs logical, should stations be labeled? Defaults to
#'   \code{TRUE}
#' @param station_col chr vector of colors used to color station points.
#'   Defaults to 'black'.
#' @param lab_loc chr vector of 'R' and 'L', one letter for each station. if no
#'   \code{lab_loc} is specified then labels will default to the left.
#' @param bg_map a georeferenced \code{ggmap} or \code{ggplot} object used as a
#'   background map, generally provided by a call to \code{base_map}. If
#'   \code{bg_map} is specified, \code{maptype} and \code{zoom} are ignored.
#' @param maptype Background map type from Stadia Maps (formerly Stamen)
#'   (\url{https://docs.stadiamaps.com/}); one of c("stamen_terrain",
#'   "stamen_toner", "stamen_toner_lite", "stamen_watercolor", "alidade_smooth",
#'   "alidade_smooth_dark", "outdoors", "stamen_terrain_background",
#'   "stamen_toner_background", "stamen_terrain_labels", "stamen_terrain_lines",
#'   "stamen_toner_labels", "stamen_toner_lines").
#' @param zoom Zoom level for the base map created when \code{bg_map} is not
#'   specified.  An integer value, 5 - 15, with higher numbers providing  more
#'   detail.  If not provided, a zoom level is autoscaled based on \code{bbox}
#'   parameters.
#'
#' @importFrom magrittr "%>%"
#' @importFrom methods as
#' @importFrom sf st_as_sf st_bbox st_crs st_transform
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Creates a stylized, reserve-level base map. The user can specify the
#'   reserve and stations to plot. The user can also specify a bounding box. For
#'   multi-component reserves, the user should specify a bounding box that
#'   highlights the component of interest.
#'
#'   This function does not automatically detect conflicts between station
#'   labels. The \code{lab_loc} argument allows the user to specify "R" or "L"
#'   for each station to prevent labels from conflicting with each other.
#'
#'   This function is intended to be used with \code{mapview::mapshot} to
#'   generate a png for the reserve-level report.
#'
#' @author Julie Padilla, Dave Eslinger
#'
#' @concept analyze
#'
#' @return returns a \code{ggplot} object
#'
#' @examples
#' ### set plotting parameters
#' stns <- c('Stn 1', 'Stn 2')
#' x_coords <- c(-121.735281, -121.750369)
#' y_coords <- c(36.850377, 36.806667)
#' shp_fl <- elk_spatial
#' bounding_elk <- c(-121.8005, 36.7779, -121.6966, 36.8799)
#' lab_dir <- c('L', 'R')
#'
#' ### Low zoom and default maptype plot (for CRAN testing, not recommended)
#' #    Lower zoom number gives coarser text and fewer features
#' (x_low <- res_custom_map(stations = stns, x_loc = x_coords, y_loc = y_coords,
#'                     bbox = bounding_elk, lab_loc = lab_dir, shp = shp_fl,
#'                     zoom = 10))
#'
#' \donttest{
###  Default zoom and maptype plot
#' x_def <- res_custom_map(stations = stns, x_loc = x_coords, y_loc = y_coords,
#'                     bbox = bounding_elk, lab_loc = lab_dir, shp = shp_fl)
#'
#' res_custom_map(stations = stns, x_loc = x_coords, y_loc = y_coords,
#'                bbox = bounding_elk, lab_loc = lab_dir,
#'                shp = shp_fl, station_col = c('red', 'green'))
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
                           , bg_map = NULL
                           , zoom = NULL
                           , maptype = "stamen_toner_lite") {

  # define local variables  to remove `check()` warnings
  abbrev <- lab_long <- lab_lat <- NULL

  # check that a shape file exists
  if(!('SpatialPolygons' %in% class(shp))) {
    if(!('sf' %in% class(shp))) {
      stop('shapefile (shp) must be sf (preferred) or SpatialPolygons object')
    }
  } else {
    shp <- as(shp, "sf")   # convert SpatialPolygons to sf
  }

  # check that stations were specified
  if(is.null(stations))
    stop('No stations were specified. Specify stations to be mapped.')

  # check that length(lab_loc) = length(stations)
  if(!is.null(station_labs) && length(lab_loc) != length(stations))
    stop('Incorrect number of label location identifiers specified.
         R or L designation must be made for each station.' )

  # check that the bb has the right dimensions
  if(is.null(bbox))
    stop('Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  if(length(bbox) != 4)
    stop('Incorrect number of elements specified for bbox.
         Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  # Get min-max bounding coordinates, and format bbox correctly:
  xmin <- min(bbox[c(1,3)])
  xmax <- max(bbox[c(1,3)])
  ymin <- min(bbox[c(2,4)])
  ymax <- max(bbox[c(2,4)])
  bbox <- c(xmin, ymin, xmax, ymax)

  #check that stations, x_loc, and y_loc match
  if(length(stations) != length(x_loc))
    stop('An incorrect number of x coordinates were specified.
         One x coordinate must be specified for each station')
  if(length(stations) != length(y_loc))
    stop('An incorrect number of y coordinates were specified.
         One y coordinate must be specified for each station')

  if(is.null(station_col)) {
    station_col <- 'black'
  }

  # generate location labels
  loc <- data.frame(abbrev = stations, Latitude = y_loc, Longitude = x_loc,
                    color = station_col, stringsAsFactors = FALSE)

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


  fill_colors <-  loc_sf$color #  c('#444E65', '#A3DFFF', '#247BA0', '#0a0a0a')
  break_vals <- loc_sf$abbrev #c("inc", "dec", "insig", "insuff")

  # Set background map zoom level automatically if not specified
  # if(is.null(zoom)) {
  #   diag_size <- sqrt((xmax-xmin)^2 +(ymax-ymin)^2)
  #   zoom <- 14 - ceiling(sqrt(10*diag_size))
  #   print(paste("Zoom level calculated as", zoom, sep = " "))
  # }
  print(paste("maptype is ",maptype))

  if(is.null(bg_map)) {
    bg_map <- base_map(bbox, crs = st_crs(shp),
                     maptype = maptype,
                     zoom = zoom)
  }

  m <- bg_map +
    geom_sf(data = shp, aes(), inherit.aes = FALSE,
            fill = "yellow", col = '#B3B300', alpha = 0.3) +
    ggthemes::theme_map() +
    #    geom_sf_text(data = loc_sf, aes(), inherit.aes = FALSE) +
    geom_sf(data = loc_sf, inherit.aes = FALSE,
            aes(color = .data$abbrev,
                fill = .data$abbrev),
            shape = 21,
            size = 3.8,
            show.legend = FALSE) +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
    scale_fill_manual(values = fill_colors, breaks = break_vals)

  if(station_labs) {
    # Define lat/long for labels, based on stations, alignment, and bbox
    loc$lab_long <- loc$Longitude + 0.06* loc$align * (bbox[3] - bbox[1])
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
