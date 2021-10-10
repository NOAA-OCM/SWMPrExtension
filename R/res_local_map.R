#' Local Reserve Map
#'
#' Create a stylized reserve-level map for use with the reserve level reporting template
#'
#' @param nerr_site_id chr string of the reserve to make, first three characters used by NERRS
#' @param stations chr string of the reserve stations to include in the map
#' @param bbox a bounding box associated with the reserve. Must be in the format of c(X1, Y1, X2, Y2)
#' @param shp {sf} data frame (preferred) or SpatialPolygons object
#' @param station_labs logical, should stations be labeled? Defaults to \code{TRUE}
#' @param lab_loc chr vector of 'R' and 'L', one letter for each station. if no \code{lab_loc} is specified then labels will default to the left.
## #' @param scale_pos a vector of x and y values for scalebar location, *e.g.*, `c( "left", "bottom")`, the default.  Enter `scale_pos = NULL` for none. See `help(tm_scale_bar` for additional options.
#' @param zoom zoom level, 1-21 for OpenStreetMaps maps. Default is to autoscale based on bbox.
#' @param maptype stamen map type from OpenStreetMap::openmap. Theoretically one of c("osm", "osm-bw","maptoolkit-topo", "waze", "bing", "stamen-toner", "stamen-terrain", "stamen-watercolor", "osm-german", "osm-wanderreitkarte", "mapbox", "esri", "esri-topo", "nps", "apple-iphoto", "skobbler", "hillshade", "opencyclemap", "osm-transport", "osm-public-transport", "osm-bbike", "osm-bbike-german").  However, many of these may not work. "stamen-toner", "stamen-terrain", and "bing" seem to work well.
#'
#' @importFrom magrittr "%>%"
#' @importFrom methods as
#' @importFrom osmdata add_osm_features opq osmdata_sf
#' @importFrom sf st_as_sf st_bbox st_crs st_transform
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
#' @author Julie Padilla, Dave Eslinger
#'
#' @concept analyze
#'
#' @return returns a {ggplot} object
#'
#' @examples
#' ## a compact reserve
#' ### set plotting parameters
#' stations <-
#' sampling_stations[(sampling_stations$NERR.Site.ID == 'elk'
#'           & sampling_stations$Status == 'Active'
#'           & sampling_stations$isSWMP == "P"), ]$Station.Code
#'           to_match <- c('wq', 'met')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- elk_spatial
#' bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
#' lab_dir <- c('L', 'R', 'L', 'L', 'L')
#' labs <- c('ap', 'cw', 'nm', 'sm', 'vm')
#'
#' ### Low zoom and default maptype plot (for CRAN testing, not recommended)
#' #    Lower zoom number gives coarser text and fewer features
#' x_low <- res_local_map('elk', stations = stns, bbox = bounding_elk,
#'                    lab_loc = lab_dir, shp = shp_fl,
#'                    zoom = 10)
#'
#' \donttest{
#' ### Default zoom and maptype
#' x_def  <- res_local_map('elk', stations = stns, bbox = bounding_elk,
#'                    lab_loc = lab_dir, shp = shp_fl,
#'                    zoom = 10)
#'
#' ### A multicomponent reserve (show two different bounding boxes)
#' #    set plotting parameters
#' stations <- sampling_stations[(sampling_stations$NERR.Site.ID == 'cbm'
#'             & sampling_stations$Status == 'Active'
#'             & sampling_stations$isSWMP == "P"), ]$Station.Code
#'             to_match <- c('wq', 'met')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- cbm_spatial
#' bounding_cbm_1 <- c(-77.393, 39.741, -75.553, 38.277)
#' bounding_cbm_2 <- c(-76.8,  38.7, -76.62,  38.85)
#' lab_dir <- c('L', 'R', 'L', 'L', 'L')
#' labs <- c('ap', 'cw', 'nm', 'sm', 'vm')
#'
#' ### plot
#' y <- res_local_map('cbm', stations = stns, bbox = bounding_cbm_1,
#'                    lab_loc = lab_dir, shp = shp_fl)
#'
#' z <- res_local_map('cbm', stations = stns, bbox = bounding_cbm_2,
#'                    lab_loc = lab_dir, shp = shp_fl)
#'
#' }
#'
res_local_map <- function(nerr_site_id
                          , stations
                          , bbox
                          , shp
                          , station_labs = TRUE
                          , lab_loc = NULL
#                          , scale_pos = c("lower", "bottom")
                          , zoom = NULL
                          , maptype = "stamen-toner") {

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

  # check that length(lab_loc) = length(stations)
  if(!is.null(station_labs) && length(lab_loc) != length(stations))
    stop('Incorrect number of label location identifiers specified. R or L designation must be made for each station.' )

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

  # Default all labels to left and then change if there is location information
  loc$align <- -1.25
  if(!is.null(lab_loc))
    loc$align[lab_loc == 'R'] <- 1.25

  # order selected stations alphabetically
  loc <- loc[order(loc$Station.Code), ]

  # Swap sign of longitudes, which seem to be positive in the data!
  loc$Longitude <- -loc$Longitude

  # Define lat/long for labels, based on stations, alignment, and bbox
  loc$lab_long <- loc$Longitude + 0.045* loc$align * (bbox[3] - bbox[1])
  loc$lab_lat <- loc$Latitude + 0.015 * (bbox[4] - bbox[2])

  # convert Labels info to sf object, use lat/lon, WGS84 projection, EPSG:4326.
  labels_sf <- loc %>%
    select(abbrev, lab_long, lab_lat) %>%
    sf::st_as_sf(coords = c("lab_long","lab_lat"))
  sf::st_crs(labels_sf) <- 4326

  # convert location info to sf object, use lat/lon, WGS84 projection, EPSG:4326.
  loc_sf <- sf::st_as_sf(loc, coords = c("Longitude","Latitude"))
  sf::st_crs(loc_sf) <- 4326


  # These are the codes for the fill color, size and shape legends.
  fill_colors <-  loc_sf$color #  c('#444E65', '#A3DFFF', '#247BA0', '#0a0a0a')
  break_vals <- loc_sf$abbrev #c("inc", "dec", "insig", "insuff")

  # Set background map zoom level automatically if not specified
  if(is.null(zoom)) {
    diag_size <- sqrt((xmax-xmin)^2 +(ymax-ymin)^2)
    zoom <- 15 - ceiling(sqrt(10*diag_size))
    print(paste("Zoom level calculated as", zoom, sep = " "))
  }
  print(paste("maptype is ",maptype))

  bg_map <- base_map(bbox)
  m <- bg_map #+
    # tmap::tm_rgb(alpha = 0.5) +
    # tmap::tm_shape(shp) +
    # tmap::tm_polygons(lwd = 2, col = 'yellow', alpha = 0.3,
    #                   border.col = '#B3B300', border.alpha = 0.8) +
    # tmap::tm_shape(loc_sf) +
    # tmap::tm_dots(size = .75, col = "color")

  if(station_labs) {
    # m <- m +
    #   tmap::tm_text(text = "abbrev", xmod = "align", just = c("center","top"),
    #                 bg.color = 'white', bg.alpha = 0.75,
    #                 fontface = "bold")
  }
  # if(!is.null(scale_pos)) {
  #   m <- m +
  #     tmap::tm_scale_bar(scale_pos)
  # }


  return(m)
}
