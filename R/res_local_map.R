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
#' @param zoom zoom level, 1-21 for stamen maps. Default is to autoscale based on bbox.
#' @param maptype stamen map type from ggmap::get_stamenmap.  One of c("terrain", "terrain-background", "terrain-labels", "terrain-lines", "toner", "toner-2010", "toner-2011", "toner-background", "toner-hybrid", "toner-labels", "toner-lines", "toner-lite", "watercolor")#'
#'
#' @importFrom ggthemes theme_map
#' @importFrom magrittr "%>%"
#' @importFrom methods as
#' @importFrom sf st_as_sf st_bbox st_crs st_transform
#' @importFrom tmap tm_dots tm_polygons tm_rgb tm_shape tm_text
#' @importFrom tmaptools read_osm
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
#' & sampling_stations$Status == 'Active' & sampling_stations$isSWMP == "P"), ]$Station.Code
#' to_match <- c('wq', 'met')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- elk_spatial
#' bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
#' lab_dir <- c('L', 'R', 'L', 'L', 'L')
#' labs <- c('ap', 'cw', 'nm', 'sm', 'vm')
#'
#' ### plot
#' x <- res_local_map('elk', stations = stns, bbox = bounding_elk,
#'                    lab_loc = lab_dir, shp = shp_fl)
#'
#' \donttest{
#' ## a multicomponent reserve (show two different bounding boxes)
#' ### set plotting parameters
#' stations <-
#' sampling_stations[(sampling_stations$NERR.Site.ID == 'cbm'
#' & sampling_stations$Status == 'Active' & sampling_stations$isSWMP == "P"), ]$Station.Code
#' to_match <- c('wq', 'met')
#' stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
#' shp_fl <- cbm_spatial
#' bounding_cbm_1 <- c(-77.393, 39.741, -75.553, 38.277)
#' bounding_cbm_2 <- c(-76.8,  38.7, -76.62,  38.85)
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

  fill_colors <-  loc_sf$color #  c('#444E65', '#A3DFFF', '#247BA0', '#0a0a0a')
  break_vals <- loc_sf$abbrev #c("inc", "dec", "insig", "insuff")


  # These are the codes for the fill color, size and shape legends.
  # Set background map zoom level automatically if not specified
  if(is.null(zoom)) {
    diag_size <- sqrt((xmax-xmin)^2 +(ymax-ymin)^2)
    zoom <- 14 - ceiling(sqrt(10*diag_size))
    print(paste("Zoom level calculated as", zoom, sep = " "))
  }
  print(paste("maptype is ",maptype))
  # Plot map
  # m <- leaflet(loc, options = leafletOptions(zoomControl = FALSE), width = 500, height = 500) %>%
  #   addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%  # Add default OpenStreetMap map tiles, CartoDB.Positron
  #   addPolygons(data = shp, weight = 2, color = '#B3B300', fillColor = 'yellow')

  # bg_map <- tmaptools::read_osm(bbox, type = "stamen-toner", refsys = 4326)
  # bg_bing <- tmaptools::read_osm(bbox, type = "bing")
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
            aes(color = .data$abbrev,
                fill = .data$abbrev),
            shape = 21,
            size = 3.8,
            show.legend = FALSE) +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
    scale_fill_manual(values = fill_colors, breaks = break_vals)

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
