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
#' @param scale_pos scale_pos where should the scale be placed? Options are 'topleft', 'topright', 'bottomleft', or 'bottomright'. Defaults to 'bottomleft'
#'
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
#' @return returns a {ggplot} or? {tmap} object
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
#' \donttest{
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

  # ====Uncomment for debugging=====================================
  # FIRST <- FALSE
  # library(SWMPrExtension)
  # library(sf)
  # library(dplyr)
  # library(tmap)
  # library(tmaptools)
  # library(osmplotr)
  #   if(FIRST){
  #   ### DEBUG variables
  #   # Defaults
  #   station_labs = TRUE
  #   lab_loc = NULL
  #   scale_pos = 'bottom_left'
  #   # from Example 1
  #   stations <-
  #   sampling_stations[(sampling_stations$NERR.Site.ID == 'elk'
  #   & sampling_stations$Status == 'Active'), ]$Station.Code
  #   to_match <- c('wq', 'met')
  #   stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
  #   shp_fl <- elk_spatial
  #   bounding_elk <- c(-121.810978, 36.868218, -121.708667, 36.764050)
  #   lab_dir <- c('L', 'R', 'L', 'L', 'L')
  #   labs <- c('ap', 'cw', 'nm', 'sm', 'vm')
  #   pos <- 'bottomleft'
  #
  #   ### plot call, reassign variables
  #   ### res_local_map('elk', stations = stns, bbox = bounding_elk,
  #   ###               lab_loc = lab_dir, scale_pos = pos, shp = shp_fl)
  #   nerr_sit_id <- 'elk'
  #   stations <- stns
  #   bbox <- bounding_elk
  #   lab_loc <- lab_dir
  #   scale_pos <- pos
  #   shp <- shp_fl
  # } else {
  #   # ---------------------------------------------------------------------------
  #   # Second Example
  #   # Defaults
  #   station_labs = TRUE
  #   lab_loc = NULL
  #   scale_pos = 'bottom_left'
  #   ## a multicomponent reserve (show two different bounding boxes)
  #   ### set plotting parameters
  #   stations <-
  #   sampling_stations[(sampling_stations$NERR.Site.ID == 'cbm'
  #   & sampling_stations$Status == 'Active'), ]$Station.Code
  #   to_match <- c('wq', 'met')
  #   stns <- stations[grep(paste(to_match, collapse = '|'), stations)]
  #   shp_fl <- cbm_spatial
  #   bounding_cbm_1 <- c(-77.393, 39.741, -75.553, 38.277)
  #   bounding_cbm_2 <- c(-76.862006, 38.811571, -76.596508, 38.642454)
  #   lab_dir <- c('L', 'R', 'L', 'L', 'L')
  #   labs <- c('ap', 'cw', 'nm', 'sm', 'vm')
  #   pos <- 'bottomleft'
  #   #
  #   ### plot
  #   # res_local_map('cbm', stations = stns, bbox = bounding_cbm_1,
  #   # lab_loc = lab_dir, scale_pos = pos, shp = shp_fl)
  #   nerr_sit_id <- 'cbm'
  #   stations <- stns
  #   bbox <- bounding_cbm_2
  #   lab_loc <- lab_dir
  #   scale_pos <- pos
  #   shp <- shp_fl
  # }
  # ===========================================================================

  # check that a shape file exists
  if(class(shp) != 'SpatialPolygons') {
    if(class(shp) != 'sf') {
      stop('shapefile (shp) must be sf (preferred) or SpatialPolygons object')
    }
  } else {
    shp <- methods::as(shp, "sf")   # convert SpatialPolygons to sf
  }

  # check that length(lab_loc) = length(stations)
  if(!is.null(station_labs) && length(lab_loc) != length(stations))
    stop('Incorrect number of label location identifiers specified. R or L designation must be made for each station.' )

  # check that the bb has the right dimensions
  if(is.null(bbox))
    stop('Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  if(length(bbox) != 4)
    stop('Incorrect number of elements specified for bbox. Specify a bounding box (bbox) in the form of c(X1, Y1, X2, Y2)')
  # Get min-max bounding coordinates:
  xminmax <- c(min(bbox[c(1,3)]), max(bbox[c(1,3)]))
  yminmax <- c(min(bbox[c(2,4)]), max(bbox[c(2,4)]))

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
  # convert location info to sf object
  # use lat/lon, WGS84 projection, EPSG:4326.
  loc_sf <- sf::st_as_sf(loc, coords = c("Longitude","Latitude"))
  sf::st_crs(loc_sf) <- 4326
  # Now transform into the projected web-mercator projection EPSG:3857
  #loc_sf <- sf::st_transform(loc_sf, 3857)

  # Plot map
  # m <- leaflet(loc, options = leafletOptions(zoomControl = FALSE), width = 500, height = 500) %>%
  #   addProviderTiles(leaflet::providers$Esri.WorldGrayCanvas) %>%  # Add default OpenStreetMap map tiles, CartoDB.Positron
  #   addPolygons(data = shp, weight = 2, color = '#B3B300', fillColor = 'yellow')
  #
  #library(osmplotr)
  bg_etop <- tmaptools::read_osm(bbox, type = "esri-topo") # sf::st_bbox(shp), type = "osm")
  # bg_bing <- tmaptools::read_osm(bbox, type = "bing")
  m <- tmap::tm_shape(bg_etop) +
    tmap::tm_rgb() +
    tmap::tm_shape(shp) +
    tmap::tm_polygons(lwd = 2, col = 'yellow', alpha = 0.3,
                border.col = '#B3B300', border.alpha = 0.8) +
    tmap::tm_shape(loc_sf) +
    tmap::tm_dots(size = .75, col = "color") +
    tmap::tm_text(text = "abbrev", xmod = "align", just = c("center","top"),
            bg.color = 'white', bg.alpha = 0.75,
            fontface = "bold")


  # tm_shape(shp) +
  #   tm_polygons(lwd = 2, col = 'yellow', alpha = 0.3,
  #               border.col = '#B3B300', border.alpha = 0.8) +
  #   tm_shape(loc_sf) +
  #   tm_dots(size = .75, col = "color") +
  #   tm_text(text = "abbrev", xmod = "align", just = c("center","top"),
  #           bg.color = 'white', bg.alpha = 0.75,
  #           fontface = "bold")

  # if(exists('left_labs')){
  #
  #     addCircleMarkers(lng = ~Longitude[left_labs] * -1, lat = ~Latitude[left_labs], radius = 5
  #                      , weight = 0, fillOpacity = 1
  #                      , color = loc$color[left_labs]
  #                      , label = loc$abbrev[left_labs]
  #                      , labelOptions = labelOptions(noHide = station_labs
  #                                                    , direction = c('left')
  #                                                    , opacity = 1
  #                                                    , offset = c(-5, 0)
  #                                                    , style = label_style))
  # }
  #
  # if(exists('right_labs')){
  #   m <- m %>%
  #     addCircleMarkers(lng = ~Longitude[right_labs] * -1, lat = ~Latitude[right_labs], radius = 5
  #                      , weight = 0, fillOpacity = 1
  #                      , color = loc$color[right_labs]
  #                      , label = loc$abbrev[right_labs]
  #                      , labelOptions = labelOptions(noHide = station_labs
  #                                                    , direction = c('right')
  #                                                    , opacity = 1
  #                                                    , offset = c(5, 0)
  #                                                    , style = label_style))
  # }
  #
  # m <- m %>%
  #   addScaleBar(position = scale_pos) %>%
  #   fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])

  return(m)
}
