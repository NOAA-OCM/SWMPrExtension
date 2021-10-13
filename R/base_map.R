#' Create background map
#'
#' Create a background map from a bounding box using Stamen Map tiles or a crude
#' vector-based map.
#'
#' @param bbox Bounding box vector.
#' @param bg_crs EPSG code or \code{st_crs} object for the returned map.
#' @param vector_only Logical, draw only a simple vector-based map.
#' @param maptype Background map type from Stamen Maps
#'   (\url{http://maps.stamen.com/}); one of c("terrain", "terrain-background",
#'   "terrain-labels", "terrain-lines", "toner", "toner-2010", "toner-2011",
#'   "toner-background", "toner-hybrid", "toner-labels", "toner-lines",
#'   "toner-lite", "watercolor").
#' @param zoom Zoom level for the base map created when \code{bg_map} is not
#'   specified.  An integer value, 5 - 15, with higher numbers providing  more
#'   detail.  If not provided, a zoom level is autoscaled based on \code{bbox}
#'   parameters.
#' @param ... Additional arguments to be passed to \code{ggmap::get_stamenmap}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom sf st_as_sf st_bbox st_crs st_sfc st_transform st_polygon
#' @export
#'
#' @details A helper, or stand-alone, function to create background map based on
#'   based on raster map tiles retrieved with \code{ggmap::get_stamenmap}.  If
#'   \code{ggmap} is unavailable, the function creates a basic map using
#'   county-level polygon files.  This map is fairly crude and should be
#'   considered a placeholder.
#'
#' @author Dave Eslinger
#'
#' @concept analyze
#'
#' @return Returns a \code{ggplot2} object.
#'
#' @examples
#'
#' # Simple, low-zoom  map for testing
#' bound_box <- c(-77.393, 38.277, -75.553, 39.741)
#' x <- base_map(bound_box, zoom = 7)
#'
#' \donttest{
#' # Default zoom map with terrain maptype.
#' y <- base_map(bound_box, maptype = 'terrain')
#' }


base_map <- function(bbox, bg_crs = 4326,
                     vector_only = FALSE,
                     maptype = 'toner-lite',
                     zoom = NULL,
                     ... ) {

  if (requireNamespace("ggmap", quietly=TRUE) & !vector_only) {
    # Set background map zoom level automatically if not specified
    if(is.null(zoom)) {
      xmin <- min(bbox[c(1,3)])
      xmax <- max(bbox[c(1,3)])
      ymin <- min(bbox[c(2,4)])
      ymax <- max(bbox[c(2,4)])
      diag_size <- sqrt((xmax-xmin)^2 +(ymax-ymin)^2)
      zoom <- 14 - ceiling(sqrt(10*diag_size))
      print(paste("Zoom level calculated as", zoom, sep = " "))
    }

    bg_map <- ggmap::get_stamenmap(bbox,
                                   maptype = maptype,
                                   zoom = zoom,
                                   ...,
                                   source = "stamen",
                                   messaging = FALSE,
                                   crop = TRUE,
                                   force = FALSE,
                                   urlonly = FALSE)
    map <- ggmap::ggmap(bg_map, alpha = 0.5) +
      coord_sf(
        xlim = c(bbox[1], bbox[3]),
        ylim = c(bbox[2], bbox[4]),
        expand = FALSE,
        crs = bg_crs,
        default_crs = NULL,
        datum = sf::st_crs(4326),
        # label_graticule = waiver(),
        # label_axes = waiver(),
        lims_method = c("cross", "box", "orthogonal", "geometry_bbox"),
        ndiscr = 100,
        default = FALSE,
        clip = "on"
      )
  } else {
    warning("ggmap not available, creating vector map")
    counties_4269 <- NULL
    # Create polygon from bounding box. ----
    bb_to_poly <- function(bb){
      polbb <- sf::st_sfc(sf::st_polygon(
        list(cbind(c(bb[1],bb[3],bb[3],bb[1],bb[1]),
                   c(bb[2],bb[2],bb[4],bb[4],bb[2])))))
      pol_sf <- sf::st_sf(bbox = 1, polbb)
      return(pol_sf)
    }
    background <- bb_to_poly(bbox)
    sf::st_crs(background) <- 4326
    if(bg_crs != 4326) {
      bg_proj <- st_transform(background, bg_crs)
    } else {
      bg_proj <- background
    }
    load("./data/counties_4269.rda")
    land <- sf::st_crop(st_transform(counties_4269,
                                     sf::st_crs(bg_proj)),bg_proj)
    map <- ggplot() +
      geom_sf(data = bg_proj, aes(), fill = "gray90") +
      geom_sf(data = land, aes(), fill = "gray70")
  }
  return(map)
}
