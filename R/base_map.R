#' Create background map
#'
#' Create background map
#'
#' @param bbox Bounding box vector.
#' @param bg_crs EPSG code for returned map.
#' @param ... Additional arguments to be passed to \code{ggmap::get_stamenmap}
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom sf st_as_sf st_bbox st_crs st_sfc st_transform st_polygon
#' @export
#'
#' @details A helper function to create background map based on based on raster map tiles retrieved with \code{ggmap::get_stamenmap}.  If \code{ggmap} is unavailable, the function creates a basic map using county-level polygon files.  This map is fairly crude and should be considered a placeholder.
#'
#' @author Dave Eslinger
#'
#' @concept miscellaneous
#'
#' @return Returns a \code{ggplot2} object.
#'
base_map <- function(bbox, bg_crs = 4326, ...) {
  if (requireNamespace("ggmap", quietly=TRUE)) {

    bg_map <- ggmap::get_stamenmap(bbox,
                                   ...,
                                   source = "stamen",
                                   messaging = FALSE,
                                   crop = TRUE,
                                   force = FALSE,
                                   urlonly = FALSE)
    map <- ggmap::ggmap(bg_map, alpha = 0.5)
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
