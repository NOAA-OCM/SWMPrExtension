#' Create background map
#'
#' Create background map based on OpenStreetMap vector layers.
#'
#' @param bbox Bounding box vector,
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom osmdata add_osm_features opq osmdata_sf
#' @importFrom sf st_as_sf st_bbox st_crs st_transform
#' @export
#'
#' @details A helper function to create background map based on OpenStreetMap vector layers.
#'
#' @author Dave Eslinger
#'
#' @concept miscellaneous
#'
#' @return returns a \code{\link[ggplot2]} object
#'
#'
bg_map <- function(bbox) {
  feats <- c("\"natural\"=\"coastline\"",
             "\"natural\"=\"water\"",
             "\"natural\"=\"wetland\"",
             "\"natural\"=\"beach\"",
             "\"natural\"=\"bay\"",
             "\"highway\"=\"primary\"")
  vecs <- opq(bbox = bbox) %>%
    # add_osm_feature(key = 'nature', value = "coastline", value_exact = FALSE) %>%
    add_osm_features(features = feats) %>%
    osmdata_sf()
  vecs

  # polys <- vecs$osm_polygons
  # mpolys <- vecs$osm_multipolygons
  #
  # lines <- vecs$osm_lines
  # mlines <- vecs$osm_multilines


  map <- ggplot() +
    # geom_sf(data = vecs$osm_multipolygons, aes(fill = natural)) +
    # geom_sf(data = vecs$osm_polygons, aes(fill = natural)) +
    geom_sf(data = vecs$osm_lines, aes(color = natural))


  ggplot() +
    geom_sf(data = county_pop, aes(fill = totpop))

}
