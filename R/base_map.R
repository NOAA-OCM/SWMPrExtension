#' Create background map
#'
#' Create background map based on OpenStreetMap vector layers.
#'
#' @param bbox Bounding box vector,
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom osmdata add_osm_features opq osmdata_sf
#' @importFrom sf st_as_sf st_bbox st_crs st_sfc st_transform st_polygon
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
base_map <- function(bbox, bg_crs) {


  # Create polygon from bounding box. ----
  bb_to_poly <- function(bb){
    pol = st_sfc(st_polygon(
      list(cbind(c(bb[1],bb[3],bb[3],bb[1],bb[1]),
                 c(bb[2],bb[2],bb[4],bb[4],bb[2])))))
    pol_sf = st_sf(r = 5, pol)
    return(pol_sf)
  }

  # Define osm desired features and keys ----
  land_list <- c("\"highway\"=\"primary\"")

  nat_water_list <- c("\"natural\"=\"water\"",
             "\"natural\"=\"wetland\"",
             "\"natural\"=\"beach\"",
             "\"natural\"=\"bay\"")
  water_list <- c("\"water\"=\"river\"",
                  "\"natural\"=\"water\"",
                  "\"natural\"=\"bay\"",
                  "\"place\"=\"sea\"",
                  "\"place\"=\"ocean\"",
                  "\"water\"=\"lagoon\"")

  road_list <- c("\"highway\"=\"motorway\"",
             "\"highway\"=\"trunk\"",
             "\"highway\"=\"primary\"",
             "\"highway\"=\"secondary\"")

  boundaries_list <- c("\"natural\"=\"coastline\"",
                       "\"boundary\"=\"administrative\""
                  )

  roads <- opq(bbox = bbox) %>%
    add_osm_features(features = road_list) %>%
    osmdata_sf()

  boundaries <- opq(bbox = bbox) %>%
    add_osm_features(features = boundaries_list) %>%
    osmdata_sf()

  water <- opq(bbox = bbox) %>%
    add_osm_features(features = water_list) %>%
    osmdata_sf()

  # polys <- boundaries$osm_polygons
  # mpolys <- boundaries$osm_multipolygons
  #
  # lines <- boundaries$osm_lines
  # mlines <- vecs$osm_multilines

  background <- bb_to_poly(bbox)
  st_crs(background) <- 4269
  bg_proj <- st_transform(background, bg_crs)

  map <- ggplot() +
    geom_sf(data = background, aes(), fill = "grey") +
    geom_sf(data = water$osm_polygons, aes(), fill = "lightblue") +
  geom_sf(data = roads$osm_lines, aes(), size = 1.0, color = "darkgray") +
    # geom_sf(data = boundaries$osm_lines, aes(), color = "black")
    geom_sf(data = boundaries$osm_multipolygons, aes(), fill = "lightblue", color = "lightblue", alpha = 0.3) +
    # geom_sf(data = boundaries$osm_polygons, aes(fill = natural))

  #   coord_sf
  coord_sf(
    xlim = c(bbox[1], bbox[3]),
    ylim = c(bbox[2], bbox[4]),
    expand = FALSE,
    crs = bg_crs,
    default_crs = NULL,
    datum = sf::st_crs(4326),
    label_graticule = waiver(),
    label_axes = waiver(),
    lims_method = c("cross", "box", "orthogonal", "geometry_bbox"),
    ndiscr = 100,
    default = FALSE,
    clip = "on"
  )


  return(map)
}
