#' Local Reserve Map
#'
#' Create a stylized reserve level map for use with the reserve level reporting template
#'
#' @param nerr_site_id chr string of the reserve to make, first three characters used by NERRS
#' @param stations chr string of the reserve stations to include in the map
#' @param bbox a bounding box associated with the reserve
#'
#' @import ggplot2
#'
#' @importFrom ggthemes theme_map
#' @importFrom maptools elide spRbind unionSpatialPolygons
#' @importFrom rgdal readOGR
#' @importFrom rlang .data
#' @importFrom sp CRS bbox proj4string spTransform
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Creates a base map of the US with options for including AK, HI, and PR. The user can also
#' This function was developed from a blog post by Bob Rudis (https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/)
#'
#' @examples
#' \dontrun{
#'
#' ##Just the national map
#'
#' ## a compact reserve
#'
#' ## a multicomponent reserve
#' }

res_local_map <- function(nerr_site_id, stations, bbox) {

}
