#' Load and format shapefile for reserve level map
#'
#' Load and format shapefile for use with res_local_map. If polygons are dissolved, the only attribute returned will be a count of the number of grouped polygons, otherwise, all attributes are retained.
#'
#' @param path path to shapefile and name
#' @param dissolve_boundaries logical, should reserve boundaries be dissolved? Defaults to \code{TRUE}
#'
#' @importFrom dplyr group_by mutate summarise
#' @importFrom magrittr "%>%"
#' @importFrom methods slot
#' @importFrom rlang .data
#' @importFrom sf read_sf st_transform
#'
#' @export
#'
#' @details This function is intended for internal use with the NERRS reserve level reporting scripts. It loads a NERRS boundary shp file and dissolves unnecessary reserve boundaries. The resulting \code{sf} object is then used with \code{\link{res_sk_map}} and \code{\link{res_local_map}}
#'
#' @author Julie Padilla, Dave Eslinger
#'
#' @concept reporting
#'
#' @return Returns a \code{\link[sf]{sf}} object
#'
load_shp_file <- function(path, dissolve_boundaries = TRUE){

  shp <- sf::read_sf(dsn = path) %>%
    sf::st_transform(4269)

  if(dissolve_boundaries) shp <- shp %>%
      # create new field, merge on it, and count merged poygons
      dplyr::mutate(polyval = 1) %>%
      dplyr::group_by(.data$polyval) %>%
      dplyr::summarise(polys_grouped = sum(.data$polyval))

  return(shp)
}
