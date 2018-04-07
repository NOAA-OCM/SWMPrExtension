#' Load and format shapefile for reserve level map
#'
#' Load and format shapefile for use with res_local_map
#'
#' @param path path to shapefile and name
#' @param dissolve_boundaries logical, should reserve boundaries be dissolved? Defaults to \code{TRUE}
#'
#' @importFrom maptools unionSpatialPolygons
#' @importFrom methods slot
#' @importFrom rgdal readOGR
#' @importFrom sp spTransform
#'
#' @export
#'
#' @details This function is intended for internal use with the NERRS reserve level reporting scripts. It loads a NERRS boundary shp file and dissolves unnecessary reserve boundaries. The resulting \code{sp} object is then used with \code{\link{res_sk_map}} and \code{\link{res_local_map}}
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a \code{\link[sp]{sp}} object
#'
load_shp_file <- function(path, dissolve_boundaries = TRUE){

  shp <- rgdal::readOGR(dsn = path) %>%
    sp::spTransform(CRS("+proj=longlat +datum=WGS84"))

  if(dissolve_boundaries) shp <- maptools::unionSpatialPolygons(shp, IDs = rep('x', length(methods::slot(shp, 'polygons'))))

  return(shp)
}
