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
#' @details Creates a base map of the US with options for including AK, HI, and PR. The user can also
#' This function was developed from a blog post by Bob Rudis (https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/)
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a character string of the shapefile for the reserve boundary
#'
load_shp_file <- function(path, dissolve_boundaries = T){

  shp <- rgdal::readOGR(dsn = path) %>%
    sp::spTransform(CRS("+proj=longlat +datum=WGS84"))

  if(dissolve_boundaries) shp <- maptools::unionSpatialPolygons(shp, IDs = rep('x', length(methods::slot(shp, 'polygons'))))

  return(shp)
}
