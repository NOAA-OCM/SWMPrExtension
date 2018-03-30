#' Identify shapefile for NERRS reserve
#'
#' Identify the shapefile name associated with the reserve in the data file
#'
#' @param gis.file.loc path to gis file location
#'
#' @export
#'
#' @details This function is intended for internal use with the NERRS reserve level reporting scripts. It identifies the name of the shapefile associated with the NERRS reserve.
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a character string of the shapefile for the reserve boundary
#'
get_shp_name <- function(gis.file.loc){
  x <- list.files(path = gis.file.loc, pattern = '.shp$')

  return(x)
}
