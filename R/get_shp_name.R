#' Identify shapefile for NERRS reserve
#' 
#' Identify the shapefile name associated with the reserve in the data file
#' 
#' @param gis.file.loc path to gis file location
#' 
#' @concept reporting
#' 
#' @import dplyr SWMPr
#' 
#' @export
#' 
#' 
#' @return Returns a character string of the full reserve name
#'

get_shp_name <- function(gis.file.loc){
  x <- list.files(path = gis.file.loc, pattern = '.shp$')
  
  return(x)
}