#' Identify NERRS reserve stations from metadata
#' 
#' A helper function for the NERRS automated reporting template
#' 
#' @param data.file location of data
#' @param type chr string of data station type (\code{'wq'}, \code{'nut'}, or \code{'met'})
#' @param active logical. Should inactive stations be excluded?
#' 
#' @concept reporting
#' 
#' @import dplyr stringr SWMPr
#' 
#' @export
#' 
#' 
#' @return Returns a character vector of reserve stations

get_sites <- function(data.file, type = c('wq', 'nut', 'met'), active = TRUE){
  
  if (active == TRUE){
    res_data <- SWMPr::site_codes()
    res_data <- res_data[res_data$nerr_site_id == get_site_code(data.file) & res_data$status == 'Active', ]
  }else{
    res_data <- SWMPr::site_codes()
    res_data <- res_data[res_data$nerr_site_id == get_site_code(data.file) & res_data$status == 'Active', ]
  }
  
  sites <- unique(grep(paste(type, collapse = '|')
                       , res_data$station_code, value = TRUE))
  
  return(sites)
}
