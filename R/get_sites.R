#' Identify NERRS reserve stations from metadata
#'
#' Identify the NERRS reserve sampling stations based on the metadata in the data file
#'
#' @param data.file location of data
#' @param type chr string of data station type (\code{'wq'}, \code{'nut'}, or \code{'met'})
#' @param active logical. Should inactive stations be excluded? Defaults to \code{TRUE}
#' @param primary logical. Should non-primary stations be excludes? Defaults to \code{TRUE}
#'
#' @importFrom SWMPr site_codes
#'
#' @export
#'
#' @details This function returns the sampling stations associated with the data in the user-specified data file
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a character vector of reserve stations
#'
get_sites <- function(data.file, type = c('wq', 'nut', 'met'), active = TRUE, primary = T){

  res_data <- get('sampling_stations')

  if (active == TRUE){
    res_data <- res_data[res_data$NERR.Site.ID == get_site_code(data.file) & res_data$Status == 'Active', ]
  }else{
    res_data <- res_data[res_data$NERR.Site.ID == get_site_code(data.file), ]
  }

  if (primary == TRUE){
    res_data <- res_data[res_data$NERR.Site.ID == get_site_code(data.file) & res_data$isSWMP == 'Active', ]
  }else{
    res_data <- res_data[res_data$NERR.Site.ID == get_site_code(data.file), ]
  }


  sites <- unique(grep(paste(type, collapse = '|')
                       , res_data$station_code, value = TRUE))

  return(sites)
}
