#' Identify NERRS reserve from metadata
#'
#' Identify the NERRS reserve from metadata in the data file
#'
#' @param data.file location of data
#' @param active logical. Only return active stations?
#'
#' @concept reporting
#'
#' @import dplyr SWMPr
#'
#' @importFrom rlang .data
#'
#' @export
#'
#'
#' @return Returns a dataframe of station ids, station names, lat/long
#'

get_site_coordinates <- function(data.file, active = TRUE){


  if (active == TRUE){
    res_data <- SWMPr::site_codes()
    res_data <- res_data[res_data$nerr_site_id == get_site_code(data.file) & res_data$status == 'Active', ]
  }else{
    res_data <- SWMPr::site_codes()
    res_data <- res_data[res_data$nerr_site_id == get_site_code(data.file), ]
  }

  sites <- res_data %>%
    group_by(.data$nerr_site_id, .data$station_name
             , .data$latitude, .data$longitude) %>%
    summarise()

  sites$latitude <- as.numeric(as.character(sites$latitude))
  sites$longitude <- -as.numeric(as.character(sites$longitude))

  return(sites)
}
