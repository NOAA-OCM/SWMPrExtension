#' Identify NERRS sampling locations from metadata
#'
#' Identify the latitude/longitude for sampling stations based on the metadata in the data file
#'
#' @param data.file location of data
#' @param active logical. Only return active stations?
#'
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom SWMPr site_codes
#'
#' @export
#'
#' @details This function is intended for internal use with the NERRS reserve level reporting scripts. It returns the names, station codes, and coordinates associated with the data in the user-specified data folder.
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a dataframe of station ids, station names, lat/long
#'
get_site_coordinates <- function(data.file, active = TRUE){


  if (active){
    res_data <- SWMPr::site_codes()
    res_data <- res_data[res_data$nerr_site_id == get_site_code(data.file) & res_data$status == 'Active', ]
  }else{
    res_data <- SWMPr::site_codes()
    res_data <- res_data[res_data$nerr_site_id == get_site_code(data.file), ]
  }

  sites <- res_data %>%
    dplyr::group_by(.data$nerr_site_id, .data$station_name
             , .data$latitude, .data$longitude) %>%
    dplyr::summarise()

  sites$latitude <- as.numeric(as.character(sites$latitude))
  sites$longitude <- -as.numeric(as.character(sites$longitude))

  return(sites)
}
