#' Identify NERRS reserve from metadata
#'
#' Identify the NERRS reserve from metadata in the data file
#'
#' @param data.file location of data
#'
#' @export
#'
#' @details This function is intended for internal use with the NERRS reserve level reporting scripts. It determines the name of the full name of the NERRS reserve associated with the data in the user-specified data folder.
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a character string of the full reserve name
#'
get_reserve <- function(data.file){

  #Check for data
  if (length(list.files(data.file, pattern = '.csv')) == 0) {
    stop('No data files present')
  }

  res_data <- get('sampling_stations')

  res_data <- res_data[res_data$NERR.Site.ID == get_site_code(data.file), ]

  reserve <- as.character(res_data$Reserve.Name[1])

  return(reserve)
}
