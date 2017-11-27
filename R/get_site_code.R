#' Return NERRS reserve site code based on data in the data file
#'
#' Identify the 3- letter NERRS reserve code from metadata in the data file
#'
#' @param data.file data source location
#'
#' @concept reporting
#'
#' @export
#'
#' @details This function returns the 3-letter reserve code associated with the data in the user-specified data file
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns 3-letter, reserve site code as chr
#'
get_site_code <- function(data.file){
  #Check for data
  if (length(list.files(data.file, pattern = '.csv')) == 0) {
    stop('No data files present')
  }

  x <- list.files(path = data.file, pattern = '.csv')
  site_code <- substr(x[1], start = 1, stop = 3)

  return(site_code)
}
