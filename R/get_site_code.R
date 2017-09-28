#' Return NERRS reserve site code based on data in the data file
#'
#' A helper function for the NERRS automated reporting template
#'
#' @param data.file data source location
#'
#' @concept reporting
#'
#' @export
#'
#' @return Returns reserve site code as chr

get_site_code <- function(data.file){
  #Check for data
  if (length(list.files(data.file, pattern = '.csv')) == 0) {
    stop('No data files present')
  }

  x <- list.files(path = data.file, pattern = '.csv')
  site_code <- substr(x[1], start = 1, stop = 3)

  return(site_code)
}
