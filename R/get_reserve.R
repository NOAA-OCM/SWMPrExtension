#' Identify NERRS reserve from metadata
#' 
#' Identify the NERRS reserve from metadata in the data file
#' 
#' @param data.file location of data
#' 
#' @concept reporting
#' 
#' @import dplyr stringr
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
#' 
#' 
#' @return Returns a character string of the full reserve name

get_reserve <- function(data.file){
  
  #Check for data
  if (length(list.files(data.file, pattern = '.csv')) == 0) {
    stop('No data files present')
  }
  
  res_data <- SWMPr::site_codes()

  res_data <- res_data[res_data$nerr_site_id == get_site_code(data.file), ]
  
  reserve <- res_data$reserve_name[1] %>% as.character
  
  return(reserve)
}