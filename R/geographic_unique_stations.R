#' Return a vector of geographically unique NERR Stations
#'
#' Creates a alphabetically sorted, vector of geographically unique stations for mapping
#'
#' @param nerr_site_id chr vector of valid NERR stations
#'
#' @export
#'
#' @details This function takes a vector of NERR stations and only returns geographically unique locations. Intended for use with \code{\link{res_local_map}}
#'
#' @author Julie Padilla
#'
#' @concept mapping
#'
#' @return returns a vector of NERR stations
#'
#' @examples
#' \dontrun{
#' stns <- c('apacpnut', 'apacpwq', 'apadbnut', 'apadbwq', 'apaebmet',
#' 'apaebnut', 'apaebwq', 'apaesnut', 'apaeswq')
#'
#' geographic_unique_stations(stns)
#' }
#'

geographic_unique_stations <- function(nerr_site_id) {

  loc <- get('sampling_stations')

  # generate location labels
  loc <- loc[(loc$Station.Code %in% nerr_site_id), ]
  loc$abbrev <- substr(loc$Station.Code, start = 1, stop = 5)

  loc <- loc[!duplicated(loc[ , c('abbrev', 'Longitude', 'Latitude')]),]#unique(loc[, c('abbrev', 'Longitude', 'Latitude'), ])

  loc <- sort(loc$Station.Code)

  return(loc)
}
