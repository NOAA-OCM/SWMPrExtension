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

  # check for valid nerr_site_ids
  if(!(nerr_site_id %in% loc$Station.Code))
    stop("Station does not appear to be a valid NERR sampling station. Inspect data(sampling_stations) for details.")

  # generate location labels
  loc <- loc[(loc$Station.Code %in% nerr_site_id), ]
  loc$abbrev <- substr(loc$Station.Code, start = 1, stop = 5)

  loc <- unique(loc[, c('abbrev', 'Longitude', 'Latitude')])

  loc <- loc[order(loc$abbrev), ]

  loc <- loc[[1]]

  return(loc)
}
