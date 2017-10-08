#' Generate Plot Title Based on NERR Site ID
#'
#' Generate a plot title based on SWMP station abbreviation
#'
#' @param nerr_site_id chr string of NERR site id
#'
#' @export
#'
#' @details A helper function used internally by several plotting functions to generate plot titles.
#'
#' @return Returns character vector
#'
title_labeler <- function(nerr_site_id) {

  abbrev <- substr(nerr_site_id, 4, 5)

  dat_locs <- get('sampling_stations')

  dat_locs$Station.Code <- trimws(dat_locs$Station.Code)

  ttl <- dat_locs[dat_locs$Station.Code == nerr_site_id, ]$Station.Name

  ttl <- trimws(ttl)

  ttl <- paste(ttl, ' (', abbrev, ')', sep = '')

  # ttl <- 'fake title'

  return(ttl)
}
