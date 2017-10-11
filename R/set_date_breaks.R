#' Set reasonable date breaks
#'
#' Select reasonable breaks for scale_x_datetime
#'
#' @param rng date range years
#'
#' @concept analyze
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @export
#'
#' @details Annual time series for year of interest on top of long-term percentiles
#'
#' @author Julie Padilla
#'
#'
#'
set_date_breaks <- function(rng) {
  if(length(unique(rng)) > 2) {
    brks <- ifelse(diff(rng) > 3, '1 year', '4 months')
    # lab_brks <- ifelse(diff(rng) > 3, '%Y', '%b-%y')
  } else {
    brks <- ifelse(length(unique(rng)) > 1, '2 months', '1 month')
    # lab_brks <- '%b-%y'
  }
}


#' Set reasonable date breaks labels
#'
#' Select reasonable labels for breaks used in scale_x_datetime
#'
#' @param rng date range years
#'
#' @concept analyze
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @export
#'
#' @details Annual time series for year of interest on top of long-term percentiles
#'
#' @author Julie Padilla
#'
#'
#'
set_date_break_labs <- function(rng) {
  if(length(unique(rng)) > 2) {
    # brks <- ifelse(diff(rng) > 3, '1 year', '4 months')
    lab_brks <- ifelse(diff(rng) > 3, '%Y', '%b-%y')
  } else {
    # brks <- ifelse(length(unique(rng)) > 1, '3 months', '1 month')
    lab_brks <- ifelse(length(unique(rng)) > 1, '%b-%y', '%b')
  }
}
