#' Set reasonable date breaks
#'
#' Select reasonable breaks for \code{\link[scales]{scale_x_datetime}}
#'
#' @param rng date range years
#'
#' @concept analyze
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @export
#'
#' @details A Helper function for easier date label setting
#'
#' @author Julie Padilla
#'
#' @return a chr string for \code{\link[scales]{date_breaks}}
#'
#' @seealso \code{\link{set_date_break_labs}}
#'
set_date_breaks <- function(rng) {
  if(length(unique(rng)) > 2) {
    brks <- ifelse(diff(rng) > 3, '1 year', '4 months')
  } else {
    brks <- ifelse(length(unique(rng)) > 1, '2 months', '1 month')
  }
}


#' Set reasonable date breaks labels
#'
#' Select reasonable labels for breaks used in \code{\link[scales]{scale_x_datetime}}
#'
#' @param rng date range years
#'
#' @concept analyze
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @export
#'
#' @details A helper function for easier date label setting
#'
#' @author Julie Padilla
#'
#' @return a chr string for \code{\link[scales]{date_labels}}
#'
##' @seealso \code{\link{set_date_breaks}}
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
