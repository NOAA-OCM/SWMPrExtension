#' Seasonal boxplots
#'
#' Annual time series for year of interest on top of long-term percentiles
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param target_yr numeric, the target year that should be compared against the historic range. If target year is not specified then dot will not be plotted
#' @param thresholds numeric vector, numeric criteria that will be plotted in the background
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @concept analyze
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year floor_date
#'
#' @export
#'
#' @details Annual time series for year of interest on top of long-term percentiles
#'
#' @author Julie Padilla
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#'

threshold_plot <- function(swmpr_in, ...) UseMethod('threshold_plot')

#' @rdname threshold_plot
#'
#' @concept analyze
#'
#' @export
#'
#' @method threshold_plot swmpr
#'
threshold_plot.swmpr <- function(swmpr_in
                                   , param = NULL
                                   , hist_rng = NULL
                                   , target_yr = NULL
                                   , thresholds = NULL
                                   , log_trans = FALSE
                                   , ...) {




}
