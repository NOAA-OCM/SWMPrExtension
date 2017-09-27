#' Cumulative Bar Plot
#'
#' Cumulative bar plot over a historic range
#'
#' @param swmpr_in input swmpe object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param rng_avg logical, should a longterm average be included on the plot? Defaults to \code{FALSE}
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
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
#' @details Calculated annual bar plot
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}
#'
#' @examples
#' \dontrun{
## get data, prep
#' data(apaebmet)
#' dat <- apaebmet
#'
#' dat <- qaqc(apaebmet, qaqc_keep = c('0', '3', '5'))
#' prcp_plt <- seasonal_barplot(dat, param = 'do_mgl', hist_rng = c(2012, 2013))
#' prcp_plt <- seasonal_barplot(dat, param = 'do_mgl', hist_rng = c(2012, 2013), criteria = 2)
#' }

seasonal_barplot <- function(swmpr_in, ...) UseMethod('seasonal_barplot')

#' @rdname annual_range
#'
#' @concept analyze
#'
#' @export
#'
#' @method annual_range swmpr
#'
seasonal_barplot.swmpr <- function(swmpr_in
                               , param = NULL
                               , target_yr = NULL
                               , rng_avg = FALSE
                               , log_trans = FALSE
                               , ...) {

}
