#' Seasonal boxplots of raw data
#'
#' Boxplots of raw data by user-defined season for a target year
#'
#' @param swmpr_in input swmp object
#' @param param chr string of variable to plot
#' @param target_yr numeric, if target year is not specified then dot will not be plotted. If target year is not specified the most recent year in the \code{swmpr_in} will be used.
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}} and \code{\link{y_labeler}}.
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @details This function produces boxplots of raw, unaggregated data by user-specified season for
#' Annual time series for year of interest
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}, \code{\link{y_labeler}}
#'
raw_boxplot <- function(swmpr_in, ...) UseMethod('raw_boxplot')

#' @rdname raw_boxplot
#'
#' @concept analyze
#'
#' @export
#'
#' @method raw_boxplot swmpr
#'
raw_boxplot.swmpr <- function(swmpr_in
                              , param = NULL
                              , target_yr = NULL
                              , criteria = NULL
                              , log_trans = FALSE
                              , converted = FALSE
                              , plot_title = FALSE
                              , ...) {

  dat <- swmpr_in
  parm <- sym(param)
  conv <- converted

  rng <- target_yr

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #CHECKS
  #determine historical range exists, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No target year specified. Maximum year in data set will be used.')
    rng <- max(lubridate::year(dat$datetimestamp))
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine type WQ, MET, NUT
  #IF WQ or MET then use "Instantaneous data" otherwise "Monthly data"
  #determine data type
  if(substr(station, 6, nchar(station)) == 'nut') {
    warning('Nutrient data detected. Consider specifying seasons > 1 month.')
    data_type = 'Data'
  } else {
    data_type = 'Instantaneous Data'
  }

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  #determine parameter column index
  parm_index <- grep(param, colnames(dat))

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

  # Assign the seasons and order them
  dat$season <- assign_season(dat$datetimestamp, abb = T, ...)

  mx <- max(dat[, parm_index], na.rm = T)
  mx <- ceiling(mx)
  mn <- ifelse(log_trans == TRUE, 0.1, 0)

  bp_fill <- paste(rng, ' ', data_type, sep = '')

  seas <- sym('season')

  plt <- ggplot(data = dat, aes_(x = seas, y = parm, fill = factor(bp_fill))) +
    geom_boxplot(outlier.size = 0.5) +
    scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = scales::comma) +
    scale_fill_manual(name = '', values = c('skyblue1')) +
    labs(x = NULL, y = eval(y_label)) +
    theme_bw() +
    theme(legend.position = 'top'
          , legend.direction = 'horizontal')


  if(!is.null(criteria)) {

    plt <- plt +
      geom_hline(aes(yintercept = criteria, color = factor('WQ Threshold'), linetype = factor('WQ Threshold'))
                  , show.legend = T) +
      scale_color_manual('', values = c('WQ Threshold' = 'red')) +
      scale_linetype_manual('', values = c('WQ Threshold' = 'longdash'))

    plt <- plt + guides(fill = guide_legend(order = 1)
                    , 'WQ Threshold' = guide_legend(order = 2))


  }

  # add plot title if specified
  if(plot_title) {
    ttl <- title_labeler(nerr_site_id = station)

    plt <-
      plt +
      ggtitle(ttl) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  return(plt)
}
