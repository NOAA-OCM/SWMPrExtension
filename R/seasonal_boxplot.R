#' Seasonal boxplots
#'
#' Annual time series for year of interest on top of long-term percentiles
#'
#' @param swmpr_in input swmpe object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param target_yr numeric, the target year that should be compared against the historic range. If target year is not specified then dot will not be plotted
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param FUN function used to aggregate daily SWMP data
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
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#'

seasonal_boxplot <- function(swmpr_in, ...) UseMethod('seasonal_boxplot')

#' @rdname seasonal_boxplot
#'
#' @concept analyze
#'
#' @export
#'
#' @method seasonal_boxplot swmpr
#'
seasonal_boxplot.swmpr <- function(swmpr_in
                                   , param = NULL
                                   , hist_rng = NULL
                                   , target_yr = NULL
                                   , criteria = NULL
                                   , log_trans = FALSE
                                   , FUN = function(x) mean(x, na.rm = T)
                                   , ...) {

  dat <- swmpr_in
  parm <- sym(param)

  seas <- sym('season')
  res <- sym('result')
  dt <- sym('date')
  avg <- sym('mean')

  rng <- hist_rng

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  #determine type WQ, MET, NUT
  #determine log scale transformation
  if(substr(station, 6, nchar(station)) == 'nut')
    warning('Nutrient data detected. Consider specifying seasons > 1 month.')

  #determine historical range exists, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No historical range specified. Minimum and maximum year in data set will be used.')
    rng <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine target year (if there is one)
  if(!is.null(target_yr))
    warning('No target year provided')

  #determine y axis transformation
  y_trans <- ifelse(log_trans, 'log10', 'identity')

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  # Assign the seasons and order them
  dat$season <- assign_season(dat$datetimestamp, abb = T, ...)

  # Assign date for determining daily stat value
  dat$date <- lubridate::floor_date(dat$datetimestamp, unit = 'days')

  # Filter for parameter of interest
  dat <- dat %>% dplyr::select(.data$datetimestamp, date, .data$season, !!parm)

  ##historic range
  dat_hist <- dat %>% dplyr::filter(lubridate::year(.data$datetimestamp) >= rng[[1]]
                                    & lubridate::year(.data$datetimestamp) <= rng[[2]])

  dat_hist <- dat_hist %>%
    group_by(!! seas, !! dt) %>%
    summarise(result = FUN(!! parm))

  mx <- max(dat_hist$result, na.rm = T)
  mx <- ceiling(mx)
  mn <- ifelse(log_trans == TRUE, 0.1, 0)

  # sn <- ifelse(length(levels(dat_hist$season)) == 12, 'Month', 'Season')
  bp_fill <- paste(hist_rng[[1]], '-', hist_rng[[2]], ' Daily Average', sep = '')

  # res = sym('result')

  x <- ggplot(data = dat_hist, aes_(x = seas, y = res, fill = factor(bp_fill))) +
    geom_boxplot(outlier.size = 0.5) +
    scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = scales::comma) +
    scale_fill_manual(name = '', values = c('skyblue1')) +
    labs(x = '', y = '') +
    theme_bw() +
    theme(legend.position = 'top'
          , legend.direction = 'horizontal')

  # Add target year dots if specified
  if(!is.null(target_yr)) {
    dat_yr <- dat %>% dplyr::filter(lubridate::year(.data$datetimestamp) == target_yr)

    dat_yr <- dat_yr %>%
      dplyr::group_by(!! seas, !! dt) %>%
      dplyr::summarise(result = FUN(!! parm)) %>%
      dplyr::group_by(!! seas) %>%
      dplyr::summarise(mean = mean(.data$result, na.rm = T))

    pt_fill <- paste(target_yr, ' Average Daily Average', sep = '')

    x <- x +
      geom_point(data = dat_yr, aes_(x = seas, y = avg, shape = factor(pt_fill)), fill = 'red', size = 2) +
      scale_shape_manual(name = '', values = c(21))
  }

  # Add criteria line if specified
  if(!is.null(criteria)) {

    x <- x +
      geom_hline(aes(yintercept = criteria, color = factor('WQ Threshold'), linetype = factor('WQ Threshold'))
                  , show.legend = T) +
      scale_color_manual('', values = c('WQ Threshold' = 'red')) +
      scale_linetype_manual('', values = c('WQ Threshold' = 'longdash'))

    x <- x + guides(fill = guide_legend(order = 1)
                    , shape = guide_legend(order = 2, override.aes = list(linetype = 0))
                    , 'WQ Threshold' = guide_legend(order = 3))


  }

  return(x)
}
