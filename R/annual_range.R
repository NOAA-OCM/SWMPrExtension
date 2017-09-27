#' Annual Range Timeseries
#'
#' Annual time series for year of interest on top of long-term percentiles
#'
#' @param swmpr_in input swmpe object
#' @param param chr string of variable to plot
#' @param target_yr numeric, the target year that should be compared against the historic range. If target year is not specified then dot will not be plotted
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
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
#' @concept analyze
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}
#'
#'

annual_range <- function(swmpr_in, ...) UseMethod('annual_range')

#' @rdname annual_range
#'
#' @concept analyze
#'
#' @export
#'
#' @method seasonal_boxplot swmpr
#'
annual_range.swmpr <- function(swmpr_in
                                   , param = NULL
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

  rng <- target_yr

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
    warning('No target year specified. Entire time series will be used.')
    rng <- max(lubridate::year(dat$datetimestamp))
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

  sn <- ifelse(length(levels(dat_hist$season)) == 12, 'Monthly', 'Seasonal')
  bp_fill <- paste(rng, ' ', sn, ' Average', sep = '')

  dat_day <- df %>%
    group_by(station, month_name, datetimestamp = floor_date(datetimestamp, unit = 'day')) %>%
    summarise(mean = mean(result, na.rm = T)
              , min = min(result, na.rm = T)
              , max = max(result, na.rm = T))

  dat_month <- dat_day %>%
    group_by(station, month_name) %>%
    summarise(mean = mean(mean, na.rm = T)
              , min_avg = mean(min, na.rm = T)
              , max_avg = mean(max, na.rm = T)
              , min = min(min, na.rm = T)
              , max = max(max, na.rm = T))

  plt <-
    ggplot(data = dat_month, aes(x = month_name, y = mean, group = 1)) +
    geom_ribbon(aes(x = month_name, ymax = max_avg, ymin = min_avg)
                , fill = 'steelblue3', alpha = 0.25) +
    geom_ribbon(aes(x = month_name, ymax = max, ymin = min, group = 1)
                , fill = 'steelblue3', alpha = 0.15) +
    geom_line(lwd = 1, color = 'steelblue3') +
    geom_point(shape = 21, fill = 'steelblue3') +
    # geom_hline(yintercept = 2, linetype = 'dashed', color = 'red4') +
    scale_y_continuous(limits = c(mn, mx), trans = y_axis_trans, labels = comma) +
    facet_wrap(~ station, ncol = 2) +
    theme_bw()

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
