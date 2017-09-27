#' Annual Range Timeseries
#'
#' Monthly average and variability. Looking at variability within each month; no historical context
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
#' @examples
#' \dontrun{
#' ## get data, prep
#' data(apacpwq)
#' dat <- apacpwq
#'
#' dat <- qaqc(dat)
#' annual_range(dat, param = 'do_mgl', target_yr = 2012)
#' }

annual_range <- function(swmpr_in, ...) UseMethod('annual_range')

#' @rdname annual_range
#'
#' @concept analyze
#'
#' @export
#'
#' @method annual_range swmpr
#'
annual_range.swmpr <- function(swmpr_in
                                   , param = NULL
                                   , target_yr = NULL
                                   , criteria = NULL
                                   , log_trans = FALSE
                                   , ...) {

  dat <- swmpr_in
  parm <- sym(param)

  seas <- sym('season')
  res <- sym('result')
  dt <- sym('date')
  avg <- sym('mean')
  mini <- sym('min')
  maxi <- sym('max')
  mini_avg <- sym('min_avg')
  maxi_avg <- sym('max_avg')

  rng <- target_yr

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  #determine type WQ, MET, NUT
  #determine log scale transformation
  if(substr(station, 6, nchar(station)) == 'nut')
    warning('Nutrient data detected. Consider specifying seasons > 1 month. See `?assign_season` for details.')

  #determine historical range exists, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No target year specified. Entire time series will be used.')
    rng <- max(lubridate::year(dat$datetimestamp))
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine target year (if there is one)
  if(is.null(target_yr))
    stop('No target year provided')

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

  dat_day <- dat %>%
    group_by(!! seas, !! dt) %>%
    summarise(mean = mean(!! parm, na.rm = T)
              , min = min(!! parm, na.rm = T)
              , max = max(!! parm, na.rm = T))

  dat_month <- dat_day %>%
    group_by(!! seas) %>%
    summarise(mean = mean(!! avg, na.rm = T)
              , min_avg = mean(!! mini, na.rm = T)
              , max_avg = mean(!! maxi, na.rm = T)
              , min = min(!! mini, na.rm = T)
              , max = max(!! maxi, na.rm = T))

  mx <- max(dat_day$max, na.rm = T)
  mx <- ceiling(mx)
  mn <- ifelse(log_trans == TRUE, 0.1, 0)

  # bp_fill <- paste(rng, ' Daily Averages', sep = '') # need to add in flex for 'min', 'max'

  plt <-
    ggplot(data = dat_month, aes_(x = seas, y = mean, group = 1)) +
    geom_ribbon(aes_(x = seas, ymax = maxi_avg, ymin = mini_avg)
                , fill = 'steelblue3', alpha = 0.25) +
    geom_ribbon(aes_(x = seas, ymax = maxi, ymin = mini, group = 1)
                , fill = 'steelblue3', alpha = 0.15) +
    geom_line(lwd = 1, color = 'steelblue3') +
    geom_point(shape = 21, fill = 'steelblue3') +
    scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = comma) +
    labs(x = '', y = '') +
    theme_bw() +
    theme(legend.position = 'top'
          , legend.direction = 'horizontal')

  # Add criteria line if specified
  if(!is.null(criteria)) {

    plt <- plt +
      geom_hline(aes(yintercept = criteria, color = factor('WQ Threshold'), linetype = factor('WQ Threshold'))
                 , show.legend = T) +
      scale_color_manual('', values = c('WQ Threshold' = 'red')) +
      scale_linetype_manual('', values = c('WQ Threshold' = 'longdash'))

    plt <- plt + guides(fill = guide_legend(order = 1)
                    , shape = guide_legend(order = 2, override.aes = list(linetype = 0))
                    , 'WQ Threshold' = guide_legend(order = 3))


  }

  return(plt)
}
