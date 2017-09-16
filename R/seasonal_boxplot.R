#' Assign seasons to SWMP sampling data
#'
#' Annual time series for year of interest on top of long-term percentiles
#'
#'
#'
#' @param swmpr_in input swmpe object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then range will default to the minimum/maximum year in \code{swmpr_in}
#' @param target_yr numeric, if target year is not specified then dot will not be plotted
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
#' @param season
#' @param season_name
#' @param FUN
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @import ggplot2 dplyr lubridate magrittr
#'
#'
#' @export seasonal_boxplot
#'
#' @method seasonal_boxplot swmpr
#'
#' @details
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
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
                                   , log_trans = FALSE
                                   , FUN = function(x) mean(x, na.rm = TRUE)
                                   , ...) {

  dat <- swmpr_in
  func <- FUN
  parm <- as.name(param)
  parm <- enquo(parm)

  # seas <- season # maybe remove?
  # seas_nm <- season_name # maybe remove?

  rng <- hist_rng

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  #determine type WQ, MET, NUT

  #determine historical range exists, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No historical range specified. Minimum and maximum year in data set will be used.')
    rng <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
  }


  #determine that variable name exists
  if(!any(!!parm %in% parameters) & !is.null(params))
    stop('Param argument must name input column')

  #determine target year (if there is one)

  #determin log scale transformation
  if(substr(station, 6, nchar(station)) == 'nut')
    warning('Nutrient data detected. Consider specifying seasons > 1 month.')

  #determine y axis transformation
  y_trans <- ifelse(log_trans, 'log10', 'identity')

  #determine seasons (must be at least two)

  #determine that season names equal seasons length

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  #logic to allow for mean/min/max analysis only?

  # Assign the seasons and order them
  dat$season <-
    # assign_season(dat$datetimestamp, season = seas, season_name = seas_nm)
    assign_season(dat$datetimestamp, abb = T, ...)

  # Assign date for determining daily stat value
  dat$date <- lubridate::floor_date(dat$datetimestamp, unit = 'days')

  # Filter for parameter of interest
  dat <- dat %>% dplyr::select(datetimestamp, date, season, !!parm)

  ##historic range
  dat_hist <- dat %>% dplyr::filter(lubridate::year(datetimestamp) >= rng[[1]]
                                    & lubridate::year(datetimestamp) <= rng[[2]])

  dat_hist <- dat_hist %>%
    dplyr::group_by(season, date) %>%
    # dplyr::summarise(result = mean(do_mgl, na.rm = T))
    dplyr::summarise(result = func(!!parm))

  mx <- max(dat_hist$result, na.rm = T) #%>% round_any(., 1)
  mx <- ceiling(mx * 10) / 10
  mn <- ifelse(log_trans == TRUE, 0.1, 0)

  sn <- ifelse(length(levels(dat_hist$season)) == 12, 'Month', 'Season')
  bp_fill <- paste(hist_rng[[1]], '-', hist_rng[[2]], ' Daily Average by ', sn, sep = '')

  x <- ggplot(data = dat_hist, aes(x = season, y = result, fill = factor(bp_fill))) +
    geom_boxplot(outlier.size = 0.5) +
    scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = scales::comma) +
    scale_fill_manual(name = '', values = c('skyblue1')) +
    labs(x = '') +
    # labs(x = '', ...) +
    theme_bw() +
    theme(legend.position = 'top'
          , legend.direction = 'horizontal')

  if(!is.null(target_yr)) {
    dat_yr <- dat %>% dplyr::filter(lubridate::year(datetimestamp) == target_yr)

    dat_yr <- dat_yr %>%
      dplyr::group_by(season, date) %>%
      # dplyr::summarise(result = mean(do_mgl, na.rm = T)) %>%
      dplyr::summarise(result = func(!!parm)) %>%
      dplyr::group_by(season) %>%
      dplyr::summarise(mean = mean(result, na.rm = T))

    pt_fill <- paste(target_yr, ' Daily Average by ', sn, sep = '')

    x <- x +
      geom_point(data = dat_yr, aes(x = season, y = mean, shape = factor(pt_fill)), fill = 'red', size = 2) +
      scale_shape_manual(name = '', values = c(21))
  }

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


# # Do I even need a default method?
# seasonal_boxplot <- function(swmpr_in) {
#
# }
