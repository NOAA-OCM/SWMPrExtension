#' Historical Range Timeseries
#'
#' Monthly average and variability. Looking at variability within each month; no historical context
#'
#' @param swmpr_in input swmpe object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param target_yr numeric, the target year that should be compared against the historic range. If target year is not specified then dot will not be plotted
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
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
#' @details Comparison of analysis year statistical summaries to the long term monthly averages and variability.
#' To put it another way, this analysis is looking at monthly averages in context of comparison to period variability.
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
#' data(apacpwq)
#' dat <- apacpwq
#'
#' dat <- qaqc(apacpwq, qaqc_keep = c('0', '3', '5'))
#' # with criteria
#' y <- historical_range(dat, param = 'do_mgl', target_yr = 2013, criteria = 2, abb = T)
#'
#' # w/o criteria
#' x <- historical_range(dat, param = 'do_mgl', target_yr = 2013, abb = T)
#'
#' # add a y label
#' x <- x + labs(x = NULL, y = "Dissolved Oxygen (mg/L)")
#' }

historical_range <- function(swmpr_in, ...) UseMethod('historical_range')

#' @rdname historical_range
#'
#' @concept analyze
#'
#' @export
#'
#' @method historical_range swmpr
#'
historical_range.swmpr <- function(swmpr_in
                                   , param = NULL
                                   , hist_rng = NULL
                                   , target_yr = NULL
                                   , criteria = NULL
                                   , log_trans = FALSE
                                   , plot = TRUE
                                   , ...) {

  dat <- swmpr_in
  parm <- sym(param)

  seas <- sym('season')
  dt <- sym('date')
  avg <- sym('mean')
  mini <- sym('min')
  maxi <- sym('max')

  rng <- hist_rng

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  #determine type WQ, MET, NUT
  if(substr(station, 6, nchar(station)) == 'nut')
    warning('Nutrient data detected. Consider specifying seasons > 1 month. See `?assign_season` for details.')

  #determine historical range exists, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No historical range specified. Entire time series will be used.')
    rng <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine target year (if there is one)
  if(is.null(target_yr))
    warning('No target year provided. Only historic range will be plotted.')

  #determine y axis transformation
  y_trans <- ifelse(log_trans, 'log10', 'identity')

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  # Filter to historic range
  dat <- dat %>% dplyr::filter(lubridate::year(.data$datetimestamp) >= rng[[1]]
                                    & lubridate::year(.data$datetimestamp) <= rng[[2]])

  # Assign date for determining daily stat value
  dat$date <- lubridate::floor_date(dat$datetimestamp, unit = 'days')

  # Filter for parameter of interest and remove NA values
  dat <- dat %>% dplyr::select(.data$datetimestamp, date, !!parm)
  dat <- dat %>% dplyr::filter(!is.na(!! parm))

  # Determine min/max/mean for each day
  dat_all <- dat %>%
    dplyr::group_by(!! dt) %>%
    dplyr::summarise(mean = mean(!! parm, na.rm = TRUE)
                     , min = min(!! parm, na.rm = TRUE)
                     , max = max(!! parm, na.rm = TRUE))

  # Assign seasons
  dat_all$season <- assign_season(dat_all$date, ...)

  # Determine average min/max/mean for each month (for all years together)
  dat_hist <- dat_all %>%
    dplyr::group_by(!! seas) %>%
    dplyr::summarise(mean = mean(!! avg, na.rm = T)
                     , min = mean(!!  mini, na.rm = T)
                     , max = mean(!! maxi, na.rm = T))

  dat_yr <- dat_all %>% dplyr::filter(year(date) == target_yr)

  dat_yr <- dat_yr %>%
    dplyr::group_by(!! seas) %>%
    dplyr::summarise(mean = mean(!! avg, na.rm = T)
                     , min = mean(!!  mini, na.rm = T)
                     , max = mean(!! maxi, na.rm = T))

  if(plot){
    # Set the plot range
    mx <- max(dat_hist$max, na.rm = T)
    mx <- ceiling(mx)
    mn <- ifelse(log_trans == TRUE, 0.1, 0)

    # Make some labels
    lab_hist_rng <- paste(rng[[1]], '-', rng[[2]], ' Daily Avg Range', sep = '')
    lab_hist_ln <- paste(rng[[1]], '-', rng[[2]], ' Daily Avg', sep = '')
    lab_yr_rng <- paste(target_yr, ' Daily Avg Range', sep = '')
    lab_yr_ln <- paste(target_yr, ' Daily Avg', sep = '')

    # Make plot
    plt <-
      ggplot(data = dat_yr, aes_(x = seas, y = avg, group = 1)) +
      geom_ribbon(data = dat_hist, aes_(x = seas, ymin = mini, ymax = maxi
                                        , fill = lab_hist_rng, alpha = lab_hist_rng)) +
      geom_ribbon(data = dat_yr, aes_(x = seas, ymin = mini, ymax = maxi
                                      , fill = lab_yr_rng, alpha = lab_yr_rng)) +
      geom_line(data = dat_hist, aes_(x = seas, y = avg, color = lab_hist_ln)
                , linetype = 'solid', lwd = 1) +
      geom_line(color = 'steelblue3', lwd = 1) +
      geom_point(aes_(fill = lab_yr_ln, shape = lab_yr_ln), size = 2) +
      labs(x = NULL, y = NULL) +
      theme_bw() +
      theme(legend.position = 'top', legend.direction = 'horizontal')

    # Adjust scale
    plt <-
      plt +
      scale_color_manual('', values = c('gray40')) +
      scale_fill_manual('', values = c('gray40', 'steelblue3', 'steelblue3'), guide = F) +
      scale_shape_manual('', values = c(21)) +
      scale_alpha_manual('', values = rep(0.25, 2))

    # Override legend defaults
    plt <-
      plt +
      guides(alpha = guide_legend(override.aes = list(fill = c('steelblue3', 'gray40')), order = 3, reverse = T)
             , shape = guide_legend(override.aes = list(fill = 'steelblue3'), order = 1)
             , color = guide_legend(override.aes = list(color = 'gray40'), order = 2))

      # Adjust theme
      plt <-
        plt +
        theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.border = element_rect(color = 'black')) +
        theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
        theme(text = element_text(size = 16))

    # Adjust legend keys and spacing
    plt <-
      plt +
      theme(legend.key.size = unit(7, 'pt')) +
      theme(legend.text = element_text(size = 8)) +
      theme(legend.spacing.x = unit(-6, 'pt'))

    # Add criteria line if specified
    if(!is.null(criteria)) {

      plt <- plt +
        geom_hline(aes(yintercept = criteria, linetype = factor('WQ Threshold'))
                       , color = 'red', show.legend = T) +
        scale_linetype_manual('', values = c('longdash'))

      plt <-
        plt +
        guides(alpha = guide_legend(override.aes = list(fill = c('steelblue3', 'gray40'), linetype = 0), order = 3, reverse = T)
               , shape = guide_legend(override.aes = list(fill = 'steelblue3', linetype = 0), order = 1)
               , color = guide_legend(override.aes = list(color = 'gray40'), order = 2)
               , linetype = guide_legend(override.aes = list(color = 'red'), order = 4))
    }

    return(plt)

  } else {

    #Add information about the station and the year ranges
    dat_hist$rng <- paste(rng[[1]], '-', rng[[2]])
    dat_yr$rng <- as.character(target_yr)

    tbl <- rbind(dat_hist, dat_yr)

    tbl$station <- attr(dat, 'station')

    return(tbl)
  }

}
