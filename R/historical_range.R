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
#' do_plt <- historical_range(dat, param = 'do_mgl', target_yr = 2012)
#' do_plt <- historical_range(dat, param = 'do_mgl', target_yr = 2012, criteria = 2)
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
  # res <- sym('result')
  dt <- sym('date')
  avg <- sym('mean')
  mini <- sym('min')
  maxi <- sym('max')
  # mini_avg <- sym('min_avg')
  # maxi_avg <- sym('max_avg')

  rng <- hist_rng

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
  dat_hist <- dat %>% dplyr::filter(lubridate::year(.data$datetimestamp) >= rng[[1]]
                                    & lubridate::year(.data$datetimestamp) <= rng[[2]])

  # Assign the seasons and order them
  # dat$season <- assign_season(dat$datetimestamp, abb = T, ...)

  # Assign date for determining daily stat value
  dat$date <- lubridate::floor_date(dat$datetimestamp, unit = 'days')

  # Filter for parameter of interest and remove NA values
  dat <- dat %>% dplyr::select(.data$datetimestamp, date, !!parm)
  dat <- dat %>% dplyr::filter(!is.na(!! parm))

  #Determine min/max/mean for each day
  dat_all <- dat %>%
    dplyr::group_by(!! dt) %>%
    dplyr::summarise(mean = mean(!! parm, na.rm = TRUE)
                     , min = min(!! parm, na.rm = TRUE)
                     , max = max(!! parm, na.rm = TRUE))

  dat_all$season <- assign_season(dat_all$date, abb = T, ...)

  #Determine average min/max/mean for each month (for all years together)
  dat_hist <- dat_all %>%
    dplyr::group_by(!! seas) %>%
    dplyr::summarise(mean = mean(!! avg, na.rm = T)
                     , min = mean(!!  mini, na.rm = T)
                     , max = mean(!! maxi, na.rm = T))

  dat_yr <- dat_all %>% dplyr::filter(year(date) == target_yr)

  dat_yr <- dat_yr %>%
    dplyr::summarise(mean = mean(!! avg, na.rm = T)
                     , min = mean(!!  mini, na.rm = T)
                     , max = mean(!! maxi, na.rm = T))

  if(plot){
    # Set the plot range
    mx <- max(dat_hist$max, na.rm = T)
    mx <- ceiling(mx)
    mn <- ifelse(log_trans == TRUE, 0.1, 0)

    # Make some labels
    lab_hist_rng <- paste(rng[[1]], '-', rng[[2]], ' Daily Average Range by Month', sep = '') %>% sym
    lab_hist_ln <- paste(rng[[1]], '-', rng[[2]], ' Daily Average by Month', sep = '') %>% sym
    lab_yr_rng <- paste(target_yr, ' Daily Average Range by Month', sep = '') %>% sym
    lab_yr_ln <- paste(target_yr, ' Daily Average by Month', sep = '') %>% sym

    plt_yr <-
      ggplot() +
      geom_ribbon(data = dat_yr
                  , aes_(x = seas, ymin = mini, ymax = maxi, fill = lab_yr_rng, group = lab_yr_rng)) +
      geom_line(data = dat_hist
                , aes_(x = seas, y = maxi, group = lab_hist_rng, linetype = lab_hist_rng, color = lab_hist_rng)
                , show.legend = F) +
      geom_line(data = dat_hist
                , aes_(x = seas, y = mini, group = lab_hist_rng, linetype = lab_hist_rng, color = lab_hist_rng)
      ) +
      geom_line(data = dat_hist
                , aes_(x = seas, y = mean, color = lab_hist_ln, group = lab_hist_ln)
                , linetype = 'solid', lwd = 1) +
      geom_line(data = dat_yr
                , aes_(x = seas, y = avg,  group = lab_yr_ln, linetype = lab_yr_ln)
                , color = 'steelblue3', lwd = 1) +
      geom_point(data = dat_yr
                 , aes_(x = seas, y = avg, group = lab_yr_ln)
                 , fill = 'steelblue3', shape = 21, size = 2, show.legend = T) +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(limits = c(mn, mx), trans = y_trans)

    # Format the scales
    plt_yr <-
      plt_yr +
      scale_fill_manual(values = c('lightskyblue1')) +
      # scale_shape_manual(values = c(21)) +
      scale_color_manual(name = '', values = c('gray40', 'gray40', 'gray40', 'steelblue3')) +
      scale_shape_manual(name = '', values = c(NA, NA, NA, 21)) +
      scale_linetype_manual(name = '', values = c('dashed', 'solid', 'dashed', 'solid'))

    plt_yr <-
      plt_yr +
      guides(fill = guide_legend(override.aes = list(shape = NA))
             , shape = guide_legend(override.aes = list(shape = NA))
             , linetype = guide_legend(override.aes = list(shape = c(NA, 21), linetype = c('solid', 'dashed')))
             , colour = guide_legend(override.aes = list(shape = c(NA, 21))))

      # Adjust theme
      plt_yr <-
      plt_yr +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background = element_blank(),
            panel.border = element_rect(color = 'black')) +
      theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
      theme(text = element_text(size = 16))

    # Adjust legend
    plt_yr <-
      plt_yr +
      # guides(color = guide_legend(order = 2),
      #        fill = guide_legend(order = 1, ncol = 2, nrow = 1)) +
      theme(legend.position = 'top'
            , legend.box = c('vertical')
            , legend.title = element_blank()) +
      # theme(legend.key.size = unit(7, 'pt')) +
      theme(legend.text = element_text(size = 8)) +
      theme(legend.spacing.x = unit(-5, 'pt'))

    # plt <-
    #   ggplot(data = dat_month, aes_(x = seas, y = avg, group = 1)) +
    #   geom_ribbon(aes_(x = seas, ymax = maxi_avg, ymin = mini_avg, fill = rng_avg, alpha = rng_avg)) +
    #   geom_ribbon(aes_(x = seas, ymax = maxi, ymin = mini, group = 1, fill = rng_mx, alpha = rng_mx)) +
    #   geom_line(lwd = 1, color = 'steelblue3') +
    #   geom_point(aes_(fill = ln, shape = ln), color = 'black', size = 2) +
    #   scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = comma) +
    #   labs(x = '', y = '') +
    #   theme_bw() +
    #   theme(legend.position = 'top', legend.direction = 'horizontal')
    #
    # plt <-
    #   plt +
    #   scale_fill_manual('', values = c(rep('steelblue3', 3)), guide = F) +
    #   scale_shape_manual('', values = c(21)) +
    #   scale_alpha_manual('', values = c(0.4, 0.15))
    #
    # plt <-
    #   plt +
    #   guides(alpha = guide_legend(override.aes = list(fill = 'steelblue3'))
    #          , shape = guide_legend(override.aes = list(fill = 'steelblue3')))

    # Add criteria line if specified
    if(!is.null(criteria)) {

      plt <- plt +
        geom_hline(aes(yintercept = criteria, color = factor('WQ Threshold')
                       , linetype = factor('WQ Threshold'))
                   , show.legend = T) +
        scale_color_manual('', values = c('WQ Threshold' = 'red')) +
        scale_linetype_manual('', values = c('WQ Threshold' = 'longdash'))

      plt <-
        plt +
        guides(alpha = guide_legend(override.aes = list(fill = 'steelblue3', linetype = 0), order = 2)
               , shape = guide_legend(override.aes = list(fill = 'steelblue3', linetype = 0), order = 1)
               , 'WQ Threshold' = guide_legend(order = 3))
    }

    return(plt)

  } else {

    tbl <- rbind(dat_hist, dat_yr)
    tbl$station <- attr(dat, 'station')

    return(tbl)
  }

}
