#' Historical Monthly/Seasonal Range Timeseries
#'
#' Compare seasonal averages/mins/maxes for a target year to historical seasonal averages/mins/maxes
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param target_yr numeric, the target year that should be compared against the historic range. If target year is not specified then dot will not be plotted
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param criteria_lab chr, label for the threshold criteria defined in \code{criteria}. Defaults to "WQ Threshold"
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}} and \code{\link{y_labeler}}.
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter group_by select summarise
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year floor_date
#' @importFrom rlang .data
#' @importFrom scales comma
#'
#' @export
#'
#' @details This function summarizes average daily values and average daily minimums/maximums across user-defined seasons for a target year (\code{target_yr}) and for a historical range (\code{hist_rng}). If \code{hist_rng} is not specified then the minimum and maximum years within the data set will be used. If \code{target_yr} is not specified then only the results for the \code{hist_rng} will be returned.
#'
#' The user also has the option to add a threshold hold line using the \code{criteria} argument. Typically, this value is a water quality threshold, which is why \code{criteria_lab} defaults to \code{'WQ Threshold'}. Howver, the user has the option to specify any other type of threshold they wish. when doing so, the value for \code{criteria_lab} should be changed accordingly.
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}, \code{\link{y_labeler}}
#'
#' @examples
#' \dontrun{
## get data, prep
#' data(elksmwq)
#'
#' dat <- qaqc(elksmwq, qaqc_keep = c('0', '3', '5'))
#' # with criteria
#' y <- historical_range(dat, param = 'do_mgl', target_yr = 2013, criteria = 2)
#'
#' # w/o criteria
#' x <- historical_range(dat, param = 'do_mgl', target_yr = 2013)
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
                                   , converted = FALSE
                                   , criteria_lab = 'WQ Threshold'
                                   , plot_title = FALSE
                                   , plot = TRUE
                                   , ...) {

  dat <- swmpr_in
  parm <- sym(param)
  conv <- converted

  seas <- sym('season')
  dt <- sym('date')
  avg <- sym('mean')
  mini <- sym('min')
  maxi <- sym('max')

  rng <- hist_rng

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')
  data_type <- substr(station, 6, nchar(station))

  #TESTS
  #determine type WQ, MET, NUT
  if(data_type == 'nut')
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
    warning('No target year provided. Only historic range will be returned.')

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

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
  if(data_type != 'nut') {
    dat_hist <- dat_all %>%
      dplyr::group_by(!! seas) %>%
      dplyr::summarise(mean = mean(!! avg, na.rm = T)
                       , min = mean(!!  mini, na.rm = T)
                       , max = mean(!! maxi, na.rm = T))

    # Make some labels
    lab_hist_rng <- paste('Daily Avg Range \n(', rng[[1]], '-', rng[[2]], ')', sep = '')
    lab_hist_ln <- paste('Daily Avg \n(', rng[[1]], '-', rng[[2]], ')', sep = '')
    lab_yr_rng <- paste('Daily Avg Range \n(', target_yr, ')', sep = '')
    lab_yr_ln <- paste('Daily Avg \n(', target_yr, ')', sep = '')

  } else {
    dat_hist <- dat_all %>%
      dplyr::group_by(!! seas) %>%
      dplyr::summarise(mean = mean(!! avg, na.rm = T)
                       , min = min(!!  mini, na.rm = T)
                       , max = max(!! maxi, na.rm = T))

    # Make some labels
    lab_hist_rng <- paste('Seasonal Range \n(', rng[[1]], '-', rng[[2]], ')', sep = '')
    lab_hist_ln <- paste('Seasonal Avg \n(', rng[[1]], '-', rng[[2]], ')', sep = '')
    lab_yr_rng <- paste('Seasonal Range \n(', target_yr, ')', sep = '')
    lab_yr_ln <- paste('Seasonal Avg \n(', target_yr, ')', sep = '')

  }

  dat_yr <- dat_all %>% dplyr::filter(year(date) == target_yr)

  if(data_type != 'nut') {
    dat_yr <- dat_yr %>%
      dplyr::group_by(!! seas) %>%
      dplyr::summarise(mean = mean(!! avg, na.rm = T)
                       , min = mean(!!  mini, na.rm = T)
                       , max = mean(!! maxi, na.rm = T))
  } else {
    dat_yr <- dat_yr %>%
      dplyr::group_by(!! seas) %>%
      dplyr::summarise(mean = mean(!! avg, na.rm = T)
                       , min = min(!!  mini, na.rm = T)
                       , max = max(!! maxi, na.rm = T))
  }



  if(plot){
    # Set the plot range
    mx <- ifelse(max(dat_yr[ , c(2:4)], na.rm = T) > max(dat_hist[ , c(2:4)], na.rm = T)
                 , max(dat_yr[ , c(2:4)], na.rm = T), max(dat_hist[ , c(2:4)], na.rm = T))
    mx <- ceiling(mx)
    mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), 0)



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
      labs(x = NULL, y = eval(y_label)) +
      theme_bw() +
      theme(legend.position = 'top')

    # add a log transformed access if log_trans = T
    if(!log_trans) {

      plt <- plt + scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = scales::comma)

    } else {

      mx_log <- 10^(ceiling(log10(mx)))

      mag_lo <- nchar(mn) - 2
      mag_hi <- nchar(mx_log) - 1

      brks <- 10^(-mag_lo:mag_hi)

      plt <- plt + scale_y_continuous(limits = c(mn, mx_log), breaks = brks, trans = y_trans, labels = scales::comma)
    }

    # Adjust scale
    plt <-
      plt +
      scale_color_manual('', values = c('gray40')) +
      scale_fill_manual('', values = c('steelblue3', 'gray40', 'steelblue3'), guide = F) +
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
      theme(strip.background = element_blank(),
            panel.border = element_rect(color = 'black')) +
      theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
      theme(text = element_text(size = 16))

    # Adjust legend keys and spacing
    sz <- ifelse(!is.null(criteria), 9, 10)
    plt <-
      plt +
      theme(legend.key.height = unit(0.1, 'cm')
            , legend.key.width = unit(0.5, 'cm')) +
      theme(legend.text = element_text(size = sz)
            , legend.text.align = 0.5) +
      theme(legend.spacing.x = unit(-6, 'pt'))

    # Add criteria line if specified
    if(!is.null(criteria)) {

      plt <- plt +
        geom_hline(aes(yintercept = criteria, linetype = factor(criteria_lab))
                   , color = 'red', show.legend = T) +
        scale_linetype_manual('', values = c('longdash'))

      plt <-
        plt +
        guides(alpha = guide_legend(override.aes = list(fill = c('steelblue3', 'gray40'), linetype = 0), order = 3, reverse = T)
               , shape = guide_legend(override.aes = list(fill = 'steelblue3', linetype = 0), order = 1)
               , color = guide_legend(override.aes = list(color = 'gray40'), order = 2)
               , linetype = guide_legend(override.aes = list(color = 'red'), order = 4))
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

  } else {

    #Add information about the station and the year ranges
    dat_hist$rng <- paste(rng[[1]], '-', rng[[2]])
    dat_yr$rng <- as.character(target_yr)

    tbl <- rbind(dat_hist, dat_yr)

    tbl$station <- attr(dat, 'station')

    return(tbl)
  }

}
