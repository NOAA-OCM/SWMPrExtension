#' Historical Daily Range Timeseries
#'
#' Compare daily averages for a target year to historical highs and lows
#'
#' @param swmpr_in input swmp object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param target_yr numeric, the target year that should be compared against the historic range. If target year is not specified then dot will not be plotted
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param criteria_lab chr, label for the threshold criteria defined in \code{criteria}. Defaults to "WQ Threshold"
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
#' @param ... not used
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter group_by select summarise
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year floor_date yday
#' @importFrom rlang .data
#' @importFrom scales comma
#' @importFrom tidyr complete
#'
#' @export
#'
#' @details This function compares the average daily minimums/maximums and absolute minimums/maximums from a historical range to the average daily value from a target year. If \code{hist_rng} is not specified then the minimum and maximum years within the data set will be used. If \code{target_yr} is not specified then only the results for the \code{hist_rng} will be returned.
#'
#' The user also has the option to add a threshold line using the \code{criteria} argument. Typically, this value is a water quality threshold, which is why \code{criteria_lab} defaults to \code{'WQ Threshold'}. Howver, the user has the option to specify any other type of threshold they wish. when doing so, the value for \code{criteria_lab} should be changed accordingly.
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{y_labeler}}
#'
#' @examples
#' \dontrun{
## get data, prep
#' data(apacpwq)
#' dat <- apacpwq
#'
#' dat <- qaqc(apacpwq, qaqc_keep = c('0', '3', '5'))
#' # with criteria
#' y <- historical_daily_range(dat, param = 'do_mgl', target_yr = 2013, criteria = 2)
#'
#' # w/o criteria
#' x <- historical_daily_range(dat, param = 'do_mgl', target_yr = 2013)
#'
#' # add a y label
#' x <- x + labs(x = NULL, y = "Dissolved Oxygen (mg/L)")
#' }

historical_daily_range <- function(swmpr_in, ...) UseMethod('historical_daily_range')

#' @rdname historical_daily_range
#'
#' @concept analyze
#'
#' @export
#'
#' @method historical_daily_range swmpr
#'
historical_daily_range.swmpr <- function(swmpr_in
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
  jd <- sym('julian_day')

  rng <- hist_rng

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  #determine historical range exists and that it is reasonable, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No historical range specified. Entire time series will be used.')
    rng <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
  } else {
    if(min(rng) < min(lubridate::year(dat$datetimestamp)) | max(rng) > max(lubridate::year(dat$datetimestamp))) {
      warning('Specified range is greater than the range of the dataset. Max/min  range of the dataset will be used.')
      rng <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
    }
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine target year (if there is one)
  if(is.null(target_yr))
    warning('No target year provided. Only historic range will be plotted.')

  # determine if target year is present within the data. If not reset it
  if(!is.null(target_yr)) {
    if(!(target_yr %in% unique(year(dat$datetimestamp)))) {
      warning('User-specified target year is not present in the data set. target_yr argument will be set to max year in the data set')
      target_yr <- max(year(dat$datetimestamp))
    }
  }

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

  dat_all$julian_day <- lubridate::yday(dat_all$date)

  # Determine average min/max/mean for each julian day (for all years together)
  dat_hist_avg <- dat_all %>%
    dplyr::group_by(!! jd) %>%
    dplyr::summarise(mean = mean(!! avg, na.rm = T)
                     , min = mean(!!  mini, na.rm = T)
                     , max = mean(!! maxi, na.rm = T))

  dat_hist_obs <- dat_all %>%
    dplyr::group_by(!! jd) %>%
    dplyr::summarise(mean = mean(!! avg, na.rm = T)
                     , min = min(!!  mini, na.rm = T)
                     , max = max(!! maxi, na.rm = T))

  dat_yr <- dat_all %>% dplyr::filter(lubridate::year(date) == target_yr)

  # account for missing julian days
  if(length(dat_yr[1, ] < 365)){
    jday_fill <- data.frame(julian_day = c(1:365))
    dat_yr <- suppressMessages(dplyr::left_join(jday_fill, dat_yr))
  }

  if(plot){
    # Set the plot range
    mx <- max(dat_hist_obs$max, na.rm = T)
    mx <- ceiling(mx)
    mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), 0)
    brks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) #jdays associated with the first of every month
    brk_labs <- month.abb

    # Make some labels
    lab_hist_avg_rng <- paste('Daily Avg Range \n(', rng[[1]], '-', rng[[2]], ')', sep = '')
    lab_hist_obs_rng <- paste('Daily Avg Range \n(', rng[[1]], '-', rng[[2]], ')', sep = '')
    lab_yr_ln <- paste('Daily Avg \n(', target_yr, ')', sep = '')

    # Make plot
    plt <-
      ggplot(data = dat_yr, aes_(x = jd, y = avg, group = 1)) +
      geom_ribbon(data = dat_hist_obs, aes_(x = jd, ymin = mini, ymax = maxi, fill = lab_hist_obs_rng)) +
      geom_ribbon(data = dat_hist_avg, aes_(x = jd, ymin = mini, ymax = maxi, fill = lab_hist_avg_rng)) +
      geom_line(aes(color = lab_yr_ln), lwd = 1.5) +
      scale_x_continuous(breaks = brks, labels = brk_labs) +
      labs(x = NULL, y = eval(y_label)) +
      theme_bw() +
      theme(legend.position = 'top', legend.direction = 'horizontal')

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
      scale_color_manual('', values = c('steelblue3')) +
      scale_fill_manual('', values = c('gray80', 'gray60'))

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
      theme(legend.key.height = unit(0.1, 'cm')
            , legend.key.width = unit(0.5, 'cm')) +
      theme(legend.text = element_text(size = 10)
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
        guides(fill = guide_legend(override.aes = list(linetype = 0), order = 2, reverse = T)
               , color = guide_legend(override.aes = list(color = 'steelblue3'), order = 1)
               , linetype = guide_legend(override.aes = list(color = 'red'), order = 3))
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
