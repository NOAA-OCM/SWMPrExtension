#' Historical Monthly/Seasonal Range Timeseries
#'
#' Compare seasonal averages/minimums/maximums for a target year to historical seasonal averages/minimums/maximums
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param target_yr numeric, the target year that should be compared against the historic range. If target year is not specified then dot will not be plotted
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
#' @param free_y logical, should the y-axis be free? Defaults to \code{FALSE}. If \code{FALSE}, defaults to zero, unless negative values are present. If \code{TRUE}, y-axis limits are selected by \code{ggplot}
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param criteria_lab chr, label for the threshold criteria defined in \code{criteria}. Defaults to "WQ Threshold"
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @import ggplot2
#'
#' @importFrom dplyr between filter group_by select summarise
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year floor_date
#' @importFrom rlang .data
#' @importFrom scales format_format pretty_breaks
#' @importFrom tidyr complete
#'
#' @export
#'
#' @details This function summarizes average daily values and average daily minimums/maximums across user-defined seasons for a target year (\code{target_yr}) and for a historical range (\code{hist_rng}). If \code{hist_rng} is not specified then the minimum and maximum years within the data set will be used. If \code{target_yr} is not specified then only the results for the \code{hist_rng} will be returned.
#'
#' The user also has the option to add a threshold hold line using the \code{criteria} argument. Typically, this value is a water quality threshold, which is why \code{criteria_lab} defaults to \code{'WQ Threshold'}. Howver, the user has the option to specify any other type of threshold they wish. when doing so, the value for \code{criteria_lab} should be changed accordingly.
#'
#' @author Julie Padilla, Kimberly Cressman
#'
#' @concept analyze
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}, \code{\link{y_labeler}}
#'
#' @examples
#' \dontshow{
#' data(apacpwq)
#'
#' dat <- qaqc(apacpwq, qaqc_keep = c('0', '3', '5'))
#'
#' y <- historical_range(dat, param = 'do_mgl')
#' }
#'
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
                                   , free_y = FALSE
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

  #determine historical range exists and that it is reasonable, if not default to min/max of the range
  x <- dat[ , c('datetimestamp', param)]
  x <- x[complete.cases(x), ]

  if(is.null(rng)) {
    warning('No historical range specified. Entire time series will be used.')
    rng <- c(min(lubridate::year(x$datetimestamp)), max(lubridate::year(x$datetimestamp)))
  } else {
    if(min(rng) < min(lubridate::year(x$datetimestamp)) | max(rng) > max(lubridate::year(x$datetimestamp))) {
      warning('Specified range is greater than the range of the dataset. Max/min  range of the dataset will be used.')
      rng <- c(min(lubridate::year(x$datetimestamp)), max(lubridate::year(x$datetimestamp)))
    }
  }

  # determine if target year is present within the data
  if(!is.null(target_yr)) {
    if(!(target_yr %in% unique(year(x$datetimestamp)))) {
      warning('User-specified target year is not present in the data set. target_yr argument will be set to max year in the data set')
      target_yr <- max(year(x$datetimestamp))
    }
  } else {
    warning('No target year specified. Maximum year in the data set will be used')
    target_yr <- max(year(x$datetimestamp))
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  # Filter to historic range and target year
  dat <- dat %>% dplyr::filter(lubridate::year(.data$datetimestamp) == target_yr |
                                 dplyr::between(lubridate::year(.data$datetimestamp),as.numeric(rng[[1]]), as.numeric(rng[[2]])))
  # dat <- dat %>% dplyr::filter(lubridate::year(.data$datetimestamp) >= rng[[1]]
  #                              & lubridate::year(.data$datetimestamp) <= rng[[2]])

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

  # separate into historical and target year data.frames
  dat_yr <- dat_all %>% dplyr::filter(lubridate::year(date) == target_yr)
  dat_all <- dat_all %>%
    dplyr::filter(dplyr::between(lubridate::year(.data$date), as.numeric(rng[[1]]), as.numeric(rng[[2]])))

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

  # ensure all factor levels are accounted for, even if there is no data
  dat_yr <- tidyr::complete(dat_yr, !! seas)
  dat_hist <- tidyr::complete(dat_hist, !! seas)

  if(plot){
    # Set the plot range
    mx <- ifelse(max(dat_yr[ , c(2:4)], na.rm = T) > max(dat_hist[ , c(2:4)], na.rm = T)
                 , max(dat_yr[ , c(2:4)], na.rm = T), max(dat_hist[ , c(2:4)], na.rm = T))
    mx <- max(pretty(mx))

    # assign a minimum of zero unles there are values < 0
    mn <- ifelse(min(dat_yr[ , c(2:4)], na.rm = T) < min(dat_hist[ , c(2:4)], na.rm = T)
                 , min(dat_yr[ , c(2:4)], na.rm = T), min(dat_hist[ , c(2:4)], na.rm = T))
    mn <- ifelse(mn < 0 , min(pretty(mn)), 0)
    mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), mn)

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

    # add a log transformed access if log_trans == T
    ## allow y-axis to be free if free_y == T
    if(!log_trans) {
      plt <- plt +
        scale_y_continuous(labels = format_format(digits = 2, big.mark = " ", decimal.mark = ".", scientific = FALSE)
                           , breaks = pretty_breaks(n = 8))

      if(!free_y){plt <- plt + expand_limits(y = mn)}

    } else {
      plt <- scale_y_continuous(trans = y_trans
                                , labels = format_format(digits = 2, big.mark = " ", decimal.mark = ".", scientific = FALSE)
                                , breaks = pretty_breaks(n = 8))

      if(!free_y) {plt <- plt + expand_limits(y = mn)}
    }

    # conditionally assign legend/ribbon colors (lab_yr_rng is less than lab_hist_rng when plotting a target year that is outside the historical range)
    if(lab_yr_rng > lab_hist_rng) {
      ribbon_fill <- c('steelblue3', 'gray40', 'steelblue3')
      alpha_fill <- c('steelblue3', 'gray40')
    } else {
      ribbon_fill <- c('steelblue3', 'steelblue3', 'gray40')
      alpha_fill <- c('gray40', 'steelblue3')
    }

    # Adjust scale
    plt <-
      plt +
      scale_color_manual('', values = c('gray40')) +
      scale_fill_manual('', values = ribbon_fill, guide = F) +
      # scale_fill_manual('', values = c('steelblue3', 'gray40', 'steelblue3'), guide = F) +
      scale_shape_manual('', values = c(21)) +
      scale_alpha_manual('', values = rep(0.25, 2))

    # Override legend defaults
    plt <-
      plt +
      guides(alpha = guide_legend(override.aes = list(fill = alpha_fill), order = 3, reverse = T)
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
    sz <- ifelse(!is.null(criteria), 8, 10)
    plt <-
      plt +
      theme(legend.key.height = unit(0.1, 'cm')
            , legend.key.width = unit(0.5, 'cm')) +
      theme(legend.text = element_text(size = sz)
            , legend.text.align = 0.5) +
      theme(legend.spacing.x = unit(3, 'pt'))

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
