#' Seasonal boxplots
#'
#' Annual time series for year of interest on top of long-term percentiles
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
#' @param stat_lab chr, label for the summary statistic defined in \code{FUN}. Defaults to "Average"
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
#' @param FUN function used to aggregate daily SWMP data
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year floor_date
#' @importFrom rlang .data
#' @importFrom scales format_format pretty_breaks
#' @importFrom stats median
#' @importFrom tidyr complete
#'
#' @export
#'
#' @details This function uses boxplots to summarize statistics calculated on a daily basis across user-defined seasons for all years within the historic range (\code{hist_rng}). If \code{hist_rng} is not specified then the minimum and maximum years within the data set will be used. The summary statistics used to generate the boxplots are \code{ggplot2} defaults: the center of the box is a median, and the lower/upper limits of the box are the 25-th and 75-th percentiles. The whiskers extend to the furthest data point within 1.5 * inter-quartile range (IQR). The dots beyond the whiskers are data points that are greater than 1.5 * IQR. If the user selects a \code{target_yr}, then a median summary statistic value will be plotted as a point against the boxplots.
#'
#' Using the \code{FUN} argument, the user can specify the daily summary statistic to use. Commonly used statistics are \code{min(x, na.rm = TRUE)}, \code{mean(x, na.rm = TRUE)}, and \code{max(x, na.rm = TRUE)}. After specifying \code{FUN}, the user should also specify \code{stat_lab}, which is used to construct appropriate legend labels.
#'
#' The user also has the option to add a threshold hold line using the \code{criteria} argument. Typically, this value is a water quality threshold, which is why \code{criteria_lab} defaults to \code{'WQ Threshold'}. However, the user has the option to specify any other type of threshold they wish. when doing so, the value for \code{criteria_lab} should be changed accordingly.
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object or a \code{data.frame} if \code{plot = FALSE}
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}
#'
#' @examples
#' \dontshow{
#' data(apacpwq)
#'
#' dat <- qaqc(apacpwq, qaqc_keep = c('0', '3', '5'))
#'
#' y <- seasonal_boxplot(dat, param = 'do_mgl', target_yr = 2012)
#' }
#'
#' \donttest{
## get data, prep
#' dat <- elksmwq
#' dat <- qaqc(dat, qaqc_keep = c('0', '3', '5'))
#'
#' x <-
#'   seasonal_boxplot(dat, param = 'do_mgl')
#'
#' y <-
#'     seasonal_boxplot(dat, param = 'do_mgl', target_yr = 2015,
#'     season_grps = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12)),
#'     season_names = c('Winter', 'Spring', 'Summer', 'Fall'),
#'     season_start = 'Spring')
#'
#' z_min <-
#'    seasonal_boxplot(dat, param = 'do_mgl',
#'    stat_lab = 'Minimum', FUN = function(x) min(x, na.rm = TRUE))
#'
#' z_max <-
#'    seasonal_boxplot(dat, param = 'do_mgl',
#'    stat_lab = 'Maximum', FUN = function(x) max(x, na.rm = TRUE))
#' }

seasonal_boxplot <- function(swmpr_in, ...) UseMethod('seasonal_boxplot')

#' @rdname seasonal_boxplot
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
                                   , free_y = FALSE
                                   , log_trans = FALSE
								                   , converted = FALSE
								                   , criteria_lab = 'WQ Threshold'
                                   , stat_lab = 'Average'
                                   , plot_title = FALSE
                                   , plot = TRUE
                                   , FUN = function(x) mean(x, na.rm = TRUE)
                                   , ...) {

  dat <- swmpr_in
  parm <- sym(param)
  conv <- converted

  seas <- sym('season')
  res <- sym('result')
  dt <- sym('date')
  avg <- sym('mean')
  medi <- sym('med')

  rng <- hist_rng

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')
  data_type <- substr(station, 6, nchar(station))

  #TESTS
  #determine type WQ, MET, NUT
  #determine log scale transformation
  if(data_type == 'nut')
    warning('Nutrient data detected. Consider specifying seasons > 1 month.')

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
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine target year (if there is one)
  if(is.null(target_yr))
    warning('No target year provided')

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  # Assign the seasons and order them
  dat$season <- assign_season(dat$datetimestamp, abb = TRUE, ...)

  # Assign date for determining daily stat value
  dat$date <- lubridate::floor_date(dat$datetimestamp, unit = 'days')

  # Filter for parameter of interest
  dat <- dat[, c('date', 'season', param)]
  dat <- dat %>% dplyr::filter(!is.na(!! parm))

  # Filter for historic range
  dat_hist <- dat %>% dplyr::filter(lubridate::year(.data$date) >= rng[[1]]
                                    & lubridate::year(.data$date) <= rng[[2]])

  # Calc summary stat defined by FUN by season and day
  dat_hist <- dat_hist %>%
    group_by(!! seas, !! dt) %>%
    summarise(result = FUN(!! parm), .groups = "drop")

  # ensure all factor levels are accounted for, even if there is no data
  dat_hist <- tidyr::complete(dat_hist, !! seas)

  if(plot) {

    mx <- max(dat_hist$result, na.rm = TRUE)
    mx <- ifelse(data_type == 'nut' && param != 'chla_n', ceiling(mx/0.01) * 0.01, ceiling(mx))

    # assign a minimum of zero unless there are values < 0
    mn <- min(dat_hist$result, na.rm = TRUE)
    mn <- ifelse(mn < 0 , min(pretty(mn)), 0)
    mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), mn)

    lab_bp_fill <- ifelse(data_type == 'nut', paste('Monthly Sample \n(', rng[[1]], '-', rng[[2]], ')', sep = '')
                      , paste('Daily ', stat_lab, 's \n(', rng[[1]], '-', rng[[2]], ')', sep = ''))

    plt <- ggplot(data = dat_hist, aes_(x = seas, y = res, fill = lab_bp_fill)) +
      geom_boxplot(outlier.size = 0.5) +
      scale_fill_manual(name = '', values = c('#D9D9D9')) +
      labs(x = NULL, y = eval(y_label)) +
      theme_bw() +
      theme(legend.position = 'top'
            , legend.direction = 'horizontal')

    # add a log transformed access if log_trans == TRUE
    ## allow y-axis to be free if free_y == TRUE
    if(!log_trans) {
      plt <- plt +
        scale_y_continuous(labels = format_format(digits = 2, big.mark = ",", decimal.mark = ".", scientific = FALSE)
                           , breaks = pretty_breaks(n = 8))

      if(!free_y){plt <- plt + expand_limits(y = mn)}

    } else {
      plt <- plt +
        scale_y_continuous(trans = y_trans
                                , labels = format_format(digits = 2, big.mark = ",", decimal.mark = ".", scientific = FALSE)
                                , breaks = pretty_breaks(n = 8))

      if(!free_y) {plt <- plt + expand_limits(y = mn)}
    }

    # Add target year dots if specified
    if(!is.null(target_yr)) {
      dat_yr <- dat %>% dplyr::filter(lubridate::year(.data$date) == target_yr)

      dat_yr <- dat_yr %>%
        dplyr::group_by(!! seas, !! dt) %>%
        dplyr::summarise(result = FUN(!! parm), .groups = "drop") %>%
        dplyr::group_by(!! seas) %>%
        dplyr::summarise(med = stats::median(.data$result, na.rm = TRUE),
                         .groups = "drop")

      pt_fill <- ifelse(data_type == 'nut', paste('Monthly Sample \n(', target_yr, ')', sep = '')
                        , paste('Median Daily ', stat_lab, ' \n(', target_yr, ')', sep = ''))

      plt <- plt +
        geom_point(data = dat_yr, aes_(x = seas, y = medi, shape = factor(pt_fill)), fill = '#65BCFF', size = 2) +
        scale_shape_manual(name = '', values = c(21))
    }

    # Add criteria line if specified
    if(!is.null(criteria)) {

      plt <- plt +
        geom_hline(aes(yintercept = criteria, color = factor(criteria_lab), linetype = factor('WQ Threshold'))
                   , show.legend = TRUE) +
        scale_color_manual('', values = c('WQ Threshold' = 'red')) +
        scale_linetype_manual('', values = c('WQ Threshold' = 'longdash'))

      plt <- plt + guides(fill = guide_legend(order = 1)
                          , shape = guide_legend(order = 2, override.aes = list(linetype = 0))
                          , 'WQ Threshold' = guide_legend(order = 3))
    }

    # add plot title if specified
    if(plot_title) {
      ttl <- title_labeler(nerr_site_id = station)

      plt <-
        plt +
        ggtitle(ttl) +
        theme(plot.title = element_text(hjust = 0.5))
    }

    # Adjust theme
    plt <-
      plt +
      theme(strip.background = element_blank(),
            panel.border = element_rect(color = 'black')) +
      theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
      theme(text = element_text(size = 14)) # was 16

    # Adjust legend keys and spacing
    plt <-
      plt +
      theme(legend.key.height = unit(0.1, 'cm')
            , legend.key.width = unit(0.5, 'cm')) +
      theme(legend.text = element_text(size = 10)
            , legend.text.align = 0.5) +
      theme(legend.spacing.x = unit(3, 'pt'))

    return(plt)
  } else {

    dat_hist$range <- paste(paste(rng[[1]], '-', rng[[2]], sep = ''))
    dat_hist$summ_stat <- 'average'
    return(dat_hist)
  }
}
