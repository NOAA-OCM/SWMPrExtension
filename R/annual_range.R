#' Annual Range Timeseries
#'
#' Assess variability within each season for a single year
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param target_yr numeric, the target year that should be compared against the historic range. If target year is not specified then the dot will not be plotted.
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
#' @importFrom dplyr filter group_by select summarise
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year floor_date
#' @importFrom rlang .data
#' @importFrom scales format_format pretty_breaks
#' @importFrom tidyr complete
#'
#' @export
#'
#' @details This function summarizes average daily values, average daily minimums/maximums, and absolute minimums/maximums across user-defined seasons for a target year (\code{target_yr}).
#'
#' The user also has the option to add a threshold hold line using the \code{criteria} argument. Typically, this value is a water quality threshold, which is why \code{criteria_lab} defaults to \code{'WQ Threshold'}. However, the user has the option to specify any other type of threshold they wish. when doing so, the value for \code{criteria_lab} should be changed accordingly.
#'
#' @author Julie Padilla
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
#' y <- annual_range(dat, param = 'do_mgl', target_yr = 2012)
#' }
#'
#' \dontrun{
#' ## get data, prep
#' data(elksmwq)
#' dat <- elksmwq
#'
#' dat <- qaqc(elksmwq, qaqc_keep = c('0', '3', '5'))
#' do_plt <- annual_range(dat, param = 'do_mgl', target_yr = 2012)
#' do_plt <- annual_range(dat, param = 'do_mgl', target_yr = 2012, criteria = 2)
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
  res <- sym('result')
  dt <- sym('date')
  avg <- sym('mean')
  mini <- sym('min')
  maxi <- sym('max')
  mini_avg <- sym('min_avg')
  maxi_avg <- sym('max_avg')

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')
  data_type <- substr(station, 6, nchar(station))

  #TESTS
  #determine type WQ, MET, NUT
  #determine log scale transformation
  if(data_type == 'nut')
    warning('Nutrient data detected. Consider specifying seasons > 1 month. See `?assign_season` for details.')



  #determine target year exists, if not default to min/max of the range
  x <- dat[ , c('datetimestamp', param)]
  x <- x[complete.cases(x), ]

  if(is.null(target_yr)) {
    warning('No target year specified. Maximum year in data set will be used.')
    target_yr <- max(lubridate::year(x$datetimestamp))
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

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  # Filter data to target year

  # Assign the seasons and order them
  dat <- dat %>% filter(lubridate::year(.data$datetimestamp) == target_yr)
  dat$season <- assign_season(dat$datetimestamp, abb = T, ...)

    # Assign date for determining daily stat value
  dat$date <- lubridate::floor_date(dat$datetimestamp, unit = 'days')

  # # Filter for parameter of interest and remove NA values
  dat <- dat %>% dplyr::select(.data$datetimestamp, date, .data$season, !!parm)
  dat <- dat %>% dplyr::filter(!is.na(!! parm))

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

  # ensure all factor levels are accounted for, even if there is no data
  dat_month <- tidyr::complete(dat_month, !! seas)

  if(plot){
    # Set the plot range
    # mx <- max(dat_day$max, na.rm = T)
    # mx <- max(pretty(mx))

    # assign a minimum of zero unles there are values < 0
    mn <- min(dat_month$min, na.rm = T)
    mn <- ifelse(mn < 0 , min(pretty(mn)), 0)
    mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), mn)

    lab_ln <- ifelse(data_type == 'nut', paste('Monthly Sample \n(', target_yr, ')', sep = ''), paste('Daily Average \n(', target_yr, ')', sep = ''))

    plt <-
      ggplot(data = dat_month, aes_(x = seas, y = avg, group = 1)) +
      geom_line(lwd = 1, color = 'steelblue3') +
      geom_point(aes_(fill = lab_ln, shape = lab_ln), color = 'black', size = 2) +
      labs(x = NULL, y = eval(y_label)) +
      theme_bw() +
      theme(legend.position = 'top', legend.direction = 'horizontal')

    if(data_type != 'nut') {
      lab_rng_avg <- paste('Avg Daily Range \n(', target_yr, ')', sep = '')
      lab_rng_mx <- paste('Daily Range \n(', target_yr, ')', sep = '')

      plt <-
        plt +
        geom_ribbon(aes_(x = seas, ymax = maxi_avg, ymin = mini_avg, fill = lab_rng_avg, alpha = lab_rng_avg)) +
        geom_ribbon(aes_(x = seas, ymax = maxi, ymin = mini, group = 1, fill = lab_rng_mx, alpha = lab_rng_mx))
    }

    # add a log transformed access if log_trans == T
    ## allow y-axis to be free if free_y == T
    if(!log_trans) {
      plt <- plt +
        scale_y_continuous(labels = format_format(digits = 2, big.mark = " ", decimal.mark = ".", scientific = FALSE)
                           , breaks = pretty_breaks(n = 8))

      if(!free_y){plt <- plt + expand_limits(y = mn)}

    } else {
      plt <- plt +
        scale_y_continuous(trans = y_trans
                                , labels = format_format(digits = 2, big.mark = " ", decimal.mark = ".", scientific = FALSE)
                                , breaks = pretty_breaks(n = 8))

      if(!free_y) {plt <- plt + expand_limits(y = mn)}
    }

    plt <-
      plt +
      scale_fill_manual('', values = c(rep('steelblue3', 3)), guide = F) +
      scale_shape_manual('', values = c(21)) +
      scale_alpha_manual('', values = c(0.4, 0.15))

    plt <-
      plt +
      guides(alpha = guide_legend(override.aes = list(fill = 'steelblue3'))
             , shape = guide_legend(override.aes = list(fill = 'steelblue3')))

    # Add criteria line if specified
    if(!is.null(criteria)) {
      # return(plt)

      plt <- plt +
        geom_hline(aes(yintercept = criteria, color = factor(criteria_lab)
                       , linetype = factor(criteria_lab))
                   , show.legend = T) +
        scale_color_manual('', values = c('red')) +
        scale_linetype_manual('', values = c('longdash'))
    }

    # add plot title if specified
    if(plot_title) {
      ttl <- title_labeler(nerr_site_id = station)

      plt <-
        plt +
        ggtitle(ttl) +
        theme(plot.title = element_text(hjust = 0.5))
    }

    plt <-
      plt +
      guides(alpha = guide_legend(override.aes = list(fill = 'steelblue3', linetype = 0), order = 2)
             , shape = guide_legend(override.aes = list(fill = 'steelblue3', linetype = 0), order = 1)
             , 'WQ Threshold' = guide_legend(order = 3))

    # Adjust theme
    plt <-
      plt +
      theme(strip.background = element_blank(),
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
      theme(legend.spacing.x = unit(3, 'pt'))

    return(plt)

  } else {
    tbl <- dat_month
    # tbl$station <- station
  }

}
