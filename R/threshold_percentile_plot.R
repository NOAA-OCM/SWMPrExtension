#' Threshold Percentile Plot
#'
#' Observed data compared against user-defined percentiles
#'
#' @param swmpr_in input swmp object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector of year(s) used to calculate percentiles, if range is not specified then the min/max values of the data set will be used.
#' @param target_yr numeric, year of interest for plotting. If not, specified, the entire data set will be plotted.
#' @param percentiles numeric vector of percentiles to calculate (maximum: 2). Defaults to 5th and 95th percentiles.
#' @param by_month logical. should percentiles be calculated on a monthly basis? Defaults to \code{FALSE}
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param ... additional arguments passed to other methods (not used for this function).
#'
#' @import ggplot2 dplyr rlang
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  ymd_hms month year
#' @importFrom scales comma
#' @importFrom stats quantile
#'
#' @export
#'
#' @details This function provides an alternative to \code{threshold_plot}. For parameters that may not have numeric threshold criteria, a percentile threshold can be used instead. For a one-tailed analysis, the 90-th percentile is recommended. For a two-tailed analysis, the 5-th and 95-th percentiles are recommended.
#'
#' Using \code{by_month}, the user can specify whether the percentiles should be calculated on a monthly basis or by using the entire data set.
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
#' dat_wq <- qaqc(elksmwq, qaqc_keep = c(0, 3, 5))
#' dat_wq <- subset(dat_wq, subset = '2007-01-01 0:00', operator = '>=')
#'
#' x <-
#'   threshold_percentile_plot(dat_wq, param = 'do_mgl'
#'   , hist_rng = c(2007, 2014), by_month = F)
#'
#' y <-
#'   threshold_percentile_plot(dat_wq, param = 'do_mgl', percentiles = c(0.95)
#'   , hist_rng = c(2007, 2014), target_yr = 2014, by_month = F)
#'
#' x2 <-
#'   threshold_percentile_plot(dat_wq, param = 'do_mgl'
#'   , hist_rng = c(2007, 2014), by_month = T)
#'
#' y2 <-
#'   threshold_percentile_plot(dat_wq, param = 'do_mgl'
#'   , hist_rng = c(2007, 2014), target_yr = 2014, by_month = T)
#'
#'
#' dat_nut <- qaqc(elknmnut, qaqc_keep = c(0, 3, 5))
#' dat_nut <- subset(dat_nut, subset = '2007-01-01 0:00', operator = '>=')
#' dat_nut <- rem_reps(dat_nut)
#'
#' x <-
#'   threshold_percentile_plot(dat_nut, param = 'chla_n'
#'   , hist_rng = c(2007, 2014), by_month = F)
#'
#' y <-
#'   threshold_percentile_plot(dat_nut, param = 'chla_n'
#'   , hist_rng = c(2007, 2014), target_yr = 2016, by_month = F)
#' }
#'
threshold_percentile_plot <- function(swmpr_in, ...) UseMethod('threshold_percentile_plot')

#' @rdname threshold_percentile_plot
#'
#' @concept analyze
#'
#' @export
#'
#' @method threshold_percentile_plot swmpr
#'
threshold_percentile_plot.swmpr <- function(swmpr_in
                                         , param = NULL
                                         , hist_rng = NULL
                                         , target_yr = NULL
                                         , percentiles = c(0.05, 0.95)
                                         , by_month = TRUE
                                         , log_trans = FALSE
                                         , converted = FALSE
                                         , plot_title = FALSE
                                         , ...){
  dat <- swmpr_in
  parm <- sym(param)
  conv <- converted
  grp <- sym(ifelse(by_month, 'month', 'dummy'))

  dt <- sym('datetimestamp')
  yr <- sym('year')
  mo <- sym('month')

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')
  data_type <- substr(station, 6, nchar(station))

  #TESTS
  #determine range exists, if not default to min/max of the range
  if(is.null(hist_rng)) {
    warning('No range specified. Minimum and maximum year in data set will be used.')
    hist_rng <- c(min(lubridate::year(dat$datetimestamp), na.rm = T), max(lubridate::year(dat$datetimestamp), na.rm = T))
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  #determine y axis transformation, y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

  ##filter for range
  if(!is.null(hist_rng)) {
    dat <- dat %>% filter(lubridate::year(.data$datetimestamp) <= max(hist_rng)
                          , lubridate::year(.data$datetimestamp) >= min(hist_rng))
  }

  dat_subset <- dat[ , c('datetimestamp', param)]
  dat_subset$year <- lubridate::year(dat_subset$datetimestamp)

  if(by_month) {
    dat_subset$month <- lubridate::month(dat_subset$datetimestamp)
  }

  dat_subset$dummy <- -999

  # calculate percentiles and format for plotting ----
  if(length(percentiles) > 1) {
    bars <- dat_subset %>%
      group_by(!! grp) %>%
      summarise(perc_hi = quantile(!! parm, probs = max(percentiles), na.rm = T)
                , perc_lo = quantile(!! parm, probs = min(percentiles), na.rm = T))
  } else {
    bars <- dat_subset %>%
      group_by(!! grp) %>%
      summarise(perc_hi = quantile(!! parm, probs = percentiles, na.rm = T))
  }

  if(!is.null(target_yr)){
    dat_subset <- dat_subset %>% filter(year == target_yr)
  }

  mn_yr <- min(lubridate::year(dat_subset$datetimestamp))
  mx_yr <- max(lubridate::year(dat_subset$datetimestamp))

  yr_ct <- mx_yr - mn_yr + 1

  # add dummy data to bars
  dummy <- data.frame(month = rep(c(1:12), yr_ct), year = rep(c(mn_yr:mx_yr), each = 12), dummy = -999, stringsAsFactors = F)
  dummy[nrow(dummy) + 1 , ] <- c(1, max(dummy$year) + 1, -999)

  bar_plt <- left_join(dummy, bars)
  bar_plt$datetimestamp <- lubridate::ymd_hms(paste(bar_plt$year, bar_plt$month, '1 00:00:00', sep = '-'))

  # set a few labels and colors ----
  col_ln <- ifelse(length(unique(dat_subset$year)) > 1, 'gray80', 'steelblue3')
  fill_ln <- ifelse(length(unique(dat_subset$year)) > 1, 'gray80', 'steelblue3')
  lab_perc <- ifelse(length(percentiles) > 1, paste(min(percentiles) * 100, 'th and ', max(percentiles) * 100, 'th percentiles', sep = '')
                     , paste(percentiles * 100, 'th percentile', sep = ''))
  lab_yr <- ifelse(length(hist_rng) > 1, paste('\n(', min(hist_rng), '-', max(hist_rng), ')', sep = ''), paste('\n(', hist_rng, ')', sep = ''))
  lab_perc <- paste(lab_perc, lab_yr, sep = '')
  lab_tgt <- ifelse(is.null(target_yr), lab_yr, paste('\n(', target_yr, ')', sep = ''))
  lab_dat <- paste('Obs Data ', lab_tgt, sep = '')

  brks <- ifelse(is.null(target_yr), set_date_breaks(hist_rng), set_date_breaks(target_yr))
  lab_brks <- ifelse(is.null(target_yr), set_date_break_labs(hist_rng), set_date_break_labs(target_yr))

  mx <- max(dat_subset[ , 2], na.rm = T)
  mx <- ceiling(mx)
  mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), 0)

  # return(y_trans)
  # y_trans_inv <- ifelse(log_trans, function(x) 10 ^ x, 'identity')
  # numticks <- ifelse(log_trans, 4, 5)

  # plot ----

  ##this works
  # plt <- ggplot(dat_subset, aes_(x = dt, y = parm, color = lab_dat)) +
  #   geom_line(lwd = 1) +
  #   scale_x_datetime(date_breaks = brks, date_labels = lab_brks) +
  #   scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = scales::comma)

  plt <- ggplot(dat_subset, aes_(x = dt, y = parm, color = lab_dat)) +
    geom_line(lwd = 1) +
    scale_x_datetime(date_breaks = brks, date_labels = lab_brks)

  # add a log transformed access if log_trans = T
  if(!log_trans) {

    plt <- plt + scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = scales::comma)

  } else {

    mx_log <- 10^(ceiling(log10(mx)))

    mag_lo <- nchar (mn) - 2
    mag_hi <- nchar(mx_log) - 1

    brks <- 10^(-mag_lo:mag_hi)

    plt <- plt + scale_y_continuous(limits = c(mn, mx_log), breaks = brks, trans = y_trans, labels = scales::comma)
  }

  if(length(percentiles) > 1) {

    p_hi <- sym('perc_hi')
    p_lo <- sym('perc_lo')

    plt <-
      plt +
      geom_line(data = bar_plt
              , aes_(x = dt, y = p_hi, color = lab_perc)
              , lwd = 1) +
      geom_line(data = bar_plt
                , aes_(x = dt, y = p_lo, color = lab_perc)
                , lwd = 1)

  } else {
    p_hi <- sym('perc_hi')

    plt <-
      plt +
      geom_line(data = bar_plt, aes_(x = dt, y = p_hi, color = lab_perc)
                , lwd = 1
                , inherit.aes = F)
  }


  if(data_type == 'nut') {
    plt <-
      plt +
      geom_point(aes(fill = lab_dat), color = 'black', size = 2, shape = 21) +
      scale_fill_manual('', values = c(fill_ln))
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
    scale_color_manual('', values = c('red', col_ln)) +
    theme_bw() +
    theme(legend.position = 'top'
          , legend.direction = 'horizontal')

  plt <-
    plt +
    labs(x = '', y = eval(y_label))

  # adjust theme
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

  plt <-
    plt +
    guides(fill = guide_legend(order = 1)
           , color = guide_legend(order = 2, reverse = T))


  return(plt)

}
