#' Threshold Percentile Plot
#'
#' Observed data compared against user-defined percentiles
#'
#' @param swmpr_in input swmpr object
#' @param param chr, variable to plot
#' @param hist_rng num, years to include in the plot. This variable can either be one year (e.g., \code{hist_rng = 2012}), or two years (e.g. \code{hist_rng = c(2012, 2016)}) , If range is not specified then the entire data set will be used.
#' @param target_yr num, year of interest for plotting. If not specified, the entire data set will be plotted.
#' @param percentiles num, percentiles to calculate (maximum: 2). Defaults to 5th and 95th percentiles.
#' @param free_y logical, should the y-axis be free? Defaults to \code{FALSE}. If \code{FALSE}, defaults to zero, unless negative values are present. If \code{TRUE}, y-axis limits are selected by \code{ggplot}
#' @param by_month logical. should percentiles be calculated on a monthly basis? Defaults to \code{FALSE}
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param ... additional arguments passed to other methods (not used for this function).
#'
#' @import ggplot2 dplyr rlang
#'
#' @importFrom dplyr filter group_by left_join summarise
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  ymd_hms month year
#' @importFrom scales format_format
#' @importFrom stats quantile
#'
#' @export
#'
#' @details This function provides an alternative to \code{\link{threshold_criteria_plot}}. For parameters that may not have numeric threshold criteria, a percentile threshold can be used instead. For a one-tailed analysis, the 90-th percentile is recommended. For a two-tailed analysis, the 5-th and 95-th percentiles are recommended.
#'
#' Using \code{by_month}, the user can specify whether the percentiles should be calculated on a monthly basis or by using the entire data set.
#'
#' Recommended thresholds for chlorophyll-a, dissolved inorganic nitrogen, dissolved inorganic phosphorus, and dissolved oxygen can be found in the National Coastal Condition Assessment 2010 (USEPA 2016)
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @references
#' United States Environmental Protection Agency (USEPA). 2016. "National Coastal Condition Assessment 2010". EPA 841-R-15-006.
#' https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=327030
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' dat_wq <- qaqc(elksmwq, qaqc_keep = c(0, 3, 5))
#' dat_wq <- subset(dat_wq, subset = '2007-01-01 0:00', operator = '>=')
#'
#' x <-
#'   threshold_percentile_plot(dat_wq, param = 'do_mgl'
#'   , hist_rng = c(2007, 2014), by_month = FALSE)
#'
#' \dontrun{
#' y <-
#'   threshold_percentile_plot(dat_wq, param = 'do_mgl', percentiles = c(0.95)
#'   , hist_rng = c(2007, 2014), target_yr = 2014, by_month = FALSE)
#'
#' x2 <-
#'   threshold_percentile_plot(dat_wq, param = 'do_mgl'
#'   , hist_rng = c(2007, 2014), by_month = TRUE)
#'
#' y2 <-
#'   threshold_percentile_plot(dat_wq, param = 'do_mgl'
#'   , hist_rng = c(2007, 2014), target_yr = 2014, by_month = TRUE)
#'
#'
#' dat_nut <- qaqc(elknmnut, qaqc_keep = c(0, 3, 5))
#' dat_nut <- subset(dat_nut, subset = '2007-01-01 0:00', operator = '>=')
#' dat_nut <- rem_reps(dat_nut)
#'
#' x <-
#'   threshold_percentile_plot(dat_nut, param = 'chla_n'
#'   , hist_rng = c(2007, 2014), by_month = FALSE)
#'
#' y <-
#'   threshold_percentile_plot(dat_nut, param = 'chla_n'
#'   , hist_rng = c(2007, 2014), target_yr = 2016, by_month = FALSE)
#' }
#'
threshold_percentile_plot <- function(swmpr_in, ...) UseMethod('threshold_percentile_plot')

#' @rdname threshold_percentile_plot
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
                                         , free_y = FALSE
                                         , by_month = FALSE
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
  #determine historical range exists and that it is reasonable, if not default to min/max of the range
  x <- dat[ , c('datetimestamp', param)]
  x <- x[complete.cases(x), ]

  if(is.null(hist_rng)) {
    warning('No historical range specified. Entire time series will be used.')
    hist_rng <- c(min(lubridate::year(x$datetimestamp)), max(lubridate::year(x$datetimestamp)))
  } else {
    if(min(hist_rng) < min(lubridate::year(x$datetimestamp)) | max(hist_rng) > max(lubridate::year(x$datetimestamp))) {
      warning('Specified range is greater than the range of the dataset. Max/min  range of the dataset will be used.')
      hist_rng <- c(min(lubridate::year(x$datetimestamp)), max(lubridate::year(x$datetimestamp)))
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

  mx <- ifelse(max(dat_subset[ , 2], na.rm = T) > max(bars$perc_hi), max(dat_subset[ , 2], na.rm = T), max(bars$perc_hi))
  mx <- ifelse(data_type == 'nut' && param != 'chla_n', ceiling(mx/0.01) * 0.01, ceiling(mx))

  if(length(percentiles) > 1) {
    mn <- ifelse(min(dat_subset[ , 2], na.rm = T) < min(bars$perc_lo), min(dat_subset[ , 2], na.rm = T), min(bars$perc_lo))
    mn <- ifelse(data_type == 'nut', 0, floor(mn))
  } else {
    mn <- ifelse(min(dat_subset[ , 2], na.rm = T) < min(bars$perc_hi), min(dat_subset[ , 2], na.rm = T), min(bars$perc_hi)) #note: perc_lo DNE when percentiles < 2
    mn <- ifelse(data_type == 'nut', 0, ceiling(mx))
  }
  mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), mn)


  # plot ----
  plt <- ggplot(dat_subset, aes_(x = dt, y = parm, color = lab_dat)) +
    geom_line(lwd = 1) +
    scale_x_datetime(date_breaks = brks, date_labels = lab_brks)

  # add a log transformed access if log_trans = T
  if(!log_trans) {

    plt <- plt +
      scale_y_continuous(labels = format_format(digits = 2, big.mark = ",", decimal.mark = ".", scientific = FALSE)
                         , breaks = pretty_breaks(n = 8))

    if(!free_y){plt <- plt + expand_limits(y = mn)}

  } else {

    mx_log <- 10^(ceiling(log10(mx)))

    mag_lo <- nchar (mn) - 2
    mag_hi <- nchar(mx_log) - 1

    brks <- 10^(-mag_lo:mag_hi)

    plt <- plt +
      scale_y_continuous(breaks = brks, trans = y_trans
                         , labels = format_format(digits = 2, big.mark = ",", decimal.mark = ".", scientific = FALSE))

    if(!free_y) {plt <- plt + expand_limits(y = mn)}
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
    theme(plot.margin = margin(5.5, 11, 11, 5.5, 'pt')) +
    theme(axis.title.y = element_text(margin = margin(0, 8, 0, 0, 'pt'), angle = 90)) + #trbl
    theme(text = element_text(size = 16))

  # Adjust legend keys and spacing
  plt <-
    plt +
    theme(legend.key.height = unit(0.1, 'cm')
          , legend.key.width = unit(0.5, 'cm')) +
    theme(legend.text = element_text(size = 10)
          , legend.text.align = 0.5) +
    theme(legend.spacing.x = unit(3, 'pt'))

  plt <-
    plt +
    guides(fill = guide_legend(order = 1)
           , color = guide_legend(order = 2, reverse = T))

  # add plot title if specified
  if(plot_title) {
    ttl <- title_labeler(nerr_site_id = station)

    plt <-
      plt +
      ggtitle(ttl) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  return(plt)

}
