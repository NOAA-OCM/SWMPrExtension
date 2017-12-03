#' Water Quality Threshold Plot
#'
#' Visualize water quality exceedances
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot, also includes the calculated parameter dissolved inorganic nitrogen ('din'). See details for more information.
#' @param rng numeric vector, if range is not specified then the entire data set will be used.
#' @param thresholds numeric vector, numeric criteria that will be plotted in the background
#' @param threshold_labs chr vector of labels for categories created by \code{thresholds}.
#' @param threshold_cols chr vector of color values for categories created by \code{thresholds}.
#' @param monthly_smooth logical, calculate a monthly average? Defaults to \code{FALSE}
#' @param crit_threshold num, value at which the critical threshold line should be plotted. Typically the same value used to establish the 'Poor' threshold.
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param ... additional arguments passed to other methods.#' @param ... additional arguments passed to other methods. See \code{\link{y_labeler}}.
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year month
#'
#' @export
#'
#' @details This function vizualizes exceedances of numeric criteria. Suggested numeric criteria for several parameters (dissolved oxygen, dissolved inorganic phosphorus, dissolved inorganic nitrogen, and chlorophyll-a) can be found in the USEPA National Coastal Condition Report (2012).
#'
#' If the parameter of interest does not have numeric criteria, then \code{threshold_percentile_plot}
#'
#' @author Julie Padilla
#'
#' @concept analyze

#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' \dontrun{
#' data(apacpwq)
#' dat_wq <- apacpwq
#'
#' dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
#'
#' ## Due to the volume of instantaneous data, these plots are a bit slow
#' x <-
#'   threshold_plot(dat_wq, param = 'do_mgl'
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Poor', 'Fair', 'Good')
#'                  , monthly_smooth = T
#'                  , threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9'))
#'
#'
#' y <-
#'   threshold_plot(dat_wq, param = 'do_mgl'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Poor', 'Fair', 'Good')
#'                  , threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9'))
#'
#' z <-
#'   threshold_plot(dat_wq, param = 'do_mgl'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Poor', 'Fair', 'Good')
#'                  , threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9')
#'                  , monthly_smooth = T)
#'
#'
#' data(apacpnut)
#' dat_nut <- apacpnut
#'
#' dat_nut <- qaqc(dat_nut, qaqc_keep = c(0, 3, 5))
#' dat_nut <- rem_reps(dat_nut)
#'
#' x <-
#'   threshold_plot(dat_nut, param = 'chla_n'
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Good', 'Fair', 'Poor'))
#'
#'
#' y <-
#'   threshold_plot(dat_nut, param = 'chla_n'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Good', 'Fair', 'Poor'))
#'
#' ## Nutrient plots are not capable of accidentally displaying any kind of smooth
#' z <-
#'   threshold_plot(dat_nut, param = 'chla_n'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Good', 'Fair', 'Poor')
#'                  , monthly_smooth = T)
#' }

threshold_plot <- function(swmpr_in, ...) UseMethod('threshold_plot')

#' @rdname threshold_plot
#'
#' @concept analyze
#'
#' @export
#'
#' @method threshold_plot swmpr
#'
threshold_plot.swmpr <- function(swmpr_in
                                 , param = NULL
                                 , rng = NULL
                                 , thresholds = NULL
                                 , threshold_labs = c('Good', 'Fair', 'Poor')
                                 , threshold_cols = c('#ABD9E9', '#FFFFCC', '#FEC596')
                                 , crit_threshold = NULL
                                 , log_trans = FALSE
                                 , monthly_smooth = FALSE
                                 , plot_title = FALSE
                                 , ...) {

  dat <- swmpr_in
  parm <- sym(param)

  dt <- sym('datetimestamp')
  avg <- sym('mean')

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')
  data_type <- substr(station, 6, nchar(station))

  #TESTS
  #determine range exists, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No range specified. Minimum and maximum year in data set will be used.')
    rng <- c(min(lubridate::year(dat$datetimestamp), na.rm = T), max(lubridate::year(dat$datetimestamp), na.rm = T))
  }

  #determine that variable name exists
  if(!any(param %in% parameters | param != 'din'))
    stop('Param argument must name input column')

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  #determine if a reasonable number of thresholds and labels have been specified
  if(length(thresholds) + 1 != length(threshold_labs))
    stop('length(threshold_labs) must be one greater than length(thresholds).')

  #determine y axis transformation, y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, ...)

  ##filter for range
  if(!is.null(rng)) {
    dat <- dat %>% filter(lubridate::year(.data$datetimestamp) <= max(rng)
                          , lubridate::year(.data$datetimestamp) >= min(rng))
  }

  if(param == 'din') {
    dat$din <- dat$no2f + dat$no3f + dat$nh4f
  }

  if(param == 'dip') {
    dat$din <- dat$po4f
  }

  # plot prep
  #determine plotting color palette based on range
  ts_col <- ifelse(length(unique(rng)) > 1, '#B3B3B3', '#4F94CD') #check with colorcop
  smooth_col <- ifelse(length(unique(rng)) > 1, '#7F7F7F', '#36648B') #check with colorcop
  smooth_ln <- ifelse(length(unique(rng)) > 2, 'solid', 'dashed')

  brks <- set_date_breaks(rng)
  lab_brks <- set_date_break_labs(rng)
# return(brks)
  # set y axis range
  mx <- max(dat[, grep(param, colnames(dat))], na.rm = T)
  mx <- ifelse(max(thresholds) > mx, 1.1 * max(thresholds), mx)
  mx <- ceiling(mx)
  mn <- ifelse(log_trans == TRUE, 0.1, 0)

  # set legend label and time series line type
  lab_dat <- ifelse(length(unique(rng)) > 1, paste(rng[[1]], '-', rng[[2]], ' Data', sep = ''), paste(rng[[1]], ' Data', sep = ''))
  ts_ln <- 'solid'

  plt <-
    ggplot(data = dat, aes_(x = dt, y = parm)) +
    geom_rect(data = NULL, aes(xmin = as.POSIXct(-Inf, origin = "1960-01-01")
                               , xmax = as.POSIXct(Inf, origin = "1970-01-01")
                               , ymin = -Inf, ymax = thresholds[1])
              , fill = threshold_cols[1]) +
    geom_rect(data = NULL, aes(xmin = as.POSIXct(-Inf, origin = "1960-01-01")
                               , xmax = as.POSIXct(Inf, origin = "1970-01-01")
                               , ymin = thresholds[1], ymax = thresholds[2])
              , fill = threshold_cols[2]) +
    geom_rect(data = NULL, aes(xmin = as.POSIXct(-Inf, origin = "1960-01-01")
                               , xmax = as.POSIXct(Inf, origin = "1970-01-01")
                               , ymin = thresholds[2], ymax = Inf)
              , fill = threshold_cols[3])

  plt <-
    plt +
    geom_line(aes(color = lab_dat, linetype = lab_dat), lwd = 1) +
    scale_x_datetime(date_breaks = brks, date_labels = lab_brks) +
    scale_y_continuous(limits = c(mn, mx)) +
    scale_color_manual('', values = c(ts_col)) +
    scale_linetype_manual('', values = c(ts_ln)) +
    labs(x = NULL, y = eval(y_label)) +
    theme_bw() +
    theme(legend.position = 'top'
          , legend.direction = 'horizontal')

  # add background labels
  plt <-
    plt +
    annotate("text", x = max(dat$datetimestamp), y = mx
             , label = threshold_labs[3], fontface = 2, hjust = 1) +
    annotate("text", x = max(dat$datetimestamp), y = thresholds[1] + (thresholds[2] - thresholds[1]) / 2
             , label = threshold_labs[2], fontface = 2, hjust = 1) +
    annotate("text", x = max(dat$datetimestamp), y = mn
             , label = threshold_labs[1], fontface = 2, hjust = 1)

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
    theme(legend.key.size = unit(7, 'pt')) +
    theme(legend.text = element_text(size = 8)) +
    theme(legend.spacing.x = unit(-6, 'pt'))

  if(data_type == 'nut') {
    plt <-
      plt +
      geom_point(aes(fill = lab_dat), size = 2, shape = 21) +
      scale_fill_manual('', values = c(ts_col))
  }

  if(!is.null(crit_threshold)) {
    #Add the line for emphasis
    plt <-
      plt +
      geom_hline(yintercept = crit_threshold, linetype = 'longdash', color = 'orange', lwd = 2)

  }

  if(monthly_smooth && data_type != 'nut') {

    lab_smooth <- ifelse(length(unique(rng)) > 1, paste(rng[[1]], '-', rng[[2]], ' Monthly Average', sep = ''), paste(rng[[1]], ' Monthly Average', sep = ''))

    df_smooth <- dat %>%
      group_by(year = lubridate::year(!! dt), month = lubridate::month(!! dt)) %>%
      summarise(mean = mean(!! parm, na.rm = T)) %>%
      mutate(datetimestamp = paste(year, '-', month, '-', '01', ' ', '0:00', sep = ''))

    df_smooth$datetimestamp <- as.POSIXct(df_smooth$datetimestamp)

    plt <-
      plt +
      geom_line(data = df_smooth, aes_(x = dt, y = avg, color = lab_smooth, linetype = lab_smooth), lwd = 1) +
      scale_color_manual('', values = c(ts_col, smooth_col)) +
      scale_linetype_manual('', values = c(ts_ln, smooth_ln))

  }

  return(plt)

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
