#' Water Quality Threshold Plot For Parameters With Criteria
#'
#' Observed data compared against user-defined water quality thresholds
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of the variable to plot
#' @param rng num, years to include in the plot. This variable can either be one year (e.g., \code{rng = 2012}), or two years (e.g. \code{rng = c(2012, 2016)}) , If range is not specified then the entire data set will be used.
#' @param thresholds numeric vector, numeric criteria that will be plotted in the background
#' @param threshold_labs chr vector of labels for categories created by \code{thresholds}.
#' @param threshold_cols chr vector of color values for categories created by \code{thresholds}.
#' @param monthly_smooth logical, calculate a monthly average? Defaults to \code{FALSE}
#' @param crit_threshold num, value at which the critical threshold line should be plotted. Typically the same value used to establish the 'Poor' threshold.
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param ... additional arguments passed to other methods. See \code{\link{y_labeler}}.
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter group_by mutate summarise
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year month
#' @importFrom rlang .data
#' @importFrom tidyr complete
#'
#' @export
#'
#' @details This function visualizes exceedances of numeric criteria which are specified using \code{thresholds}. Suggested numeric criteria for several parameters (dissolved oxygen, dissolved inorganic phosphorus, dissolved inorganic nitrogen, and chlorophyll-a) can be found in the USEPA National Coastal Condition Report (2012).
#'
#' If the parameter of interest does not have numeric criteria, then \code{threshold_percentile_plot} is recommended.
#'
#'
#' @references
#' United States Environmental Protection Agency (USEPA). 2012. "National Coastal Condition Report IV."
#' http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.646.1973&rep=rep1&type=pdf
#'
#' @author Julie Padilla
#'
#' @concept analyze

#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}},\code{\link{y_labeler}}
#'
#' @examples
#' data(apacpwq)
#' dat_wq <- apacpwq
#'
#' dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
#'
#' ## Due to the volume of instantaneous data, these plots are a bit slow
#' x <-
#'   threshold_criteria_plot(dat_wq, param = 'do_mgl'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Poor', 'Fair', 'Good')
#'                  , monthly_smooth = TRUE
#'                  , threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9'))
#'
#' \dontrun{
#' y <-
#'   threshold_criteria_plot(dat_wq, param = 'do_mgl'
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Poor', 'Fair', 'Good')
#'                  , threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9'))
#'
#' z <-
#'   threshold_criteria_plot(dat_wq, param = 'do_mgl'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Poor', 'Fair', 'Good')
#'                  , threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9')
#'                  , monthly_smooth = TRUE)
#'
#' ## A few examples with only two thresholds
#' xx <-
#'   threshold_criteria_plot(dat_wq, param = 'do_mgl'
#'                  , rng = 2012
#'                  , thresholds = c(2, 2)
#'
#'                   # A dummy blank ('') value must be added as a threshold label
#'                  , threshold_labs = c('Poor', '', 'Good')
#'                  , threshold_cols = c('#FEC596', '#FFFFCC', '#ABD9E9')
#'                  , monthly_smooth = TRUE)
#'
#' xy <-
#'   threshold_criteria_plot(dat_wq, param = 'do_mgl'
#'                  , rng = 2012
#'                  , thresholds = c(5, 5)
#'
#'                  # A dummy blank ('') value must be added as a threshold label
#'                  , threshold_labs = c('Poor', '', 'Good')
#'                  , threshold_cols = c('#FEC596', '#FEC596', '#ABD9E9')
#'                  , monthly_smooth = TRUE)
#'
#' xz <-
#'   threshold_criteria_plot(dat_wq, param = 'do_mgl'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Poor', 'Good', 'Poor')
#'                  , threshold_cols = c('#FEC596', '#ABD9E9', '#FEC596')
#'                  , monthly_smooth = TRUE)
#'
#'
#' data(apacpnut)
#' dat_nut <- apacpnut
#'
#' dat_nut <- qaqc(dat_nut, qaqc_keep = c(0, 3, 5))
#' dat_nut <- rem_reps(dat_nut)
#'
#' x <-
#'   threshold_criteria_plot(dat_nut, param = 'chla_n'
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Good', 'Fair', 'Poor'))
#'
#'
#' y <-
#'   threshold_criteria_plot(dat_nut, param = 'chla_n'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Good', 'Fair', 'Poor'))
#'
#' ## Nutrient plots are not capable of accidentally displaying any kind of smooth
#' z <-
#'   threshold_criteria_plot(dat_nut, param = 'chla_n'
#'                  , rng = 2012
#'                  , thresholds = c(2, 5)
#'                  , threshold_labs = c('Good', 'Fair', 'Poor')
#'                  , monthly_smooth = TRUE)
#' }

threshold_criteria_plot <- function(swmpr_in, ...) UseMethod('threshold_criteria_plot')

#' @rdname threshold_criteria_plot
#'
#' @export
#'
#' @method threshold_criteria_plot swmpr
#'
threshold_criteria_plot.swmpr <- function(swmpr_in
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
  #determine historical range exists and that it is reasonable, if not default to min/max of the range
  x <- dat[ , c('datetimestamp', param)]
  x <- x[complete.cases(x), ]

  if(is.null(rng)) {
    warning('No historical range specified. Entire time series will be used.')
    rng <- c(min(lubridate::year(x$datetimestamp)), max(lubridate::year(x$datetimestamp)))
  } else if (length(rng) > 1) {
    if(min(rng) < min(lubridate::year(x$datetimestamp)) | max(rng) > max(lubridate::year(x$datetimestamp))) {
      warning('Specified range is greater than the range of the dataset. Max/min  range of the dataset will be used.')
      rng <- c(min(lubridate::year(x$datetimestamp)), max(lubridate::year(x$datetimestamp)))
    }
  } else {
    if(!(rng %in% unique(year(x$datetimestamp)))) {
      warning('User-specified rng is not present in the data set. rng argument will be set to max year in the data set')
      rng <- max(year(x$datetimestamp))
    }
  }

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

  # plot prep
  #determine plotting color palette based on range
  ts_col <- ifelse(length(unique(rng)) > 1, '#B3B3B3', '#4F94CD')
  smooth_col <- ifelse(length(unique(rng)) > 1, '#7F7F7F', '#36648B')
  smooth_ln <- ifelse(length(unique(rng)) > 1, 'solid', 'dashed')

  brks <- set_date_breaks(rng)
  lab_brks <- set_date_break_labs(rng)

  # set y axis range
  mx <- max(dat[, grep(param, colnames(dat))], na.rm = TRUE)
  mx <- ifelse(max(thresholds) > mx, 1.1 * max(thresholds), mx)
  mx <- ifelse(data_type == 'nut' && param != 'chla_n', ceiling(mx/0.01) * 0.01, ceiling(mx))

  # assign a minimum of zero unless there are values < 0
  mn <- min(dat[, grep(param, colnames(dat))], na.rm = TRUE)
  mn <- ifelse(mn < 0 , min(pretty(mn)), 0)
  mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), mn)

  # set legend label and time series line type
  lab_dat <- ifelse(length(unique(rng)) > 1, paste('Data \n(', rng[[1]], '-', rng[[2]], ')', sep = ''), paste('Data \n(', rng[[1]], ')', sep = ''))
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
    theme(plot.margin = margin(5.5, 11, 11, 5.5, 'pt')) +
    theme(axis.title.y = element_text(margin = margin(0, 8, 0, 0, 'pt'), angle = 90)) + #trbl
    # theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) + #trbl
    theme(text = element_text(size = 16))

  # Adjust legend keys and spacing
  plt <-
    plt +
    theme(legend.key.height = unit(0.1, 'cm')
          , legend.key.width = unit(0.5, 'cm')) +
    theme(legend.text = element_text(size = 10)
          , legend.text.align = 0.5) +
    theme(legend.spacing.x = unit(3, 'pt'))

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

    lab_smooth <- ifelse(length(unique(rng)) > 1, paste('Monthly Average \n(', rng[[1]], '-', rng[[2]], ')', sep = ''), paste('Monthly Average \n(', rng[[1]], ')', sep = ''))

    df_smooth <- dat %>%
      group_by(year = lubridate::year(!! dt), month = lubridate::month(!! dt)) %>%
      summarise(mean = mean(!! parm, na.rm = TRUE)) %>%
      mutate(datetimestamp = paste(year, '-', month, '-', '01', ' ', '0:00', sep = ''))

    df_smooth$datetimestamp <- as.POSIXct(df_smooth$datetimestamp)

    plt <-
      plt +
      geom_line(data = df_smooth, aes_(x = dt, y = avg, color = lab_smooth, linetype = lab_smooth), lwd = 1) +
      scale_color_manual('', values = c(ts_col, smooth_col)) +
      scale_linetype_manual('', values = c(ts_ln, smooth_ln))

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

}
