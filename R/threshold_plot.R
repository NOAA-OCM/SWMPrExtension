#' Water Quality Threshold Plot
#'
#' Visualize water quality exceedances
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot, also includes the calculated parameter dissolved inorganic nitrogen ('din'). See details for more information.
#' @param rng numeric vector, if range is not specified then the entire data set will be used.
#' @param thresholds numeric vector, numeric criteria that will be plotted in the background
#' @param threshold_labs chr vector of labels for categories created by \code{thresholds}.
#' @param monthly_smooth logical, calculate a monthly average? Defaults to \code{FALSE}
#' @param crit_threshold logical, should the critical threshold be highlighted with a bold, dashed line? Defaults to \code{FALSE}
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param ... additional arguments passed to other methods.#' @param ... additional arguments passed to other methods. See \code{\link{y_labeler}}.
#'
#' @concept analyze
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year
#'
#' @export
#'
#' @details Visualize exceedances of user-specified water quality thresholds.
#'
#' @author Julie Padilla
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#'

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
                                 , threshold_labs = c('Poor', 'Fair', 'Good')
                                 , crit_threshold = F
                                 , log_trans = FALSE
                                 , monthly_smooth = FALSE
                                 , plot_title = FALSE
                                 , ...) {

  dat <- swmpr_in
  parm <- sym(param)

  seas <- sym('season')
  res <- sym('result')
  dt <- sym('date')
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
  if(length(thresholds) != length(threshold_labs) + 1)
    stop('length(thresholds) must be one greater than length(threshold_labs).')

  #determine y axis transformation, y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, ...)

  ##filter for range
  if(!is.null(rng) & length(rng) > 1) {
    dat <- dat %>% filter(lubridate::year(.data$datetimestamp) <= max(rng)
                          , lubridate::year(.data$datetimestamp) <= min(rng))
  }

  #determine plotting color palette based on range
  ifelse(length(unique(rng) > 1), fill_col <- 'gray40', fill_col <- 'steelblue3') #check with colorcop

  if(param == 'din') {
    # Calculated the DIN
  }

  #do the plotting things here

  mx <- #check if max(dat) is greater than max threshold. If not then set mx to 0.1 * max(threshold) otherwise ceiling(mx)
  # mx <- ceiling(mx)
  mn <- ifelse(log_trans == TRUE, 0.1, 0)

  # plt <- ggplot(data = dat_hist, aes_(x = seas, y = res, fill = lab_bp_fill)) +
  #   geom_boxplot(outlier.size = 0.5) +
  #   scale_y_continuous(limits = c(mn, mx), trans = y_trans, labels = scales::comma) +
  #   scale_fill_manual(name = '', values = c('skyblue1')) +
  #   labs(x = NULL, y = eval(y_label)) +
  #   theme_bw() +
  #   theme(legend.position = 'top'
  #         , legend.direction = 'horizontal')

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
    theme(legend.key.size = unit(7, 'pt')) +
    theme(legend.text = element_text(size = 8)) +
    theme(legend.spacing.x = unit(-6, 'pt'))

  if(data_type == 'nut') {
    #add points to the plot
  }

  if(crit_threshold) {
    #Add the line for emphasis
  }

  if(monthly_smooth) {
    #check for inst data
    #calc monthly smooth and add it
    #fix the legend
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
