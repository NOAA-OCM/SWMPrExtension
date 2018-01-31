#' Boxplots of raw data by user-defined season for a target year
#'
#' @param swmpr_in input swmp object
#' @param param chr string of variable to plot
#' @param target_yr numeric, if target year is not specified then all data in the data frame will be used.
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param criteria numeric, a numeric criteria that will be plotted as a horizontal line
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}} and \code{\link{y_labeler}}.
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom scales comma
#'
#' @export
#'
#' @details This function produces boxplots of raw, unaggregated data by user-specified season for year of interest
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
#' ## get data, prep
#' data(elksmwq)
#' dat <- elksmwq
#'
#' dat <- qaqc(elksmwq, qaqc_keep = c('0', '3', '5'))
#' raw_boxplot(dat, param = 'do_mgl')
#'
#' }
#'
raw_boxplot <- function(swmpr_in, ...) UseMethod('raw_boxplot')

#' @rdname raw_boxplot
#'
#' @concept analyze
#'
#' @export
#'
#' @method raw_boxplot swmpr
#'
raw_boxplot.swmpr <- function(swmpr_in
                              , param = NULL
                              , target_yr = NULL
                              , criteria = NULL
                              , log_trans = FALSE
                              , converted = FALSE
                              , plot_title = FALSE
                              , ...) {

  dat <- swmpr_in
  parm <- sym(param)
  conv <- converted

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #CHECKS
  # determine if target year is present within the data
  if(!is.null(target_yr)) {
    if(!(target_yr %in% unique(year(dat$datetimestamp)))) {
      warning('User-specified target year is not present in the data set. target_yr argument will be set to max year in the data set')
      target_yr <- max(year(dat$datetimestamp))
    }
  } else {
    warning('No target year specified. Entire data set will be used.')
    target_yr <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
    target_yr <- unique(target_yr)
  }

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine type WQ, MET, NUT
  #IF WQ or MET then use "Instantaneous data" otherwise "Monthly data"
  #determine data type
  if(substr(station, 6, nchar(station)) == 'nut') {
    warning('Nutrient data detected. Consider specifying seasons > 1 month.')
    lab_data = 'Data'
  } else {
    lab_data = 'Instantaneous Data'
  }

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  #determine parameter column index
  parm_index <- grep(param, colnames(dat))

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)


  if(!is.null(target_yr))
    dat <- dat %>% dplyr::filter(year(.data$datetimestamp) == target_yr)

  # Assign the seasons and order them
  dat$season <- assign_season(dat$datetimestamp, abb = T, ...)

  mx <- max(dat[, parm_index], na.rm = T)
  mx <- max(pretty(mx))

  # assign a minimum of zero unles there are values < 0
  mn <- min(dat[, parm_index], na.rm = T)
  mn <- ifelse(mn < 0 , min(pretty(mn)), 0)
  mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), mn)

  bp_fill <- ifelse(length(unique(target_yr)) == 1, paste(lab_data, '\n(', target_yr, ')', sep = ''), paste(lab_data, '\n(', target_yr[1], '-', target_yr[2], ')', sep = ''))

  seas <- sym('season')

  # ensure all factor levels are accounted for, even if there is no data
  dat <- tidyr::complete(dat, !! seas)

  plt <- ggplot(data = dat, aes_(x = seas, y = parm, fill = factor(bp_fill))) +
    geom_boxplot(outlier.size = 0.5) +
    scale_fill_manual(name = '', values = c('skyblue1')) +
    labs(x = NULL, y = eval(y_label)) +
    theme_bw() +
    theme(legend.position = 'top'
          , legend.direction = 'horizontal')

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


  if(!is.null(criteria)) {

    plt <- plt +
      geom_hline(aes(yintercept = criteria, color = factor('WQ Threshold'), linetype = factor('WQ Threshold'))
                  , show.legend = T) +
      scale_color_manual('', values = c('WQ Threshold' = 'red')) +
      scale_linetype_manual('', values = c('WQ Threshold' = 'longdash'))

    plt <- plt + guides(fill = guide_legend(order = 1)
                    , 'WQ Threshold' = guide_legend(order = 2))


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
    theme(text = element_text(size = 16))

  # Adjust legend keys and spacing
  plt <-
    plt +
    theme(legend.key.height = unit(0.1, 'cm')
          , legend.key.width = unit(0.5, 'cm')) +
    theme(legend.text = element_text(size = 10)
          , legend.text.align = 0.5) +
    theme(legend.spacing.x = unit(-6, 'pt'))

  return(plt)
}
