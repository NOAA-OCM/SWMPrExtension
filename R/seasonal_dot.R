#' Seasonal Dot Plot
#'
#' Plot average/min/max seasonal values faceted by season
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param lm_trend logical, add linear trend line?
#' @param lm_lab logical, add significance label? Statisically significant results will appear in bold.
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}} and \code{\link{y_labeler}}.
#'
#' @concept analyze
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @importFrom lubridate  year floor_date
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
#' @importFrom stats median
#'
#' @export
#'
#' @details This function summarizes minimum, mean, and maximum values calculated on a seasonal basis to allow for easier intra-season comparisons over time.
#'
#' \code{lm_trend = T} adds a linear regression to the plot, and \code{lm_lab = T} will add p-values from the linear regression to the plot. If the p-values are significant (p < 0.05) then the text will appear in bold. \code{lm_lab} text is color coded to match with the corresponding dots.
#'
#' @author Julie Padilla
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}, \code{\link{y_labeler}}
#'
#' @examples
#' \dontrun{
#'
#' dat_wq <- elksmwq
#' dat_wq <- subset(dat_wq, subset = c('2007-01-01 0:00', '2017-01-01 0:00'))
#' dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
#'
#' x <-
#'   seasonal_dot(dat_wq, param = 'do_mgl',
#'                , lm_trend = F
#'                , lm_lab = F
#'                , plot_title = T)
#'
#' x <-
#'   seasonal_dot(dat_wq, param = 'do_mgl',
#'                , lm_trend = T
#'                , lm_lab = F
#'                , plot_title = T)
#'
#' x <-
#'   seasonal_dot(dat_wq, param = 'do_mgl',
#'                , lm_trend = T
#'                , lm_lab = T
#'                , plot_title = T)
#'
#' dat_nut <- elksmnut
#' dat_nut <- subset(dat_nut, subset = c('2007-01-01 0:00', '2017-01-01 0:00'))
#' dat_nut <- qaqc(dat_nut, qaqc_keep = c(0, 3, 5))
#'
#' x <-
#'   seasonal_dot(dat_nut
#'                , param = 'chla_n'
#'                , season_grps = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12))
#'                , season_names = c('Winter', 'Spring', 'Summer', 'Fall')
#'                , season_start = 'Spring'
#'                , lm_trend = F
#'                , lm_lab = F
#'                , plot_title = T)
#'
#' x <-
#'   seasonal_dot(dat_nut, param = 'chla_n',
#'                , lm_trend = T
#'                , lm_lab = F
#'                , plot_title = T)
#'
#' x <-
#'   seasonal_dot(dat_nut, param = 'chla_n',
#'                , lm_trend = T
#'                , lm_lab = T
#'                , plot_title = T)
#' }

seasonal_dot <- function(swmpr_in, ...) UseMethod('seasonal_dot')

#' @rdname seasonal_dot
#'
#' @concept analyze
#'
#' @export
#'
#' @method seasonal_dot swmpr
#'
seasonal_dot.swmpr <- function(swmpr_in
                               , param = NULL
                               , lm_trend = FALSE
                               , lm_lab = FALSE
                               , log_trans = FALSE
							                 , converted = FALSE
                               , plot_title = FALSE
                               , plot = TRUE
                               , ...) {

  dat <- swmpr_in
  parm <- sym(param)
  conv <- converted

  seas <- sym('season')
  yr <- sym('year')

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  #determine type WQ, MET, NUT
  #determine log scale transformation
  if(substr(station, 6, nchar(station)) == 'nut' && !('season_names' %in% names(list(...))))
    warning('Nutrient data detected. Consider specifying seasons > 1 month.')

  #determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    warning('QAQC columns present. QAQC not performed before analysis.')

  # Assign the seasons and order them
  dat$season <- assign_season(dat$datetimestamp, abb = T, ...)

  # Assign date for determining daily stat value
  dat$year <- lubridate::year(dat$datetimestamp)

  # Filter for parameter of interest and remove missing values (in case there is a month with no data)
  dat <- dat[, c('year', 'season', param)]
  dat <- dat %>% dplyr::filter(!is.na(!! parm))

  # --------
  # calc seasonal values
  plt_data <- dat %>%
    group_by(!! yr, !! seas) %>%
    summarise(min = min(!! parm, na.rm = T)
              , mean = mean(!! parm, na.rm = T)
              , max = max(!! parm, na.rm = T)
              )

  if(plot) {
    agg_lab <- ifelse(length(levels(plt_data$season)) == 12, 'Monthly ', 'Seasonal ')

    labs_legend <- factor(paste0(agg_lab, c('Minimum', 'Average', 'Maximum'), sep = ''))
    brks <- range(plt_data$year)

    mx <- max(plt_data[ , c(3:5)]) *1.2 #max(dat_hist$result, na.rm = T)
    mx <- ceiling(mx)
    mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), 0)

    plt <-
      ggplot(data = plt_data, aes_string(x = 'year', y = 'min', color = labs_legend[1])) +
      geom_point() +
      geom_point(data = plt_data, aes_string(x = 'year', y = 'mean', color = labs_legend[2])) +
      geom_point(data = plt_data, aes_string(x = 'year', y = 'max', color = labs_legend[3])) +
      geom_point() +
      scale_color_manual('', values = c('black', 'red', 'blue')) +
      scale_x_continuous(breaks = seq(from = brks[1], to = brks[2], by = 1)) +
      facet_wrap(~ season) +
      labs(x = NULL, y = eval(y_label))

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


    # Adjust theme
    plt <-
      plt +
      theme_bw() +
      theme(legend.position = 'top'
            , legend.direction = 'horizontal') +
      theme(panel.grid.major = element_line(linetype = 'solid'),
            panel.grid.minor = element_line(linetype = 'solid'),
            strip.background = element_blank(),
            panel.border = element_rect(color = 'black')) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
            axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
      theme(text = element_text(size = 16))

    # Adjust legend keys and spacing
    plt <-
      plt +
      theme(legend.key.size = unit(7, 'pt')) +
      theme(legend.text = element_text(size = 8)) +
      theme(legend.spacing.x = unit(-6, 'pt'))

    # add regression line if specified
    if(lm_trend) {
      plt <-
        plt +
        geom_smooth(method = 'lm', se = F, lwd = 0.5) +
        geom_smooth(aes_string(x = 'year', y = 'mean', color = labs_legend[2])
                    , method = 'lm', se = F, lwd = 0.5) +
        geom_smooth(aes_string(x = 'year', y = 'max', color = labs_legend[3])
                    , method = 'lm', se = F, lwd = 0.5)
    }

    # add regression p-values if specified
    if(lm_lab) {

      p_labs <- lm_p_labs(plt_data)

      plt <-
        plt +
        annotate("text", x = brks[2], y = mx
                 , label = p_labs$max, fontface = ifelse(p_labs$max == 'p < 0.05', 2, 1)
                 , hjust = 1, color = 'red') +
        annotate("text", x = brks[2], y = mx * 0.9
                 , label = p_labs$mean, fontface = ifelse(p_labs$mean == 'p < 0.05', 2, 1)
                 , hjust = 1, color = 'black') +
        annotate("text", x = brks[2], y = mx * 0.8
                 , label = p_labs$min, fontface = ifelse(p_labs$min == 'p < 0.05', 2, 1)
                 , hjust = 1, color = 'blue')

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
    return(plt_data)
  }
}
