#' Seasonal Dot Plot
#'
#' Plot average/min/max seasonal values faceted by season
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param lm_trend logical, add linear trend line?
#' @param lm_lab logical, add significance label? Statistically significant results will appear in bold.
#' @param free_y logical, should the y-axis be free? Defaults to \code{FALSE}. If \code{FALSE}, defaults to zero, unless negative values are present. If \code{TRUE}, y-axis limits are selected by \code{ggplot}
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @concept analyze
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter group_by summarise
#' @importFrom lubridate  year floor_date
#' @importFrom magrittr "%>%"
#' @importFrom tidyr complete gather
#' @importFrom rlang .data
#' @importFrom scales format_format breaks_pretty
#' @importFrom stats median
#'
#' @export
#'
#' @details This function summarizes minimum, mean, and maximum values calculated on a seasonal basis to allow for easier intra-season comparisons over time.
#'
#' \code{lm_trend = TRUE} adds a linear regression to the plot, and \code{lm_lab = TRUE} will add p-values from the linear regression to the plot. If the p-values are significant (p < 0.05) then the text will appear in bold. \code{lm_lab} text is color coded to match with the corresponding dots.
#'
#' @author Julie Padilla
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}, \code{\link{y_labeler}}
#'
#' @examples
#' dat_wq <- elksmwq
#' #dat_wq <- subset(dat_wq, subset = c('2010-01-01 0:00', '2017-01-01 0:00'))
#' dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
#'
#' x <-
#'   seasonal_dot(dat_wq, param = 'do_mgl'
#'                , lm_trend = TRUE
#'                , lm_lab = TRUE
#'                , plot_title = TRUE)
#'
#' \donttest{
#' x <-
#'   seasonal_dot(dat_wq, param = 'do_mgl'
#'                , lm_trend = FALSE
#'                , lm_lab = FALSE
#'                , plot_title = TRUE)
#'
#' x <-
#'   seasonal_dot(dat_wq, param = 'do_mgl'
#'                , lm_trend = TRUE
#'                , lm_lab = FALSE
#'                , plot_title = TRUE)
#'
#'
#' dat_nut <- elknmnut
#' dat_nut <- subset(dat_nut, subset = c('2007-01-01 0:00', '2017-01-01 0:00'))
#' dat_nut <- qaqc(dat_nut, qaqc_keep = c(0, 3, 5))
#'
#' x <-
#'   seasonal_dot(dat_nut
#'                , param = 'chla_n'
#'                , season_grps = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12))
#'                , season_names = c('Winter', 'Spring', 'Summer', 'Fall')
#'                , season_start = 'Spring'
#'                , lm_trend = FALSE
#'                , lm_lab = FALSE
#'                , plot_title = TRUE)
#'
#' x <-
#'   seasonal_dot(dat_nut, param = 'chla_n'
#'                , lm_trend = TRUE
#'                , lm_lab = FALSE
#'                , plot_title = TRUE)
#'
#' x <-
#'   seasonal_dot(dat_nut, param = 'chla_n'
#'                , lm_trend = TRUE
#'                , lm_lab = TRUE
#'                , plot_title = TRUE)
#' }

seasonal_dot <- function(swmpr_in, ...) UseMethod('seasonal_dot')

#' @rdname seasonal_dot
#'
#' @export
#'
#' @method seasonal_dot swmpr
#'
seasonal_dot.swmpr <- function(swmpr_in
                               , param = NULL
                               , lm_trend = FALSE
                               , lm_lab = FALSE
                               , free_y = FALSE
                               , log_trans = FALSE
							                 , converted = FALSE
                               , plot_title = FALSE
                               , plot = TRUE
                               , ...) {

  # #------------FOR DEBUGGING--------------------------------------------------
  # library(dplyr)
  # dat_wq <- elksmwq
  # dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
  # swmpr_in <- dat_wq
  # param = 'do_mgl'
  # lm_trend = TRUE
  # lm_lab = TRUE
  # plot_title = TRUE
  # free_y = FALSE
  # log_trans = FALSE
  # converted = FALSE
  # plot_title = FALSE
  # plot = TRUE

  # #--------------END DEBUGGING------------------------------------------------

  dat <- swmpr_in
  parm <- sym(param)
  conv <- converted

  seas <- sym('season')
  yr <- sym('year')

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')
  data_type <- substr(station, 6, nchar(station))

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
  # DEBUG dat$season <- assign_season(dat$datetimestamp, abb = TRUE)
  dat$season <- assign_season(dat$datetimestamp, abb = TRUE, ...)

  # Assign date for determining daily stat value
  dat$year <- lubridate::year(dat$datetimestamp)

  # Filter for parameter of interest and remove missing values (in case there is a month with no data)
  dat <- dat[, c('year', 'season', param)]
  dat <- dat %>% dplyr::filter(!is.na(!! parm))

  # --------
  # calc seasonal values
  plt_data <- dat %>%
    group_by(!! yr, !! seas) %>%
    summarise(min = min(!! parm, na.rm = TRUE)
              , mean = mean(!! parm, na.rm = TRUE)
              , max = max(!! parm, na.rm = TRUE)
              )

  # ensure all factor levels are accounted for, even if there is no data
  plt_data <- tidyr::complete(plt_data, !! seas)

  # remove NaN, -Inf, Inf values
  # DLE 4/24/2020: Kludge due to tibble change: call individually
  plt_data[, 3] <- remove_inf_and_nan(plt_data[, 3])
  plt_data[, 4] <- remove_inf_and_nan(plt_data[, 4])
  plt_data[, 5] <- remove_inf_and_nan(plt_data[, 5])

  if(plot) {
    agg_lab <- ifelse(length(levels(plt_data$season)) == 12, 'Monthly ', 'Seasonal ')

    labs_legend <- factor(paste0(agg_lab, c('Minimum', 'Average', 'Maximum'), sep = ''))
    brks <- range(plt_data$year)

    mx <- max(plt_data[ , c(3:5)], na.rm = TRUE) * 1.2
    mx <- ifelse(data_type == 'nut' && param != 'chla_n', ceiling(mx/0.01) * 0.01, ceiling(mx))

    # assign a minimum of zero unles there are values < 0
    mn <- min(plt_data[ , c(3:5)], na.rm = TRUE)
    mn <- ifelse(mn < 0 , min(pretty(mn)), 0)
    mn <- ifelse(log_trans, ifelse(substr(station, 6, nchar(station)) == 'nut', 0.001, 0.1), mn)

    plt <-
      ggplot(data = plt_data, aes_string(x = "year", y = "min", color = labs_legend[1])) +
      geom_point() +
      geom_point(data = plt_data, aes_string(x = "year", y = "mean", color = labs_legend[2])) +
      geom_point(data = plt_data, aes_string(x = "year", y = "max", color = labs_legend[3])) +
      geom_point() +
      scale_color_manual('', values = c('black', 'red', 'blue')) +
      scale_x_continuous(breaks = seq(from = brks[1], to = brks[2], by = 1)) +
      facet_wrap(~ season) +
      labs(x = NULL, y = eval(y_label))

    # add a log transformed access if log_trans == TRUE
    ## allow y-axis to be free if free_y == TRUE
    if(!log_trans) {

      plt <- plt +
        scale_y_continuous(labels = scales::format_format(digits = 2, big.mark = ",", decimal.mark = ".", scientific = FALSE)
                           , breaks = scales::breaks_pretty(n = 8))

      if(!free_y){plt <- plt + expand_limits(y = mn)}

    } else {
      plt <- plt +
        scale_y_continuous(trans = y_trans
                                , labels = scales::format_format(digits = 2, big.mark = ",", decimal.mark = ".", scientific = FALSE)
                                , breaks = scales::breaks_pretty(n = 8))

      if(!free_y) {plt <- plt + expand_limits(y = mn)}
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
      theme(legend.text = element_text(size = 10)) +
      theme(legend.spacing.x = unit(3, 'pt'))

    # add regression line if specified
    if(lm_trend) {
      plt <-
        plt +
        geom_smooth(method = 'lm', se = FALSE, lwd = 0.5) +
        geom_smooth(aes_string(x = 'year', y = 'mean', color = labs_legend[2])
                    , method = 'lm', se = FALSE, lwd = 0.5) +
        geom_smooth(aes_string(x = 'year', y = 'max', color = labs_legend[3])
                    , method = 'lm', se = FALSE, lwd = 0.5)
    }

    # add regression p-values if specified
    if(lm_lab) {

      p_labs <- lm_p_labs(plt_data)

      if(nrow(p_labs) > 0) {
        # return max & min y-value from ggplot object
        y_mx <- max(ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range)
        y_mn <- min(ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range)
        y_rng <- y_mx - y_mn

        # Add plot coordinates to label dataframe for geom_text()
        p_labs$x  <-  brks[2]
        p_labs$max_y <- y_mx - (0.01 * y_rng)
        p_labs$mean_y <- y_mx - (0.135 * y_rng)
        p_labs$min_y <- y_mx - (0.255 * y_rng)

        # Annotate with geom_text() instead of annotate()
        plt <-
          plt +
          geom_text(aes(label = .data$max, x = .data$x, y = .data$max_y)
                    , data = p_labs
                    , fontface = ifelse(p_labs$max == 'p < 0.05', 2, 1)
                    , hjust = 1, color = 'red') +
          geom_text(aes(label = .data$mean, x = .data$x, y = .data$mean_y)
                    , data = p_labs
                    , fontface = ifelse(p_labs$mean == 'p < 0.05', 2, 1)
                    , hjust = 1, color = 'black') +
          geom_text(aes(label = .data$min, x = .data$x, y = .data$min_y)
                    , data = p_labs
                    , fontface = ifelse(p_labs$min == 'p < 0.05', 2, 1)
                    , hjust = 1, color = 'blue')
      # plt <-
      #   plt +
      #     annotate("text", x = brks[2], y = y_mx
      #              , label = p_labs$max, fontface = ifelse(p_labs$max == 'p < 0.05', 2, 1)
      #              , hjust = 1, color = 'red') +
      #     annotate("text", x = brks[2], y = y_mx * 0.9
      #              , label = p_labs$mean, fontface = ifelse(p_labs$mean == 'p < 0.05', 2, 1)
      #              , hjust = 1, color = 'black') +
      #     annotate("text", x = brks[2], y = y_mx * 0.8
      #              , label = p_labs$min, fontface = ifelse(p_labs$min == 'p < 0.05', 2, 1)
      #              , hjust = 1, color = 'blue')
      } else {
        warning('Insufficient data to calculate linear regression p-values')
      }
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
