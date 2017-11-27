#' Cumulative Bar Plot
#'
#' Cumulative bar plot over a historic range
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param rng_avg logical, should a longterm average be included on the plot? Defaults to \code{FALSE}
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param hist_avg logical, should a historical average be included? Defaults to \code{TRUE}.
#' @param season_facet logical, should plot be faceted by season? Defaults to \code{FALSE}.
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year
#'
#' @export
#'
#' @details This function produces an annual bar plot with seasonal components. This function is intended to be an analysis similar to \code{seasonal_boxplot}, but for parameters that are assessed on a cumulative basis (e.g., precipitation)
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
#' data(apaebmet)
#' dat <- qaqc(apaebmet, qaqc_keep = c('0', '3', '5'))
#'
#' x <- seasonal_barplot(dat, param = 'totprcp'
#'                       , season = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12))
#'                       , season_names = c('Winter', 'Spring', 'Summer', 'Fall')
#'                       , convert = T)
#'
#' y <- seasonal_barplot(dat, param = 'totprcp'
#'                       , season = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12))
#'                       , season_names = c('Winter', 'Spring', 'Summer', 'Fall')
#'                       , convert = T
#'                       , plot = F)
#'
#' }
#"
seasonal_barplot <- function(swmpr_in, ...) UseMethod('seasonal_barplot')

#' @rdname seasonal_barplot
#'
#' @concept analyze
#'
#' @export
#'
#' @method seasonal_barplot swmpr
#'
seasonal_barplot.swmpr <- function(swmpr_in
                               , param = NULL
                               , hist_rng = NULL
                               , rng_avg = FALSE
                               , log_trans = FALSE
                               , converted = FALSE
                               , hist_avg = TRUE
                               , season_facet = FALSE
                               , plot_title = FALSE
                               , plot = TRUE
                               , ...) {

  dat <- swmpr_in
  parm <- sym(param)
  conv <- converted

  seas <- sym('season')
  res <- sym('result')
  yr <- sym('year')
  avg <- sym('mean')

  rng <- hist_rng

  cols <- c('#1F4E79', '#4374A0', '#5B9BD5', '#97B9E0') %>% rev #will need to adjust color scheme

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  #determine type WQ, MET, NUT
  #determine log scale transformation
  if(substr(station, 6, nchar(station)) != 'met')
    stop('Currently, function only works with MET data.')

  #determine historical range exists, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No historical range specified. Minimum and maximum year in data set will be used.')
    rng <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
  }

  #determine that variable name exists
  if(!(param %in% c('totprcp', 'totpar')))
    stop('Param argument must be precipitation (totprcp) or PAR (totpar)')

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    stop('QAQC columns present. QAQC must be performed before analysis.')

  ##historic range
  dat_hist <- dat %>% dplyr::filter(lubridate::year(.data$datetimestamp) >= rng[[1]]
                                    & lubridate::year(.data$datetimestamp) <= rng[[2]])

  dat_hist$year <- factor(lubridate::year(dat_hist$datetimestamp))

  # Assign the seasons and order them
  dat_hist$season <- assign_season(dat_hist$datetimestamp, abb = T, ...)

  dat_hist <- dat_hist %>%
    dplyr::group_by(!! yr, !! seas) %>%
    dplyr::summarise(result = sum(!! parm, na.rm = T))

  if(plot){
    seas_col <- cols
    yr_mx <- dat_hist %>% group_by(year) %>% summarise(max_val = sum(.data$result, na.rm = T))
    mx <- ceiling(max(yr_mx$max_val) / 10) * 10

    # Add data
    bar_seas <- ggplot(data = dat_hist, aes_(x = yr, y = res, fill = seas)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(expand = c(0, 0), limits = c(0, mx), breaks = seq(0 , mx, 5)) +
      scale_fill_manual(values = seas_col) +
      labs(x = NULL, y = eval(y_label))

    # Add themes
    bar_seas <- bar_seas +
      theme_bw() +
      guides(fill = guide_legend(override.aes = list(linetype = 'blank'), order = 1)) +
      theme(panel.grid.minor.y = element_blank()
            , panel.grid.major.y = element_line(linetype = 'dashed')) +
      theme(panel.grid.major.x = element_blank()) +
      theme(legend.position = 'top', legend.title = element_blank()) +
      theme(axis.title.x = element_text(margin = unit(c(8, 0, 0, 0), 'pt'))
            , axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90))

    # Formatting text
    bar_seas <- bar_seas +
      theme(text = element_text(size = 16))

    bar_seas <- bar_seas +
      theme(legend.key.size = unit(7, 'pt')) +
      theme(legend.text = element_text(size = 8)) +
      theme(legend.spacing.x = unit(-5, 'pt'))

    # add plot title if specified
    if(plot_title) {
      ttl <- title_labeler(nerr_site_id = station)

      bar_seas <-
        bar_seas +
        ggtitle(ttl) +
        theme(plot.title = element_text(hjust = 0.5))
    }

    # facet wrap if specified
    if(season_facet) {
      bar_seas <-
        bar_seas +
        facet_wrap(~ season, ncol = 1)

      seas_means <- dat_hist %>%
        group_by(season) %>%
        summarise(mean = mean(result, na.rm = T))

      dat_hist <- merge(dat_hist, seas_means)
    }

    # return(seas_means)
    # historica range average if specified
    if(hist_avg) {
      var_nm <- ifelse(season_facet, 'Seasonal Average', 'Average')

      lab_parm <- paste(var_nm, ' (', rng[[1]], '-', rng[[2]], ')', sep = '')

      if(season_facet) {
        bar_seas <- bar_seas +
          geom_hline(aes(yintercept = dat_hist$mean, linetype = factor(lab_parm))
                     , color = '#D9D9D9', lwd = 1.5, show.legend = T) +
          scale_linetype_manual(values = 'solid')
      } else {
        bar_seas <- bar_seas +
          geom_hline(aes(yintercept = mean(dat_hist$result), linetype = factor(lab_parm))
                     , color = '#D9D9D9', lwd = 1.5, show.legend = T) +
          scale_linetype_manual(values = 'solid')
      }

    }



    return(bar_seas)

  } else {

    tbl <- dat_hist
    tbl$station <- attr(dat_hist, 'station')

    return(tbl)
  }
}
