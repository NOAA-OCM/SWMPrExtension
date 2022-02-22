#' Cumulative Bar Plot
#'
#' Cumulative bar plot over a historic range
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param hist_rng numeric vector, if historic range is not specified then the min/max values of the data set will be used.
#' @param log_trans logical, should y-axis be log? Defaults to \code{FALSE}
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param hist_avg logical, should a historical average be included? Defaults to \code{TRUE}.
#' @param bar_position chr string, options available are \code{stack} or \code{dodge}. Defaults to \code{stack}
#' @param season_facet logical, should plot be faceted by season? Defaults to \code{FALSE}.
#' @param plot_title logical, should the station name be included as the plot title? Defaults to \code{FALSE}
#' @param plot logical, should a plot be returned? Defaults to \code{TRUE}
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter group_by summarise
#' @importFrom grDevices colorRampPalette
#' @importFrom magrittr "%>%"
#' @importFrom lubridate  year
#' @importFrom RColorBrewer brewer.pal
#' @importFrom rlang .data
#' @importFrom scales comma
#'
#' @export
#'
#' @details This function uses barplots to summarize parameters that are best viewed on a cumulative basis (e.g., precipitation). Data are aggregated on a seasonal and annual basis.
#'
#' There are two ways to make interannual comparisons: on an aggregate basis and on a seasonal basis. If the argument \code{season_facet = FALSE} then parameter totals from each season will be added together to compose one, multi-color bar.If \code{season_facet = TRUE} then parameter totals from each season separated into multiple plots for easier intra-season comparison across years.
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
#' data(apaebmet)
#' dat <- qaqc(apaebmet, qaqc_keep = c('0', '3', '5'))
#'
#' x <- seasonal_barplot(dat, param = 'totprcp'
#'                       , season_grps = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12))
#'                       , season_names = c('Winter', 'Spring', 'Summer', 'Fall')
#'                       , hist_avg = TRUE
#'                       , converted = FALSE)
#'
#' \donttest{
#' # return a table instead of a figure
#' y <- seasonal_barplot(dat, param = 'totprcp'
#'                       , season_grps = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12))
#'                       , season_names = c('Winter', 'Spring', 'Summer', 'Fall')
#'                       , converted = FALSE
#'                       , plot = FALSE)
#'
#' ## divide plot into seasonal facets
#' z <- seasonal_barplot(dat, param = 'totprcp'
#'                       , season_grps = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12))
#'                       , season_names = c('Winter', 'Spring', 'Summer', 'Fall')
#'                       , season_facet = TRUE
#'                       , hist_avg = TRUE
#'                       , converted = FALSE)
#'
#' ## convert from mm to in
#' dat$totprcp <- dat$totprcp / 25.4
#'
#' x1 <- seasonal_barplot(dat, param = 'totprcp'
#'                       , season_grps = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12))
#'                       , season_names = c('Winter', 'Spring', 'Summer', 'Fall')
#'                       , hist_avg = TRUE
#'                       , converted = TRUE)
#' }
#"
seasonal_barplot <- function(swmpr_in, ...) UseMethod('seasonal_barplot')

#' @rdname seasonal_barplot
#'
#' @export
#'
#' @method seasonal_barplot swmpr
#'
seasonal_barplot.swmpr <- function(swmpr_in
                               , param = NULL
                               , hist_rng = NULL
                               , log_trans = FALSE
                               , converted = FALSE
                               , hist_avg = TRUE
                               , bar_position = 'stack'
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

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  #determine type WQ, MET, NUT
  #determine log scale transformation
  if(substr(station, 6, nchar(station)) != 'met')
    stop('Currently, function only works with MET data.')

  #determine historical range exists and that it is reasonable, if not default to min/max of the range
  if(is.null(rng)) {
    warning('No historical range specified. Entire time series will be used.')
    rng <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
  } else {
    if(min(rng) < min(lubridate::year(dat$datetimestamp)) | max(rng) > max(lubridate::year(dat$datetimestamp))) {
      warning('Specified range is greater than the range of the dataset. Max/min  range of the dataset will be used.')
      rng <- c(min(lubridate::year(dat$datetimestamp)), max(lubridate::year(dat$datetimestamp)))
    }
  }

  #determine that variable name exists ----
  if(!(param %in% c('totprcp', 'totpar')))
    stop('Param argument must be precipitation (totprcp) or PAR (totpar)')

  #determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

  #determine if QAQC has been conducted
  if(attr(dat, 'qaqc_cols'))
    stop('QAQC columns present. QAQC must be performed before analysis.')

  ## Clip data to historic range ----
  dat_hist <- dat %>% dplyr::filter(lubridate::year(.data$datetimestamp) >= rng[[1]]
                                    & lubridate::year(.data$datetimestamp) <= rng[[2]])

  dat_hist$year <- factor(lubridate::year(dat_hist$datetimestamp))

  # Assign the seasons and order them ----
  dat_hist$season <- assign_season(dat_hist$datetimestamp, ...)

  # assign colors to a color ramp (may need interpolation)
  cols <- colorRampPalette(RColorBrewer::brewer.pal(9, 'Blues'))
  cols <- cols(length(unique(dat_hist$season)) + 1) # add one in order to skip over the lightest color in the palette
  cols <- cols[2:length(cols)]

  dat_hist <- dat_hist %>%
    dplyr::group_by(!! yr, !! seas) %>%
    # dplyr::filter(!all(is.na(!! parm))) %>%
    # dplyr::summarise(result = sum(!! parm, na.rm = TRUE), .groups = "drop_last")
    #dplyr::summarise(result = sum(!! parm, na.rm = TRUE), .groups = "drop_last") %>%
    dplyr::mutate(na_flag = ifelse(all(is.na(!! parm)), NA, 1)) %>%
    dplyr::summarise(result = .data$na_flag * sum(!! parm, na.rm = TRUE), .groups = "drop") %>%
    unique()

  # Plotting section start ----
  if(plot){
    seas_col <- cols
    x_range <- range(as.numeric(as.character(dat_hist$year)))
    tick_interval <- case_when(
      diff(x_range) > 20  ~ 4,
      diff(x_range) > 10  ~ 2,
      TRUE            ~ 1)
    # x_range <- hist_rng
    # x_brks <- set_date_breaks(x_range)
    # x_minor_brks <- set_date_breaks_minor(rng)
    # x_lab_brks <- set_date_break_labs(rng)

    if(season_facet) {
      yr_mx <- dat_hist %>% group_by(!! yr, !! seas) %>%
        mutate(na_flag = ifelse(all(is.na(!! res)), NA, 1)) %>%
        summarise(max_val = .data$na_flag* sum(!! res, na.rm = TRUE), .groups = "drop_last")
    } else {
      yr_mx <- dat_hist %>% group_by(!! yr) %>%
        summarise(max_val = sum(!! res, na.rm = TRUE), .groups = "drop")
    }

    mx <- ceiling(max(yr_mx$max_val * 1.1, na.rm = TRUE) / 10) * 10
    brk_pts <- ifelse(mx < 50, 5, ifelse(mx < 100, 10, ifelse(mx < 1000, 100, ifelse(mx < 1000000, 200, 1000000))))

    # return(mx)
    # Add data to plot -----
    bar_seas <- ggplot(data = dat_hist, aes_(x = yr, y = res, fill = seas)) +
      geom_bar(stat = "identity", position = bar_position) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, mx), breaks = seq(0 , mx, brk_pts)) +
      scale_x_discrete(breaks = seq(from = x_range[1], to = x_range[2],
                                      by = tick_interval)) +
      # scale_x_datetime(date_breaks = x_brks, date_labels = x_lab_brks,
      #                  date_minor_breaks = x_minor_brks) +
      scale_fill_manual(values = seas_col) +
      labs(x = NULL, y = eval(y_label))

    # Add themes ----
    bar_seas <- bar_seas +
      theme_bw() +
      guides(fill = guide_legend(override.aes = list(linetype = 'blank'), order = 1)) +
      theme(panel.grid.minor.y = element_blank()
            , panel.grid.major.y = element_line(linetype = 'dashed')) +
      theme(panel.grid.major.x = element_blank()) +
      theme(legend.position = 'top', legend.title = element_blank()) +
      theme(axis.title.x = element_text(margin = unit(c(8, 0, 0, 0), 'pt'))
            , axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90))

    # Formatting text ----
    ## conditional based on parameter
    sz <- ifelse(param == 'totpar', 12, 16)
    bar_seas <- bar_seas +
      theme(text = element_text(size = sz))

    bar_seas <- bar_seas +
      theme(legend.key.height = unit(0.1, 'cm')
            , legend.key.width = unit(0.5, 'cm')) +
      theme(legend.text = element_text(size = 10)
            , legend.text.align = 0.5) +
      theme(legend.spacing.x = unit(3, 'pt'))

    # add plot title if specified ----
    if(plot_title) {
      ttl <- title_labeler(nerr_site_id = station)

      bar_seas <-
        bar_seas +
        ggtitle(ttl) +
        theme(plot.title = element_text(hjust = 0.5))
    }

    # historical range average if specified ----
    if(hist_avg) {
      var_nm <- ifelse(season_facet, 'Seasonal Average', 'Average')

      lab_parm <- paste(var_nm, ' (', rng[[1]], '-', rng[[2]], ')', sep = '')

      if(season_facet) {
        seas_means <- dat_hist %>%
          group_by(.data$season) %>%
          summarise(mean = mean(.data$result, na.rm = TRUE), .groups = "drop")

        dat_hist <- merge(dat_hist, seas_means)

        # return(dat_hist)
        bar_seas <- bar_seas +
          geom_hline(aes(yintercept = dat_hist$mean, linetype = factor(lab_parm))
                     , color = '#767171', lwd = 1.0, show.legend = TRUE) +
          scale_linetype_manual(values = 'solid')

      } else {

        avg <- dat_hist %>% group_by(year) %>%
          summarise(sum = sum(.data$result, na.rm = TRUE)) %>%
          summarise(avg = mean(sum, na.rm = TRUE))

        bar_seas <- bar_seas +
          geom_hline(aes(yintercept = avg[[1]], linetype = factor(lab_parm))#mean(dat_hist$result), linetype = factor(lab_parm))
                     , color = '#767171', lwd = 1.5, show.legend = TRUE) +
          scale_linetype_manual(values = 'solid')
      }

    }


    # facet wrap if specified ----
    if(season_facet) {

      # return(dat_hist)

      bar_seas <-
        bar_seas +
        facet_wrap(seas, ncol = 1)

    }


    return(bar_seas)

  } else {

    tbl <- dat_hist
    tbl$station <- attr(dat_hist, 'station')

    return(tbl)
  }
}
