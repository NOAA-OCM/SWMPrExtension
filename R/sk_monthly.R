#' Seasonal Kendall Analysis for Monthly data
#'
#' Seasonal trends
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param stat_lab chr, label for the summary statistic defined in \code{FUN}. Defaults to "Average"
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @concept analyze
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @importFrom EnvStats kendallSeasonalTrendTest
#' @importFrom lubridate  year floor_date
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
#' @importFrom stats median
#'
#' @export
#'
#' @details Seasonal Kendall
#'
#' @author Julie Padilla
#'
#' @return A \code{data.frame} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[EnvStats]{kendallSeasonalTrendTest}}
#'
#' @examples
#' \dontrun{
#'
#' dat_wq <- elksmwq
#' dat_wq <- subset(dat_wq, subset = c('2007-01-01 0:00', '2017-01-01 0:00'))
#' dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
#' }

sk_monthly <- function(swmpr_in, ...) UseMethod('sk_monthly')

#' @rdname sk_monthly
#'
#' @concept analyze
#'
#' @export
#'
#' @method sk_monthly swmpr
#'
#'
sk_monthly.swmpr <- function(swmpr_in
                             , param = NULL
                             , stat_lab = 'Average'
                             , FUN = function(x) mean(x, na.rm = T)) {
  dat <- swmpr_in
  parm <- sym(param)
  seas <- sym('season')
  yr <- sym('year')

  # attributes
  parameters <- attr(dat, 'parameters')
  station <- attr(dat, 'station')

  #TESTS
  # determine type WQ, MET, NUT
  # determine log scale transformation
  if(substr(station, 6, nchar(station)) == 'nut')
    warning('Nutrient data detected. Consider specifying seasons > 1 month.')

  # determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  # determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param)

  # determine if QAQC has been conducted
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
  sk_data <- dat %>%
    group_by(!! yr, !! seas) %>%
    summarise(result = FUN(!! parm))

  ### these could be put into an lapply and then combined after -----
  # perform seasonal kendall
  sk_result <- kendallSeasonalTrendTest

  # extract results and return
  sk_tbl <- sk_tidy(sk_result, stat = stat_lab)
  ### ---------------------------------------------------------------
  # Could also add logic that would directly return the original sk object?

  return(sk_tbl)
}
