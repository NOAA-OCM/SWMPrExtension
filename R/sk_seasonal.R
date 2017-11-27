#' Seasonal Kendall Analysis for Seasonal Data
#'
#' Seasonal trends
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param converted logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.
#' @param stat_lab chr, label for the summary statistic defined in \code{FUN}. Defaults to "Average"
#' @param FUN function used to aggregate seasonal SWMP data
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @import ggplot2 dplyr scales rlang
#'
#' @importFrom EnvStats kendallSeasonalTrendTest
#' @importFrom lubridate  year floor_date
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
#'
#' @export
#'
#' @details Seasonal Kendall
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return A \code{data.frame} object
#'
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}, \code{\link{y_labeler}}, \code{\link[EnvStats]{kendallSeasonalTrendTest}}
#'
#' @examples
#' \dontrun{
#'
#' dat_wq <- elksmwq
#' dat_wq <- subset(dat_wq, subset = c('2007-01-01 0:00', '2017-01-01 0:00'))
#' dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
#' }

sk_seasonal <- function(swmpr_in, ...) UseMethod('sk_seasonal')

#' @rdname sk_seasonal
#'
#' @concept analyze
#'
#' @export
#'
#' @method sk_seasonal swmpr
#'
#'
sk_seasonal.swmpr <- function(swmpr_in
                             , param = NULL
                             , converted = FALSE
							 , stat_lab = 'Average'
							 , FUN = function(x) mean(x, na.rm = T)
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
  # determine type WQ, MET, NUT
  # determine log scale transformation
  if(substr(station, 6, nchar(station)) == 'nut')
    warning('Nutrient data detected. Consider specifying seasons > 1 month.')

  # determine that variable name exists
  if(!any(param %in% parameters))
    stop('Param argument must name input column')

  # determine y axis transformation and y axis label
  y_trans <- ifelse(log_trans, 'log10', 'identity')
  y_label <- y_labeler(param = param, converted = conv)

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
