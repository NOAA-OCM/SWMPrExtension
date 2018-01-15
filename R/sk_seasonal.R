#' Seasonal Kendall Analysis for Seasonal Data
#'
#' Non-parametric test for monotonic seasonal trends
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param alpha num, alpha value to use to significance test. Defaults to 0.05.
#' @param envStats_summary logical, should the standard \code{EnvStats::kendallSeasonalTrendTest} be returned? Defaults to \code{FALSE}. See Details for more information.
#' @param stat_lab chr, label for the summary statistic defined in \code{FUN}. Defaults to "Average"
#' @param FUN function used to aggregate seasonal SWMP data
#' @param ... additional arguments passed to other methods. See \code{\link{assign_season}}
#'
#' @importFrom dplyr filter group_by summarise
#' @importFrom EnvStats kendallSeasonalTrendTest
#' @importFrom lubridate  year floor_date
#' @importFrom magrittr "%>%"
#' @importFrom tidyr gather
#'
#' @export
#'
#' @details This function performs a seasonal kendall test on seasonally aggregated values using \code{\link[EnvStats]{kendallSeasonalTrendTest}}.
#'
#' If \code{EnvStats_summary = T} then the detailed output summary from \code{\link[EnvStats]{kendallSeasonalTrendTest}} will be returned. If \code{EnvStats_summary = F} then an abbreviated summary will be returned in a \code{data.frame}. The abbreviated summary contains the station name, the type of statistic used to summarize the data on a seasonal basis (specified by \code{stat_lab}), and the following results from \code{\link[EnvStats]{kendallSeasonalTrendTest}}: tau, slope, p-value for the chi-square test, and the p-value for the trend test.
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return A \code{data.frame} object or a summary from \code{EnvStats::kendallSeasonalTrendTest}
#'
#' @seealso \code{\link{assign_season}}, \code{\link{y_labeler}}, \code{\link[EnvStats]{kendallSeasonalTrendTest}}
#'
#' @examples
#' \dontrun{
#'
#' dat_wq <- elksmwq
#' dat_wq <- subset(dat_wq, subset = c('2007-01-01 0:00', '2017-01-01 0:00'))
#' dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
#'
#' x <- sk_seasonal(dat_wq, param = 'temp')
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
                             , alpha = 0.05
                             , envStats_summary = FALSE
                             , stat_lab = 'Average'
                             , FUN = function(x) mean(x, na.rm = T)
                             , ...) {
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
  sk_result <- kendallSeasonalTrendTest(result ~ season + year, data = sk_data)

  # extract results and return
  sk_tbl <- sk_tidy(sk_result, station = station, param = param, stat = stat_lab)

  if(envStats_summary) {
    return(sk_result)
  } else {
    return(sk_tbl)
  }

}
