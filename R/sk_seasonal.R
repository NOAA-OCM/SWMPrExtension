#' Seasonal Kendall Analysis for Seasonal Data
#'
#' Non-parametric test for monotonic seasonal trends
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param alpha num, alpha value to use to significance test. Defaults to 0.05.
#' @param data_min num, the minimum number of observations required to perform the analysis. Defaults to 5
#' @param envStats_summary logical, should the standard \code{EnvStats::kendallSeasonalTrendTest} be returned? Defaults to \code{FALSE}. See Details for more information.
#' @param stat_lab chr, label for the summary statistic defined in \code{FUN}. Defaults to "Average".
#' @param FUN function used to aggregate seasonal SWMP data.
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
#' Data are aggregated on a user-specified seasonal basis using the \code{FUN} argument. For example, using default settings, \code{sk_seasonal} would perform a seasonal kendall test on average monthly values. However, if the user set \code{FUN = min(x, na.rm = TRUE)} then a seasonal kendall would be performed on monthly minimum values.
#'
#'
#' If \code{EnvStats_summary = TRUE} then the detailed output summary from \code{\link[EnvStats]{kendallSeasonalTrendTest}} will be returned. If \code{EnvStats_summary = FALSE} then an abbreviated summary will be returned in a \code{data.frame}. The abbreviated summary contains the station name, the type of statistic used to summarize the data on a seasonal basis (specified by \code{stat_lab}), and the following results from \code{\link[EnvStats]{kendallSeasonalTrendTest}}: tau, slope, p-value for the chi-square test, and the p-value for the trend test.
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return Returns a \code{data.frame} object or a summary from \code{EnvStats::kendallSeasonalTrendTest}
#'
#' @seealso \code{\link{assign_season}}, \code{\link{y_labeler}}, \code{\link[EnvStats]{kendallSeasonalTrendTest}}
#'
#' @examples
#' dat_wq <- elksmwq
#' dat_wq <- qaqc(dat_wq, qaqc_keep = c(0, 3, 5))
#'
#' x <- sk_seasonal(dat_wq, param = 'temp')
#'

sk_seasonal <- function(swmpr_in, ...) UseMethod('sk_seasonal')

#' @rdname sk_seasonal
#'
#' @export
#'
#' @method sk_seasonal swmpr
#'
#'
sk_seasonal.swmpr <- function(swmpr_in
                             , param = NULL
                             , alpha = 0.05
                             , data_min = 5
                             , envStats_summary = FALSE
                             , stat_lab = 'Average'
                             , FUN = function(x) mean(x, na.rm = TRUE)
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
  dat$season <- assign_season(dat$datetimestamp, abb = TRUE, ...)

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

  data_check <- sk_data %>% group_by(!! seas) %>% summarise(count = n())

  # return(data_check)

  if(min(data_check$count < data_min)) {
    warning(paste('Fewer than', data_min, 'data points available for at least one season. Seasonal kendall will not be performed.'))


    # Create a dummy table with "insuff" as the pval
    sk_tbl <- data.frame(matrix(vector(), 0, 9))
    sk_tbl[1, ] <- c(station, stat_lab, param, rep(NA, 4), 'insuff', 'insuff')
    names(sk_tbl) <- c('station', 'type', 'parameter', 'tau', 'slope', 'pval.chisq', 'pval.trend', 'sig.chi', 'sig.trend')

  } else {
    ### these could be put into an lapply and then combined after -----
    # perform seasonal kendall
    sk_result <- kendallSeasonalTrendTest(result ~ season + year, data = sk_data)

    # extract results and return
    sk_tbl <- sk_tidy(sk_result, station = station, param = param, stat = stat_lab)
  }

  if(envStats_summary) {
    return(sk_result)
  } else {
    return(sk_tbl)
  }

}
