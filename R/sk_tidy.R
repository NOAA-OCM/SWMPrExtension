#' Tidy Seasonal Kendall Results
#'
#' Tidy results from \code{\link[EnvStats]{kendallSeasonalTrendTest}}
#'
#' @param data a \code{htest} object produced by \code{\link[EnvStats]{kendallSeasonalTrendTest}}
#' @param station chr string sampling station
#' @param param chr string of variable to plot
#' @param stat chr, label to be used for statistic used to group data
#' @param alpha num, significance level. Defaults to 0.05
#'
#' @export
#'
#' @details A helper function used by \code{sk_seasonal} to return a table of tidied values.
#'
#' @author Julie Padilla
#'
#' @concept miscellaneous
#'
#' @return a \code{data.frame} of results from \code{\link[EnvStats]{kendallSeasonalTrendTest}}
#'
#' @examples
#' \dontrun{
#' data(elksmwq)
#' }
#'
sk_tidy <- function(data, station, param, stat, alpha = 0.05) {
  # have a check that verifies the data type as whatever kendallSeasonalTrend returns
  dat <- data
  parm <- param

  results <- data.frame(station, stat, parm
               , dat$estimate[[1]]
               , dat$estimate[[2]]
               , dat$p.value[[1]]
               , dat$p.value[[2]])

  names(results) <- c('station', 'type', 'parameter', 'tau', 'slope', 'pval.chisq', 'pval.trend')

  results$sig.chi <- NA
  results$sig.trend <- NA

  results$sig.chi <- ifelse(results[6] < alpha, 'sig', 'insig')

  results$sig.trend <- ifelse(results[7] > alpha, 'insig', ifelse(results[5] > 0, 'inc', 'dec'))

  return(results)
}
