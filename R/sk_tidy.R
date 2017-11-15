#' Tidy Seasonal Kendall Results
#'
#' Tidy results from \code{EnvStats::kendallSeasonalTrendTest}
#'
#' @param data a vector of POSIXct dates
#' @param stat chr, label to be used for statistic used to group data
#' @param pval num, significance level
#'
#' @examples
#' \dontrun{
#' data(elksmwq)
#'
#' }
#'
#' @export
#'
#' @details A helper function used by \code{sk_monthly} to return a table of tidied values.
#'
#' @return a \code{data.frame} of results from \code{kendallSeasonalTrendTest}
#'
#'
#'
sk_tidy <- function(data, param, stat, pval) {
  # have a check that verifies the data type as whatever kendallSeasonalTrend returns
  dat <- data

  results <- data.frame(stat, param
               , smk.max$estimate[[1]]
               , smk.max$estimate[[2]]
               , smk.max$p.value[[1]]
               , smk.max$p.value[[2]])

  names(results) <- c('type', 'parameter', 'tau', 'slope', 'pval.chisq', 'pval.trend')

  results$sig.chi <- ifelse(results$pval.chisq < pval, 'insig'
                            , ifelse(results$slope = 0, 'conflicting seasonal slopes'
                                     , ifelse(results$slope < 0, 'dec', 'inc')))

  results$sig.trend <- ifelse(results$pval.trend < pval, 'insig'
                              , ifelse(results$slope = 0, 'conflicting seasonal slopes'
                                       , ifelse(results$slope < 0, 'dec', 'inc')))

  return(results)
}
