#' P-Value labels for Plotting
#'
#' Generate a dataframe of p-value labels based on p-values from linear regression
#'
#' @param dat_in \code{data.frame} with year, season, min, mean, max columns
#'
#' @import dplyr
#'
#' @importFrom broom tidy
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom stats lm
#' @export
#'
#' @details This function returns a data.frame of p-value labels for use with the seasonal dot plot. P-values are taken from linear regression.
#'
#' @return Returns \code{data.frame} for use with \code{seasonal_dot}
#'
#' @seealso \code{lm}


lm_p_labs <- function(dat_in) {
  lm_results <- dat_in %>%
    group_by(season) %>%
    do(reg_min = lm(min ~ year, data = .)
       , reg_mean = lm(mean ~ year, data = .)
       , reg_max = lm(max ~ year, data = .))

  lm_min_tidy <- tidy(lm_results, reg_min) %>%
    filter(term == 'year')
  lm_mean_tidy <- tidy(lm_results, reg_mean) %>%
    filter(term == 'year')
  lm_max_tidy <- tidy(lm_results, reg_max) %>%
    filter(term == 'year')

  lm_min_tidy$lab <- ifelse(lm_min_tidy$p.value < 0.05, 'p < 0.05', 'p > 0.05')
  lm_mean_tidy$lab <- ifelse(lm_mean_tidy$p.value < 0.05, 'p < 0.05', 'p > 0.05')
  lm_max_tidy$lab <- ifelse(lm_max_tidy$p.value < 0.05, 'p < 0.05', 'p > 0.05')

  df_lab <- data.frame(min = lm_min_tidy$lab, mean = lm_mean_tidy$lab, max = lm_max_tidy$lab, stringsAsFactors = F)

  return(df_lab)
}

