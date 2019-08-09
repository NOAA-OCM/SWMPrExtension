#' P-Value labels for Plotting
#'
#' Generate a dataframe of p-value labels based on p-values from linear regression
#'
#' @param dat_in \code{data.frame} with year, season, min, mean, max columns
#'
#' @import dplyr
#'
#' @importFrom broom tidy
#' @importFrom dplyr do filter group_by
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom stats lm
#' @importFrom tidyr complete
#'
#' @export
#'
#' @details A helper function that returns a \code{data.frame} of p-value labels for use with the \code{\link{seasonal_dot}}. P-values are taken from linear regression \code{lm}.
#'
#' @author Julie Padilla
#'
#' @concept miscellaneous
#'
#' @return Returns \code{data.frame} for use with \code{\link{seasonal_dot}}
#'
#' @seealso \code{\link[stats]{lm}}
#'
lm_p_labs <- function(dat_in) {

  # remove seasons with out results
  dat_in <- dat_in[complete.cases(dat_in), ]

  sample_check <- dat_in %>% group_by(.data$season) %>% summarise(count = n())
  sample_check <- sample_check[, 2]

  if(max(sample_check, na.rm = TRUE) > 1) {
  # if(dat_in %>% group_by(.data$season) %>% summarise(count = n()) %>% .data[, 2] %>% max(.data) > 1) {
    lm_results <- dat_in %>%
      group_by(.data$season) %>%
      do(reg_min = lm(.data$min ~ .data$year, data = .data)
         , reg_mean = lm(.data$mean ~ .data$year, data = .data)
         , reg_max = lm(.data$max ~ .data$year), data = .data)

    lm_min_tidy <- tidy(lm_results, reg_min) %>%
      filter(.data$term == '.data$year')
    lm_mean_tidy <- tidy(lm_results, reg_mean) %>%
      filter(.data$term == '.data$year')
    lm_max_tidy <- tidy(lm_results, reg_max) %>%
      filter(.data$term == '.data$year')

    lm_min_tidy$lab <- ifelse(lm_min_tidy$p.value < 0.05, 'p < 0.05', 'p > 0.05')
    lm_mean_tidy$lab <- ifelse(lm_mean_tidy$p.value < 0.05, 'p < 0.05', 'p > 0.05')
    lm_max_tidy$lab <- ifelse(lm_max_tidy$p.value < 0.05, 'p < 0.05', 'p > 0.05')

    df_lab <- data.frame(season = lm_min_tidy$season, min = lm_min_tidy$lab, mean = lm_mean_tidy$lab, max = lm_max_tidy$lab, stringsAsFactors = FALSE)

    # reinsert missing levels
    df_lab <- tidyr::complete(df_lab, season)

    # replace NA values with blank text
    df_lab[is.na(df_lab)] <- ''
  } else {
    # Create a dummy table with zero rows
    df_lab <- data.frame(matrix(vector(), 0, 4))
    names(df_lab) <- c('season', 'min', 'mean', 'max')
  }
  return(df_lab)
}

