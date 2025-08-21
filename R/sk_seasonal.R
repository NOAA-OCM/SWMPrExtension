#' Seasonal Kendall Tau Trend Test
#'
#' @description
#' Perform a Seasonal Kendall test for trend on SWMP data. This is a modification
#' of the `sk_tidy` function from the `SWMPr` package.
#'
#' @param swmpr_in input swmpr object
#' @param param chr string of variable to plot
#' @param date_var chr string of name of date variable. Default is `datetimestamp`.
#' @param season_grps list of months in each season. Defaults to
#'   `list(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12))`
#' @param season_names chr vector of season names. Defaults to
#'   `c("Winter", "Spring", "Summer", "Fall")`
#' @param min_season_data numeric minimum number of seasons with data required
#'   for the trend test.
#' @param min_year_data numeric minimum number of years with data required for
#'   the trend test.
#'
#' @import SWMPr
#'
#' @export
#'
#' @details A common issue with SWMP data is that there can be gaps in the
#' data record. The `rkt` package, which is the basis for `SWMPr::sk_tidy`,
#' can be sensitive to gaps in the data. `sk_seasonal` provides a wrapper for
#' `rkt::rkt` that is more robust to these gaps.
#'
#' The function first aggregates the data by season and year. It then checks if
#' there are enough seasons and years with data to perform the trend test. If
_# there are, it will perform the test and return the results. Otherwise, it
#' will return a data frame with a message indicating that the test could not
#' be performed.
#'
#' This function is a modification of `SWMPr::sk_tidy` and is intended to be
#' used in a similar manner.
#'
#' @author Kimberly Cressman, Marcus W. Beck
#'
#' @return Returns a data frame of station, p-value, the Sen slope, the median
#' seasonal value, and the number of years with data.
#'
#' @seealso `SWMPr::sk_tidy`
#'
#' @examples
#' \dontshow{
#' data(elksmwq)
#'
#' dat <- qaqc(elksmwq, qaqc_keep = c(0, 2, 3, 4, 5))
#'
#' #
#' sk_seasonal(dat, param = 'do_mgl')
#' }
#' \donttest{
#' data(elksmwq)
#'
#' dat <- qaqc(elksmwq, qaqc_keep = c(0, 2, 3, 4, 5))
#'
#' sk_seasonal(dat, param = 'do_mgl',
#'  season_grps = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12)),
#'  season_names = c('Winter', 'Spring', 'Summer', 'Fall'))
#' }

sk_seasonal <- function(swmpr_in, param,
                        date_var = 'datetimestamp',
                        season_grps = list(c(1,2,3), c(4,5,6),
                                           c(7,8,9), c(10,11,12)),
                        season_names = c("Winter", "Spring", "Summer", "Fall"),
                        min_season_data = 3,
                        min_year_data = 5) {
  
  # Helper function to prepare data
  prepare_data <- function(swmpr_in, param, date_var, season_grps, season_names) {
    # Add year and month columns, and assign seasons
    swmpr_in %>%
      dplyr::mutate(
        year = lubridate::year(.data[[date_var]]),
        month = lubridate::month(.data[[date_var]]),
        season = assign_season(month, season_grps, season_names)
      ) %>%
      dplyr::select(station, year, season, !!rlang::sym(param))
  }
  
  # Helper function to check data adequacy
  check_data_adequacy <- function(data, param, min_season_data, min_year_data) {
    data_summary <- data %>%
      dplyr::group_by(station, year, season) %>%
      dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>%
      dplyr::filter(n > 0)
    
    season_counts <- data_summary %>%
      dplyr::group_by(station, season) %>%
      dplyr::summarise(n_yrs = dplyr::n(), .groups = 'drop')
    
    year_counts <- data_summary %>%
      dplyr::group_by(station, year) %>%
      dplyr::summarise(n_seasons = dplyr::n(), .groups = 'drop')
    
    adequate_seasons <- all(season_counts$n_yrs >= min_season_data)
    adequate_years <- nrow(year_counts) >= min_year_data
    
    list(adequate_seasons = adequate_seasons, adequate_years = adequate_years)
  }
  
  # Helper function to perform the seasonal Kendall test
  perform_sk_test <- function(data, param) {
    station_name <- unique(data$station)
    tryCatch({
      # Formula for the rkt function
      formula <- stats::as.formula(paste(param, "~ year + season"))
      rkt_result <- rkt::rkt(data, formula)
      
      # Extract results
      data.frame(
        station = station_name,
        p_value = rkt_result$sl,
        sen_slope = rkt_result$B,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        station = station_name,
        p_value = NA,
        sen_slope = NA,
        stringsAsFactors = FALSE
      )
    })
  }
  
  # Main function logic
  data <- prepare_data(swmpr_in, param, date_var, season_grps, season_names)
  
  # Check for sufficient data
  data_check <- check_data_adequacy(data, param, min_season_data, min_year_data)
  if (!data_check$adequate_seasons || !data_check$adequate_years) {
    return(
      data.frame(
        station = unique(data$station),
        p_value = NA,
        sen_slope = NA,
        median_seasonal_value = NA,
        n_years = NA,
        message = "Insufficient data for trend analysis"
      )
    )
  }
  
  # Perform the test
  sk_results <- perform_sk_test(data, param)
  
  # Calculate median seasonal value and number of years
  summary_stats <- data %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(
      median_seasonal_value = stats::median(.data[[param]], na.rm = TRUE),
      n_years = dplyr::n_distinct(year),
      .groups = 'drop'
    )
  
  # Combine results
  dplyr::left_join(sk_results, summary_stats, by = "station")
}
