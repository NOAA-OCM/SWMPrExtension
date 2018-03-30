#' Filter Reformatted Seasonal Kendall Results
#'
#' Filters a \code{dataframe} of user-specified results for display in the NERRS reserve level report
#'
#' @param sk_result a \code{data.frame} of reformatted seasonal kendall results from \code{\link{sk_seasonal}}.
#' @param stations chr, vector of station names included in \code{sk_result} that will be displayed in the NERRS reserve level report
#' @param param chr, vector of parameters included in \code{sk_result} that will be displayed in the NERRS reserve level report
#'
#' @importFrom dplyr filter select
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @export
#'
#' @details A helper function used internally by \code{\link{create_sk_flextable_list}} to create a \code{data.frame} of user specified parameters to be displayed in the reserve level report.
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a \code{data.frame} of user-specified results to be displayed
#'

generate_results_table <- function(sk_result, stations, param) {

  sk_result <- sk_result %>% filter(.data$station %in% stations)

  sk_result <- sk_result[order(sk_result$station), ]

  sk_result <- sk_result %>% select(param)

  return(sk_result)
}
