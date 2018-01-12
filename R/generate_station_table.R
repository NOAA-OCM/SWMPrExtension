#' Filter Reformatted Seasonal Kendall Results
#'
#' Filters a \code{dataframe} of user-specified results for display in the NERRS reserve level report
#'
#' @param sk_result a \code{data.frame} of reformatted seasonal kendall results from \code{\link{sk_seasonal}}.
#' @param stations chr, vector of stations listed in \code{sk_result} that should be displayed in the NERRS reserve level report
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @export
#'
#' @details Used internally by \code{\link{create_sk_flextable_list}} to create a \code{data.frame} of user specified parameters to be displayed
#'
#' @author Julie Padilla
#'
#' @concept Reporting
#'
#' @return Returns a \code{data.frame} of user-specified results to be displayed
#'

generate_station_table <- function(sk_result, stations) {
  stns <- get('sampling_stations')

  sk_res_nms <- sk_result %>% filter(.data$station %in% stations) %>% select(.data$station)

  sk_res_nms <- data.frame(loc_id = toupper(substr(sk_res_nms$station, 4, 5))
                           , loc_name = stns$Station.Name[stns$Station.Code %in% sk_res_nms$station]
                           , stringsAsFactors = F)

  return(sk_res_nms)
}
