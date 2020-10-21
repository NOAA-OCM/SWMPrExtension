#' Filter Reformatted Seasonal Kendall Results
#'
#' Filters a \code{dataframe} of user-specified results for display in the NERRS reserve level report
#'
#' @param sk_result a \code{data.frame} of reformatted seasonal kendall results from \code{\link{sk_seasonal}}.
#' @param stations chr, vector of stations listed in \code{sk_result} that should be displayed in the NERRS reserve level report
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @details Used internally by \code{\link{create_sk_flextable_list}} to create a \code{data.frame} of user specified parameters to be displayed
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a \code{data.frame} of user-specified results to be displayed
#'

generate_station_table <- function(sk_result, stations) {
  stns <- get('sampling_stations')

  sk_res_nms <- stns[stns$Station.Code %in% stations, c('Station.Code', 'Station.Name')]
  sk_res_nms <- sk_res_nms[order(sk_res_nms$Station.Code), ]

  sk_res_nms$Station.Code <- toupper(substr(sk_res_nms$Station.Code, 4, 5))

  names(sk_res_nms) <- c('loc_id', 'loc_name')

  return(sk_res_nms)
}
