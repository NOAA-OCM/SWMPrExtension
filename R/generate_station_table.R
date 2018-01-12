#' Filter Reformatted Seasonal Kendall Results
#'
#' Filters a \code{dataframe} of user-specified results for display in the NERRS reserve level report
#'
#' @param sk_result a \code{data.frame} of reformatted seasonal kendall results from \code{\link{sk_seasonal}}.
#' @param stations
#'
#' @import magrittr "%>%"
#'
#' @export
#'
#' @details Used internally by \code{\link{create_flextable_list}} to create a \code{data.frame} of user specified parameters to be displayed
#'
#' @author Julie Padilla
#'
#' @concept Reporting
#'
#' @return Returns a \code{data.frame} of user-specified results to be displayed
#'

generate_station_table <- function(sk_result, stations) {
  get('sampling_stations')

  sk_res_nms <- sk_result %>% filter(station %in% stations) %>% select(station)

  sk_res_nms <- data.frame(loc_id = toupper(substr(sk_res_nms$station, 4, 5))
                           , loc_name = sampling_stations$Station.Name[sampling_stations$Station.Code %in% sk_res_nms$station]
                           , stringsAsFactors = F)

  return(sk_res_nms)
}
