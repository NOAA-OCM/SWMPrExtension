#' Standard Parameter Check
#'
#' Determine if a parameter is one of the standard SWMP parameters
#'
#' @param param chr string of variable abbreviation
#'
#' @export
#'
#' @details A helper function used internally by several plotting functions to determine if parameter has a standard y-axis label. To accomodate the needs of the reserve-level annual report, this function also recognizes dissolved organic phosphorus (DIP) and dissolved inorganic nitrogen (DIN) as standard parameters.
#'
#' @author Julie Padilla
#'
#' @concept miscellaneous
#'
#' @return Returns \code{TRUE} or \code{FALSE}
#'
std_param_check <- function(param) {

  # Parameter abbreviations
  wq_params <- c('temp', 'spcond', 'sal', 'do_pct', 'do_mgl'
                 , 'depth', 'cdepth', 'level', 'clevel'
                 , 'ph', 'turb', 'chlfluor')
  met_params <- c('atemp', 'rh', 'bp', 'wspd', 'maxwspd'
                  , 'maxwspdt', 'wdir', 'swdir', 'totpar'
                  , 'totprcp', 'totsorad')
  nut_params <- c('po4f', 'nh4f', 'no2f', 'no3f', 'no23f', 'chla_n', 'din', 'dip')

  # Combine together for lookup
  all_params <- c(wq_params, met_params, nut_params)


  ifelse(param %in% all_params, TRUE, FALSE)
}

