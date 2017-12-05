#' Generate y-axis Label Based on SWMP Parameter Abbreviation
#'
#' Generate a y-axis label based on SWMP parameter abbreviation and threshold criteria
#'
#' @param param chr string of variable abbreviation
#' @param parameter_threshold vector of numerical thresholds to evaluate parameters against
#' @param threshold_type vector of logical operators ('<', '>', '<=', '>=', '==', '!=')
#' @param time_threshold The amount of time an event must last to be counted (in hours)
#' @param converted logical, should the parameter label units be converted from metric to english? Defaults to \code{FALSE}. Currently available for \code{temp}, \code{depth}, \code{cdepth}, \code{level}, \code{clevel}, \code{atemp}, \code{wspd}, \code{maxwspd}, and \code{totprcp}
#'
#' @export
#'
#' @details A helper function used internally by several plotting functions to generate y-axis labels. This function does not convert sample results from metric to english. It only adjusts the units in the y-axis label.
#'
#' @author Julie Padilla
#'
#' @concept miscellaneous
#'
#' @return Returns character vector or an unevaluated expression
#'
#' @examples
#' \dontrun{
#' y_lab <- y_count_labeler(param = 'do_mgl', parameter_threshold = 2
#' , threshold_type = '<', time_threshold = 2, converted = F)
#' }
#'
y_count_labeler <- function(param, parameter_threshold, threshold_type, time_threshold = NULL, converted = F) {

  # general label
  gen_lab <- c('Count of Events Where ')
  st <- paste(threshold_type, parameter_threshold)
  st_time <- time_threshold

  # Parameter abbreviations
  wq_params <- c('temp', 'spcond', 'sal', 'do_pct', 'do_mgl'
                 , 'depth', 'cdepth', 'level', 'clevel'
                 , 'ph', 'turb', 'chlfluor')
  met_params <- c('atemp', 'rh', 'bp', 'wspd', 'maxwspd'
                  , 'maxwspdt', 'wdir', 'swdir', 'totpar'
                  , 'totprcp', 'totsorad')
  nut_params <- c('po4f', 'nh4f', 'no2f', 'no3f', 'no23f', 'chla_n', 'din', 'dip')

  wq_lab <- c('Water Temperature ', 'Specific Conductivity ', 'Salinity ', 'Dissolved Oxygen Saturation '
              , 'Dissolved Oxygen ', 'Sonde Depth ', 'Depth, Corrected for Barometric Pressure ', 'Sonde Depth '
              , 'Level, corrected for Barometric Pressure ', 'pH ', 'Turbidity ', 'Chlorophyll Fluorescence ')

  met_lab <- c('Air Temperature ', 'Relative Humidity ', 'Barometric Pressure ', 'Wind Speed '
               , 'Maximum Wind Speed ', 'Maximum Time of Wind Speed Measurement ', 'Wind Direction ', 'Wind Direction Standard Deviation '
               , 'Photosynthetically Active Radiation ', 'Precipitiation ', 'Total Solar Radiation ')

  nut_lab <- c('Orthophosphate ', 'Ammonium ', 'Nitrite ', 'Nitrate  ', 'Nitrite + Nitrate  ', 'Chlorophyll-a '
               , 'Dissolved Inorganic Nitrogen ', 'Dissolved Inorganic Phosphorus ')

  wq_units <- c('deg C', ' mS/cm', ' psu', ' %', ' mg/L', ' m', ' m', ' m', ' m', ' su', ' NTU', 'ug/L')
  met_units <- c('deg C', ' %', ' mb', ' m/s', ' m/s', ' hh:mm', ' ', ' sd', 'mmol/m^2', ' mm', 'W/m^2')
  nut_units <- c(' mg/L', ' mg/L', ' mg/L', ' mg/L', ' mg/L', 'ug/L', ' mg/L', ' mg/L')

  all_params <- c(wq_params, met_params, nut_params)
  all_labs <- c(wq_lab, met_lab, nut_lab)
  all_units <- c(wq_units, met_units, nut_units)

  names(all_labs) <- all_params
  names(all_units) <- all_params


  if(converted){
    # Create labels for select parameters in english units
    converted_param <- c('temp', 'depth', 'cdepth', 'level', 'clevel'
                         , 'atemp', 'wspd', 'maxwspd', 'totprcp')
    converted_units <- c('deg F', 'ft', 'ft', 'ft', 'ft', 'deg F', 'ft/s', 'ft/s','in')

    names(converted_units) <- converted_param

    if(!is.null(time_threshold)) {
      lab <- paste(gen_lab, all_labs[[param]], st, ' ', converted_units[[param]], ' for longer than ', st_time, ' hours', sep = '')
    } else {
      lab <- paste(gen_lab, all_labs[[param]], st, ' ', converted_units[[param]], sep = '')
    }

  } else {

    if(!is.null(time_threshold)) {
      lab <- paste(gen_lab, all_labs[[param]], st, ' ', all_units[[param]], ' for longer than ', st_time, ' hours', sep = '')
    } else {
      lab <- paste(gen_lab, all_labs[[param]], st, ' ', all_units[[param]], sep = '')
    }

  }

  return(lab)
}

