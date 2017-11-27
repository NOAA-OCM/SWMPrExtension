#' Generate y-axis Label Based on SWMP Parameter Abbreviation
#'
#' Generate a y-axis label based on SWMP parameter abbreviation
#'
#' @param param chr string of variable abbreviation
#' @param statement chr string of logical statement used to evaluate thresholds
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
#' y_lab <- y_count_labeler('do_mgl', '< 2 mg/L', converted = F)
#' }
#'
y_count_labeler <- function(param, statement, converted = F) {

  # general label
  gen_lab <- c('Count of Events Where ')
  st <- statement

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

  nut_lab <- c('Orthophosphate ', 'Ammonium ', 'Nitrite ', 'Nitrate  ', 'Nitrite + Nitrate  ', 'Chlorophyll-a'
               , 'Dissolved Inorganic Nitrogen ', 'Dissolved Inorganic Phosphorus ')

  wq_units <- c(quote(expression(paste(~degree, 'C'))), ' mS/cm', ' psu', ' %', ' mg/L'
                , ' m', ' m', ' m', ' m', ' su', ' NTU', quote(expression(paste(~mu, 'g/L'))))
  met_units <- c(quote(expression(paste(~degree, 'C'))), ' %', ' mb', ' m/s', ' m/s', ' hh:mm', ' '
                 , ' sd', quote(expression(paste('mmol/ '~m^2))), ' mm', quote(expression(paste('W/ '~m^2))))
  nut_units <- c(' mg/L', ' mg/L', ' mg/L', ' mg/L', ' mg/L', quote(expression(paste(~mu, 'g/L'))), ' mg/L', ' mg/L')

  all_params <- c(wq_params, met_params, nut_params)
  all_labs <- c(wq_lab, met_lab, nut_lab)
  all_units <- c(wq_units, met_units, nut_units)

  names(all_labs) <- all_params
  names(all_units) <- all_params

  tricky <- c('temp', 'chlfluor', 'atemp', 'totpar', 'totsorad', 'chla_n')

  if(param %in% tricky) {
    tricky_labs <- c(
      quote(expression(paste(gen_lab, 'Temperature ', statement, ~degree, 'C')))
      , quote(expression(paste(gen_lab, 'Chlorophyll Fluorescence ', statement, ~mu, 'g/L')))
      , quote(expression(paste(gen_lab, 'Air Temperature ', statement, ~degree, 'C')))
      , quote(expression(paste(gen_lab, 'Photosynthetically Active Radiation ', statement,'mmol/ ' ~m^2)))
      , quote(expression(paste(gen_lab, 'Total Solar Radiation ', statement,'W/ ' ~m^2)))
      , quote(expression(paste(gen_lab, 'Chlorophyll-a ', statement, ~mu, 'g/L')))
    )
    names(tricky_labs) <- tricky
    lab <- tricky_labs[[param]]
  } else {
    lab <- paste(gen_lab, all_labs[[param]], statement, all_units[[param]], sep = '')
  }

  # if(converted){
  #   # # Create labels for select parameters in english units
  #   # converted_param <- c('temp', 'depth', 'cdepth', 'level', 'clevel'
  #   #                      , 'atemp', 'wspd', 'maxwspd', 'totprcp')
  #   # converted_labs <- c(quote(expression(paste('Water Temperature (', ~degree, 'F)'))), 'Sonde Depth (ft)', 'Depth, Corrected for Barometric Pressure (ft)', 'Sonde Depth (ft)'
  #   #                     , 'Level, corrected for Barometric Pressure (ft)', quote(expression(paste('Air Temperature (', ~degree, 'F)'))), 'Wind Speed (ft/s)'
  #   #                     , 'Maximum Wind Speed (ft/s)','Precipitiation (in)')
  #   #
  #   # names(converted_labs) <- converted_param
  #   #
  #   # y_lab <- converted_labs[[param]]
  #
  # } else {
  #   y_lab <- units[[param]]
  # }

  return(lab)
}

