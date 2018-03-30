#' Convert Parameter Abbreviations
#'
#' Convert \code{SWMPr} parameter abbreviations into formats appropriate for use with NERRS reserve level template \code{\link[flextable]{flextable}}
#'
#' @param param chr, vector of parameter abbreviations
#'
#' @export
#'
#' @details A helper function used internally by \code{\link{create_sk_flextable_list}} to label \code{\link[flextable]{flextable}} columns in the trend table for the reserve level report.
#'
#' @author Julie Padilla
#'
#' @concept reporting
#'
#' @return Returns a \code{data.frame} of user-specified results to be displayed
#'
ft_col_names <- function(param) {
  # Parameter abbreviations
  wq_params <- c('temp', 'spcond', 'sal', 'do_pct', 'do_mgl'
                 , 'depth', 'cdepth', 'level', 'clevel'
                 , 'ph', 'turb', 'chlfluor')
  met_params <- c('atemp', 'rh', 'bp', 'wspd', 'maxwspd'
                  , 'swdir', 'totpar'
                  , 'totprcp', 'totsorad')
  nut_params <- c('po4f', 'nh4f', 'no2f', 'no3f', 'no23f', 'chla_n', 'din', 'dip')
  wq_lab <- c('Water Temperature', 'Specific Conductivity', 'Salinity', 'Dissolved Oxygen Sat.'
              , 'Dissolved Oxygen', 'Sonde Depth', 'Depth, Corrected', 'Level'
              , 'Level, Corrected', 'pH', 'Turbidity', 'Chlorophyll Fluorescence')

  met_lab <- c('Air Temperature', 'Relative Humidity', 'Barometric Pressure', 'Wind Speed'
               , 'Maximum Wind Speed', 'Wind Direction', 'Photosynthetically Active Radiation', 'Precipitiation', 'Total Solar Radiation')

  nut_lab <- c('Ortho-phosphate', 'Ammonium', 'Nitrite', 'Nitrate', 'Nitrite + Nitrate', 'Chlorophyll-a'
               , 'Diss. Inorganic Nitrogen', 'Diss. Inorganic Phosphorus')

  all_params <- c(wq_params, met_params, nut_params)
  labs <- c(wq_lab, met_lab, nut_lab)

  names(labs) <- all_params

  return(labs[param])
}
