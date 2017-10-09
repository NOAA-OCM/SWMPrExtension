#' Tabulate Threshold Exceedances
#'
#' Tabulate user-specified threshold exceedances
#'
#' @param swmpr_in input data object
#' @param ... arguments passed to other methods
#' @param param vector of parameters to evaluate
#' @param parameter_threshold vector of numerical thresholds to evaluate parameters against
#' @param threshold_type vector of logical operators ('<', '>', '<=', '>=', '==', '!=')
#' @param time_threshold The amount of time an event must last to be counted (in hours)
#'
#' @concept analyze
#'
#' @import dplyr rlang
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @details Add some details about how this thing works here.
#' ++You must 'setstep' beforehand
#' ++Nutrient thresholds do not require a time threshold, but met and wq do
#' ++Recommended nutrient thresholds are determined by NCCR (say something about DIN)
#'
#' @author Julie Padilla
#'
#' @return A data frame of threshold exceedances by parameter
#'
#' @examples
#' \dontrun{
#' ## change the type argument if plotting discrete and continuous data
#' wq <- apacpwq
#' nut <- apacpnut
#' met <- apaebmet
#'
#' threshold_identification(wq, param = 'do_mgl'
#' , parameter_threshold = c(2)
#' , threshold_type = '<', time_threshold = 2)
#' threshold_identification(wq, param = c('do_mgl', 'ph', 'temp')
#' , parameter_threshold = c('2', '5', '30')
#' , threshold_type = c('<', '<', '>'), time_threshold = 2)
#'
#' threshold_identification(nut, param = 'do_mgl'
#' , parameter_threshold = c(2)
#' , threshold_type = '<', time_threshold = 2)
#' threshold_identification(nut, param = c('do_mgl', 'ph', 'temp')
#' , parameter_threshold = c('2', '5', '30')
#' , threshold_type = c('<', '<', '>'), time_threshold = 2)
#'
#' threshold_identification(met, param = 'tot_prcp', parameter_threshold = c(2)
#' , threshold_type = '<', time_threshold = 2)
#' threshold_identification(met, param = c('tot_prcp', 'temp', 'temp')
#' , parameter_threshold = c('2', '5', '30')
#' , threshold_type = c('<', '<', '>'), time_threshold = 2)
#'
#' }
threshold_identification <- function(swmpr_in, ...) UseMethod('threshold_identification')

#' @rdname threshold_identification
#'
#' @concept analyze
#'
#' @export
#'
#' @method threshold_identification swmpr

threshold_identification.swmpr <- function(swmpr_in, param, parameter_threshold, threshold_type, time_threshold = NULL, ...){

  dat <- swmpr_in

  # attributes
  station  <- attr(swmpr_in, 'station')
  parameters <- attr(swmpr_in, 'parameters')

  # stop if data has not been through QAQC
  if(attr(swmpr_in, 'qaqc_cols'))
    stop('QAQC columns present. QAQC data first')

  # stop if qualifiers are present in station
  if(!grepl('wq|met|nut', station))
    stop('station must include wq, met, or nut')

  # check if qualifiers are present in station_code
  if(grepl('wq|met', station) & is.null(time_threshold))
    stop('Specify time threshold (in hours)')

  # Other checks
  # stop if param not in input data names
  if(!param %in% names(dat))
    stop('Param argument must name input columns')

  # stop if time series is not standardized
  chk_step <- unique(diff(dat[, dat$datetimestamp]))
  if(length(chk_step) > 1)
    stop('The time step is not standardized, use setstep')

  # Prep ----
  # Filter data for parameters of interest
  # subset by parameters
  # if(!is.null(params)) parameters <- parameters[parameters %in% params]
  dat <- dat[, c(dat$datetimestamp, param)]

  # Set threshold time (in hrs)
  ts <- as.numeric(difftime(dat[, dat$datetimestamp][2], dat[, dat$datetimestamp][1], units = 'mins'))
  thr <- time_threshold * 60 / ts

  # Set parameters and parameter thresholds
  par <- param
  thresholds <- parameter_threshold
  thresh_type <- threshold_type

  # Prepare logical statements to be used in analysis
  statements <- paste(param, thresh_type, thresholds)
  df_statements <- data.frame(parameter = param, statement = statements, stringsAsFactors = F)

  # Functions (used for analysis) ----
  # Create a vector of TRUE/FALSE flags based on a data frame and
  # a logical statement (as str)
  generate_flags <- function(data, statement){
    with(data, eval(parse(text = statement)))
  }

  # Run length encoding for TRUE/FALSE values
  # x is a list of rle objects, y is a vector of datetime
  # tstep is the time step (in minutes)
  rel_tbl <- function(x, y, tstep){
    out <- data.frame(
      endtime = y[cumsum(x$lengths)],
      duration = x$lengths,
      thr_violation = x$values
    )

    out$starttime <- out$endtime - (out$duration - 1) * tstep * 60
    out <- out[, c(4, 1:3)]

    return(out)
  }

  # Analysis ----
  # Make the flags, make RLE objects, and classify events
  if(length(param) > 1){
    ls <- list(dat[, 2:(length(param) + 1)])
    x <- mapply(generate_flags, ls, statements) %>% unlist#(.)
    y <- apply(x, 2, rle)
    z <- lapply(y, rel_tbl, y = dat$datetimestamp, tstep = ts)

    # out <- z %>% plyr::ldply(.) #look for alternative to ldply, maybe something in tidyr or dplyr::bind_rows https://stackoverflow.com/questions/29265702/r-reorganize-list-into-dataframe-using-dplyr
    #NOTE rename the .id field to parameter
  }else{
    x <- generate_flags(dat, statements)
    y <- rle(x)
    z <- rel_tbl(x = y, y = dat[, dat$datetimestamp], tstep = ts)

    # out <- z %>%
    #   dplyr::mutate(.data$parameter = param)
  }

  # to do make sure the two output types look the same (e.g. location of '.id')
  out <- out %>%
    dplyr::filter(.data$thr_violation == TRUE, .data$duration > thr) %>%
    dplyr::left_join(.data, df_statements)

  return(out)
}
