#' Tabulate Threshold Exceedances
#'
#' Tabulate user-specified threshold exceedances
#'
#' @param swmpr_in input swmpr object
#' @param ... arguments passed to other methods
#' @param param vector of parameters to evaluate
#' @param parameter_threshold vector of numerical thresholds to evaluate parameters against
#' @param threshold_type vector of logical operators ('<', '>', '<=', '>=', '==', '!=')
#' @param time_threshold The amount of time an event must last to be counted (in hours)
#'
#' @importFrom dplyr filter left_join mutate select
#' @importFrom magrittr "%>%"
#' @importFrom stats complete.cases
#'
#' @export
#'
#' @details This function creates tabular summary of events when a user-specified threshold is exceeded.
#'
#' Before using this function, the user must apply \code{\link[SWMPr]{setstep}} to normalize the \code{datetimestamp} time step.
#'
#' For MET and WQ data, the user must specify \code{time_threshold}. This argument is the minimum duration that an event must last in order to be counted. For example, if \code{time_threshold = 2}, \code{param = "do_mgl"}, \code{parameter_threshold = 2}, and \code{threshold_type = "<"} then dissolved oxygen must be lower than 2 mg/L for more than two hours or the event will not be summarized in the final table. For NUT parameters, all exceedances are included in the tabular summary.
#'
#' Recommended thresholds for chlorophyll-a, dissolved inorganic nitrogen, dissolved inorganic phosphorus, and dissolved oxygen can be found in the National Coastal Condition Assessment 2010 (USEPA 2016)
#'
#' @author Julie Padilla
#'
#' @concept analyze
#'
#' @return Returns a data frame of threshold exceedances by parameter
#'
#' @references
#' United States Environmental Protection Agency (USEPA). 2016. "National Coastal Condition Assessment 2010". EPA 841-R-15-006.
#' https://cfpub.epa.gov/si/si_public_record_Report.cfm?dirEntryId=327030
#'
#' @examples
#' wq <- apacpwq
#'
#' dat_wq <- qaqc(wq, qaqc_keep = c(0, 3, 5))
#' dat_wq <- setstep(dat_wq)
#'
#' wq_pars<- threshold_identification(dat_wq, param = c('do_mgl', 'ph', 'temp')
#'                           , parameter_threshold = c(2, 5, 30)
#'                           , threshold_type = c('<', '<', '>'), time_threshold = 2)
#'
#' \dontrun{
#' wq_par<- threshold_identification(dat_wq, param = c('do_mgl')
#'                           , parameter_threshold = c(2)
#'                           , threshold_type = c('<'), time_threshold = 2)
#'
#'
#' ## time_threshold and setstep are not necessary for monthly parameters
#' nut <- apacpnut
#'
#' dat_nut <- qaqc(nut, qaqc_keep = c(0, 3, 5))
#'
#' nut_pars <- threshold_identification(dat_nut, param = c('chla_n', 'po4f')
#'                           , parameter_threshold = c(10, 0.01)
#'                           , threshold_type = c('>', '>'))
#'
#' nut_par <- threshold_identification(dat_nut, param = c('chla_n')
#'                           , parameter_threshold = c(10)
#'                           , threshold_type = c('>'))
#'
#' nut_err <- threshold_identification(dat_nut, param = c('chla_n')
#'                           , parameter_threshold = c(30)
#'                           , threshold_type = c('>'))
#'
#' }
threshold_identification <- function(swmpr_in, ...) UseMethod('threshold_identification')

#' @rdname threshold_identification
#'
#' @export
#'
#' @method threshold_identification swmpr

threshold_identification.swmpr <- function(swmpr_in
                                           , param
                                           , parameter_threshold
                                           , threshold_type
                                           , time_threshold = NULL
                                           , ...){

  dat <- swmpr_in

  # attributes
  station  <- attr(swmpr_in, 'station')
  parameters <- attr(swmpr_in, 'parameters')
  data_type <- substr(station, 6, nchar(station))

  # stop if data has not been through QAQC
  if(attr(swmpr_in, 'qaqc_cols'))
    stop('QAQC columns present. QAQC data first')

  # # stop if qualifiers are present in station
  # if(!grepl('wq|met|nut', station))
  #   stop('station must include wq, met, or nut')
  #
  # # check if qualifiers are present in station_code
  # if(grepl('wq|met', station) & is.null(time_threshold))
  #   stop('Specify time threshold (in hours)')

  # Other checks
  # stop if param not in input data names
  # if(!(param %in% names(dat)))
  #   stop('Param argument must name input columns')

  # Prep ----
  # Filter data for parameters of interest
  # subset by parameters
  # if(!is.null(params)) parameters <- parameters[parameters %in% params]
  dat <- dat[, c('datetimestamp', param)]

  # Set threshold time (in hrs)
  ts <- as.numeric(difftime(dat[, 'datetimestamp'][2], dat[, 'datetimestamp'][1], units = 'mins'))
  thr <- time_threshold * 60 / ts

  # Set parameters and parameter thresholds
  par <- param
  thresholds <- parameter_threshold
  thresh_type <- threshold_type

  # Prepare logical statements to be used in analysis
  statements <- paste(param, thresh_type, thresholds)
  df_statements <- data.frame(parameter = param, statement = statements, stringsAsFactors = FALSE)

  if(data_type != 'nut') {
    # stop if time series is not standardized
    chk_step <- unique(diff(dat[, 'datetimestamp']))
    if(length(chk_step) > 1)
      stop('The time step is not standardized, use setstep')

    # Helper functions (used for analysis) ----
    # Create a vector of TRUE/FALSE flags based on a data frame and
    # a logical statement (as str)
    generate_flags <- function(data, statement){with(data, eval(parse(text = statement)))}

    # Run length encoding for TRUE/FALSE values
    # x is a list of rle objects, y is a vector of datetime
    # tstep is the time step (in minutes)
    rel_tbl <- function(x, y, tstep){
      out <- data.frame(endtime = y[cumsum(x$lengths)], duration = x$lengths, thr_violation = x$values)

      out$starttime <- out$endtime - (out$duration - 1) * tstep * 60
      out <- out[, c(4, 1:3)]

      return(out)
    }

    # Analysis ----
    # Make the flags, make RLE objects, and classify events
    if(length(param) > 1){
      ls <- list(rep(dat, length(param)))

      x <- mapply(generate_flags, ls, statements)
      y <- apply(x, 2, rle)
      z <- lapply(y, rel_tbl, y = dat$datetimestamp, tstep = ts)

      names(z) <- param

      out <- bind_rows(z, .id = 'parameter')

    } else {
      x <- generate_flags(dat, statements)
      y <- rle(x)
      z <- rel_tbl(x = y, y = dat[, 'datetimestamp'], tstep = ts)

      z$parameter <- param

      out <- z %>%
        dplyr::mutate(.data$parameter == param)

      out<- dplyr::left_join(out, df_statements, by = 'parameter')

      out <- dplyr::select(out, .data$parameter, .data$starttime
                           , .data$endtime, .data$duration, .data$thr_violation)
    }

    out <- out %>%
      dplyr::filter(.data$thr_violation == TRUE, .data$duration > thr)

    # Check to see if data.frame has results
    if(is.data.frame(out) && nrow(out) == 0)
      warning('No results were returned using the user-specified thresholds. Set new thresholds and re-run.')

    out <- left_join(out, df_statements, by = 'parameter')

    #Convert duration to hrs
    out$duration <- out$duration * ts / 60

  } else {
    generate_nut_flags <- function(data, statement) {
      x <- with(data, eval(parse(text = statement)))
      data$flags <- x
      return(data)
    }

    if(length(param) > 1){
      ls <- list(rep(dat, length(param)))

      x <- mapply(generate_nut_flags, ls, statements, SIMPLIFY = FALSE)
      names(x) <- param

      out <- bind_rows(x, .id = 'parameter')

    } else {
      out <- generate_nut_flags(dat, df_statements[1, 2])

      out$parameter <- param
      out <- out[ , c(4, 1:3)]
    }

    out <- out[out$flags == TRUE, ]
    out <- out[rowSums(is.na(out)) != ncol(out), ]

    if(is.data.frame(out) && nrow(out) == 0)
      warning('No results were returned using the user-specified thresholds. Set new thresholds and re-run.')

    out <- out[, !(names(out) %in% param)]
    out <- left_join(out, df_statements)

    if(nrow(out) > 0) {
      out$duration <- NA
      out$endtime <- NA
      out <- select(out, .data$parameter, .data$datetimestamp
                    , .data$endtime, .data$duration, .data$flags
                    , .data$statement)

      names(out)[c(2, 5)] <-c('starttime', 'thr_violation')

      out$station <- ifelse(nrow(out) > 0, station, character())
      out <- out[ , c(7, 1:6)]
    }


  }



  return(out)
}
