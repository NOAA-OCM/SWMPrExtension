#' Assign seasons to SWMP sampling data
#' 
#' Assign seasons to SWMPr sampling data on a monthly basis or user-defined basis
#' 
#' @param data a vector of POSIXct dates
#' @param season A list of seasons. Months (1-12) are assigned to different groups based on user preference. Defaults to 12 months, starting with January. Must assign a minimum of two seasons
#' @param season_names A string vector of season names. The number of season names must match the length of the \code{season} list. A minimum of two seasons must be assigned (e.g., 'Wet', 'Dry'). Defaults to 12 months, starting with January. The number of season names must match the number of seasons
#' @param season_start defaults to 12 months, starting with January
#' @param abb logical, should abbreviations for month names be used? Defaults to \code{FALSE}
#' 
#' @importFrom lubridate month is.POSIXt
#' 
#' @export
#' 
#' @details A helper function used by several data analyses to assign seasons to sampling data and to order the seasons. This function also allows the user to designate which season should be first.
#' 
#' @return Returns a vector of ordered 
#' season factors. 
#' 
#' @seealso 
#' 
#' @examples \dontrun {
#' data(apacpwq)
#' dat <- apacpwq
#' 
#' seas <- assign_season(dat$datetimestamp)
#' levels(seas)
#' 
#' seas <- assign_season(dat$datetimestamp, abb = TRUE)
#' levels(seas)
#' 
#' seas <- assign_season(dat$datetimestamp, season_start = 'March')
#' levels(seas)
#' 
#' seas <- assign_season(dat$datetimestamp, season = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12)), season_names = c('Winter', 'Spring', 'Summer', 'Fall'), season_start = 'Spring')
#' levels(seas)
#' 
#' seas <- assign_season(dat$datetimestamp, season = list(c(10:12, 1:3), c(4:9)), season_names = c('Wet', 'Dry'))
#' levels(seas)
#' }
#' 
assign_season <- function(data, season = NULL, season_names = NULL, season_start = NULL, abb = FALSE) {
  
  dat <- data
  
  if(abb) {
    mo_nms <- month.abb
  } else {
    mo_nms <- month.name
  }
  
  # sanity checks
  
  #Check that the vector is POSIXct format
  if(!lubridate::is.POSIXt(dat))
    stop('Input data is not in POSIXt format. Reformat input data.')
  
  #if season != NULL, check that there are at least 2 seasons
  if(!is.null(season)) {
    if (length(season) <= 1) {stop('Number of seasons is equal to 1. Assign 2 or more seasons.')}
  }
  
  #if season_namess != NULL
  ##check that season != NULL
  ##check that season names are unique
  ##check that the number of season names matches the length of season
  ##warn user that no effect will happen if abb = TRUE for this instance
  if(!is.null(season_namess)) {
    if(is.null(season)) {stop('Season names assigned with no seasons')}
    if (length(season_namess) != length(unique(season_namess))) {stop('Season names are not unique. Assign unique season names.')}
    if (length(season_namess) != length(season)) {stop('List of seasons not equal to season names. Assign an equal number of seasons and season names')}
    if(abb) {warning('abb = TRUE irrelevant for user-defined seasons and season names.')}
  }
    
  #If season_start != NULL, check that the season_start is either a month name or a season name
  if(is.null(season_start)) {
    if(!is.null(season)) {season_start %in% mo_nms} else {season_start %in% season_names}
  }

  # Assigning the season
  if(is.null(season) & is.null(season_names)){
    x <- lubridate::month(dat)
    x <- mo_nms[x]
    x <- factor(x)
    x <- ordered(x, mo_nms)
    
    if(!is.null(season_start)){
      start <- match(c(season_start), mo_nms)
      x <- ordered(x, c(mo_nms[c(start:12)], mo_nms[c(1:start-1)]))
    }
    
  } else {
    x <- data.frame(month = lubridate::month(dat))
    names(season) <- season_names
    
    seas_nm <- unlist(lapply(1:length(season_names), function(x) rep(names(season[x]),length(season[[x]])))) #this doesn't work for weird season assignments
    df <- data.frame(month = unname(unlist(season)), nm = seas_nm)
    x <- left_join(x, df)
    x <- factor(x[, 2])
    x <- ordered(x, season_names)
    
    if(!is.null(season_start)){
      start <- match(c(season_start), season_names)
      x <- ordered(x, c(season_names[start:length(season_names)], season_names[c(1:start-1)]))
    }
  }
  
  return(x)
}