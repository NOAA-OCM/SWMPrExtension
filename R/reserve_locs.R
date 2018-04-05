#' NERRS Sampling Location Data Frame
#'
#' Create a data.frame of selected NERRS locations for plotting with \code{res_national_map}
#'
#' @param incl Str vector to include AK, HI, and PR
#' @param subset_reserve chr string of 3 letter reserve IDs to include as data points. To include He'eia use the reserve code 'HEA'.
#'
#' @importFrom sp CRS proj4string spTransform
#'
#' @export
#'
#' @details A helper function used to generate a \code{data.frame} of selected reserve locations for use with \code{\link{res_national_map}}.
#'
#' @author Julie Padilla
#'
#' @concept miscellaneous
#'
#' @return Returns a \code{data.frame} for internal use with \code{res_national_map}
#'
reserve_locs <- function(incl = c('contig', 'AK', 'HI', 'PR'), subset_reserve = NULL) {

  loc <- get('sampling_stations')
  loc <- loc[(loc$Station.Type == 0 & loc$Status == 'Active' & loc$isSWMP == 'P'), ]

  # add hawaii
  if(!('hi' %in% unique(loc$State))) {
    hi <- c(NA, 'HEA', rep(NA, 2), 21.433193, 157.813641, rep(NA, 2), 'hi', rep(NA, 7))
    loc <- rbind(loc, hi)
  }

  # formatting
  loc$Latitude <- as.numeric(loc$Latitude)
  loc$Longitude <- as.numeric(loc$Longitude)
  loc$Longitude <- loc$Longitude * -1

  sp::coordinates(loc) <- ~Longitude + Latitude

  # remove select states
  sp::proj4string(loc) <- sp::CRS("+proj=longlat +datum=WGS84")
  loc <- sp::spTransform(loc, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  loc <- data.frame(loc)

  loc_mod <- loc[!loc$State %in% c("ak", "hi", "pr"), ]

  if('AK' %in% incl) {
    loc_add <- loc[loc$State == 'ak', ]
    loc_add$Latitude[1] <- -2100000
    loc_add$Longitude[1] <- -1200000

    loc_mod <- rbind(loc_mod, loc_add)
  }

  if('HI' %in% incl) {
    loc_add <- loc[loc$State == 'hi', ]
    loc_add$Latitude[1] <- -2010000
    loc_add$Longitude[1] <- -450000

    loc_mod <- rbind(loc_mod, loc_add)
  }

  if('PR' %in% incl) {
    loc_add <- loc[loc$State == 'pr', ]
    loc_add$Latitude[1] <- -2310000
    loc_add$Longitude[1] <- 2160000

    loc_mod <- rbind(loc_mod, loc_add)
  }

  if(!is.null(subset_reserve)) loc_mod <- loc_mod[(loc_mod$NERR.Site.ID %in% subset_reserve), ]

  return(loc_mod)
}
