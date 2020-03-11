#' NERRS Sampling Location Data Frame
#'
#' Create a data.frame of selected NERRS locations for plotting with \code{res_national_map}
#'
#' @param incl Str vector to include AK, HI, and PR
#' @param subset_reserve chr string of 3 letter reserve IDs to include as data points. To include He'eia use the reserve code 'HEA'.
#'
#' @importFrom sf st_as_sf st_transform
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

  # Convert to {sf} object in WGS84 lat/lon projection.  Keep coordinate fields just in case.
  loc <- sf::st_as_sf(loc, coords = c("Longitude", "Latitude"), crs = 4236, remove = FALSE)


  # #  Project to our mainland projection, epsg = 2163
  # loc <- sf::st_transform(loc, 2163)
  #
  # # remove select states
  # loc_mod <- loc[!loc$State %in% c("ak", "hi", "pr"), ]

    # Keep ak, hi and pr, filter in plotting routine  loc_mod <- loc
   loc_mod <- loc


     #
  # # reproject and add back in AK, HI, and PR individually as needed:
   # # DOesn't work, can't rbind multiple CRS's together.
  # if('AK' %in% incl) {
  #   loc_add <- sf::st_transform(loc[loc$State == 'ak', ], 3467)
  #   loc_mod <- rbind(loc_mod, loc_add)
  # }
  #
  # if('HI' %in% incl) {
  #   loc_add <- loc[loc$State == 'hi', ]
  #   loc_add <- sf::st_transform(loc[loc$State == 'ak', ], 4135)
  #   loc_mod <- rbind(loc_mod, loc_add)
  # }
  #
  # if('PR' %in% incl) {
  #   loc_add <- loc[loc$State == 'pr', ]
  #   loc_add <- sf::st_transform(loc[loc$State == 'ak', ], 4437)
  #   loc_mod <- rbind(loc_mod, loc_add)
  # }

  if(!is.null(subset_reserve)) loc_mod <- loc_mod[(loc_mod$NERR.Site.ID %in% subset_reserve), ]

  return(loc_mod)
}
