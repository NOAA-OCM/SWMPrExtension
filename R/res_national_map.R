#' Reserve National Map
#'
#' Create a base map for NERRS reserves in ggplot
#'
#' @param incl chr vector to include AK, HI , and PR (case sensitive)
#' @param highlight_states chr vector of state FIPS codes
#' @param highlight_reserves chr vector of 3 letter reserve codes
#' @param agg_county logical, should counties be aggregated tot he state-level? Defaults to \code{TRUE}
#'
#' @import ggplot2
#'
#' @importFrom ggthemes theme_map
#' @importFrom maptools elide spRbind unionSpatialPolygons
#' @importFrom rgdal readOGR
#' @importFrom rlang .data
#' @importFrom sp CRS bbox proj4string spTransform
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Create a base map of the US with options for including AK, HI, and PR. The user can choose which states and NERRS reserves to highlight.
#' This function was developed, in part, from a blog post by Bob Rudis.
#'
#' @author Bob Rudis, Julie Padilla
#' Maintainer: Julie Padilla
#'
#' @concept analyze
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @references
#' Rudis, Bob. 2014. "Moving The Earth (well, Alaska & Hawaii) With R". rud.is (blog). November 16, 2014. https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/
#'
#' @examples
#' ##National map highlighting states with NERRS
#' nerr_states <- c('01', '02', '06', '10', '12', '13', '15'
#' , '23', '24', '25', '27', '28', '33', '34', '36', '37', '39'
#' , '41', '44', '45', '48', '51', '53', '55', '72')
#'
#' res_national_map(highlight_states = nerr_states)
#'
#' \donttest{
#' #' ##Just the national map
#' res_national_map()
#'
#' ##National map highlighting west coast states and NERRS (including AK)
#' nerr_states_west <- c('02', '06', '41', '53')
#'
#' nerrs_codes <- c('pdb', 'sos', 'sfb', 'elk', 'tjr', 'kac')
#'
#' res_national_map(highlight_states = nerr_states_west, highlight_reserve = nerrs_codes)
#' }
res_national_map <- function(incl = c('contig', 'AK', 'HI', 'PR')
                        , highlight_states = NULL
                        , highlight_reserves = NULL
                        , agg_county = TRUE) {

  # Projection used is actually a North American Lambert Azimuthal Equal Area projection,
  #   (https://spatialreference.org/ref/sr-org/north-american-lambert-azimuthal-equal-area-projection/)
  #   not an Albers Equal Area projection. Changing all "_aea" to "_laea" to correct the
  #   labelling issue

  # get_US_county_2010_shape <- function() {
  #   dir <- tempdir()
  #   utils::download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_500k.zip", destfile = file.path(dir, "gz_2010_us_050_00_500k.zip"))
  #   unzip(file.path(dir, "gz_2010_us_050_00_500k.zip"), exdir = dir)
  #   rgdal::readOGR(file.path(dir, "gz_2010_us_050_00_500k.shp"))
  # }
  #
  # us <- get_US_county_2010_shape()
  # # loc <- get('sampling_stations')
  #
  # # project it to Lambert Azimuthal Equal Area, EPSG:2163
  # # projString <- "+init=epsg:2163"
  # NOTE BENE: Actually using non-standard WGS84 LAEA, no EPSG code
  # # projString <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
  # us_laea <- sp::spTransform(us, sp::CRS(projString))
  # us_laea@data$id <- rownames(us_laea@data)
  # # NOTE BENE: There is some step missing here to remove non-ascii characters.  That will create a much smaller data set.
  # save(us_laea,file = "data/us_laea.rda")

  us_laea <- get('us_laea')

  # remove old states and put new ones back in
  us_laea_mod <- us_laea[!us_laea$STATE %in% c("02", "15", "72"),]
  sp::proj4string(us_laea_mod) <- sp::proj4string(us_laea)

  if('AK' %in% incl) {
    # extract, then rotate, shrink & move alaska (and reset projection)
    # need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
    alaska <- us_laea[us_laea$STATE == "02", ]
    alaska <- maptools::elide(alaska, rotate = -50)
    alaska <- maptools::elide(alaska, scale = max(apply(sp::bbox(alaska), 1, diff)) / 2.3)
    alaska <- maptools::elide(alaska, shift = c(-2100000, -2500000))
    sp::proj4string(alaska) <- sp::proj4string(us_laea_mod)

    us_laea_mod <- maptools::spRbind(us_laea_mod, alaska)
  }

  if('HI' %in% incl) {
    # extract, then rotate & shift hawaii
    hawaii <- us_laea[us_laea$STATE == "15",]
    hawaii <- maptools::elide(hawaii, rotate = -35)
    hawaii <- maptools::elide(hawaii, shift=c(5400000, -1400000))
    sp::proj4string(hawaii) <- sp::proj4string(us_laea_mod)

    us_laea_mod <- maptools::spRbind(us_laea_mod, hawaii)
  }

  if('PR' %in% incl) {
    # extract, then rotate & shift pr
    pr <- us_laea[us_laea$STATE == "72", ]
    pr <- maptools::elide(pr, shift = c(-1400000,2000))
    sp::proj4string(pr) <- sp::proj4string(us_laea_mod)

    us_laea_mod <- maptools::spRbind(us_laea_mod, pr)

  }

  if(agg_county) {
    us_laea.diss <- maptools::unionSpatialPolygons(us_laea_mod, IDs = us_laea_mod@data$STATE)
    us_laea_mod <- us_laea.diss
  }

  # get ready for ggplotting it... this takes a cpl seconds ----
  map <- ggplot2::fortify(us_laea_mod, region = "GEO_ID")

  # Prep reserve locations for plotting
  reserve_locations <- reserve_locs(incl = incl)

  # plot it----

  # highlight some states
  gg <- ggplot()
  gg <- gg + coord_equal()
  gg <- gg + ggthemes::theme_map()
  gg <- gg + theme(plot.margin = unit(c(0, 0, 0, 0), "points")) #trbl

  if(is.null(highlight_states)) {
    gg <- gg + geom_map(data = map, map = map
                        , aes_string('long', 'lat', map_id = 'id')
                        , fill = '#f8f8f8', color = '#999999'
                        , size = 0.15, show.legend = FALSE)
  } else {
    map$flag <- ifelse(map$id %in% highlight_states, TRUE, FALSE)

    gg <- gg + geom_map(data = map, map = map
                        , aes_string('long', 'lat', map_id = 'id', fill = 'flag')
                        , color = '#999999', size = 0.15, show.legend = FALSE) +
      scale_fill_manual(values = c('#f8f8f8', '#cccccc'))
  }

  # add reserve locations
  # return(reserve_locations)
  gg <- gg +
    geom_point(data = reserve_locations, aes_string(x = 'Longitude', y = 'Latitude')
               , fill = '#444e65', shape = 21, size = 2)

  # add highlighted reserves, if specified
  if(!is.null(highlight_reserves)) {
    highlight_locations <- reserve_locs(incl = incl, subset_reserve =  highlight_reserves)

    gg <- gg + geom_point(data = highlight_locations, aes_string(x = 'Longitude', y = 'Latitude')
                          , fill = 'yellow', shape = 21, size = 3)
  }

  return(gg)
}
