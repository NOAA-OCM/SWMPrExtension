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
#' @import dplyr
#'
#' @importFrom ggthemes theme_map
##' @importFrom maptools elide spRbind unionSpatialPolygons
#' @importFrom rgdal readOGR
#' @importFrom rlang .data
##' @importFrom sp CRS bbox proj4string spTransform
#' @importFrom utils download.file unzip
#' @importFrom tidyr separate
#' @importFrom sf st_crs st
#' @importFrom maps map county state world
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

  # Function used to rotate geometry of {sf} objects
  # rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

  # Projection used is that used by the US National Atlas, which is a North American
  #   Lambert Azimuthal Equal Area projection, EPSG = 2163,
  #   (https://spatialreference.org/ref/epsg/2163/)
  # epsg = 2163

  # get_US_county_shape <- function() {
  #   shape <- "cb_2018_us_county_20m"
  #   # shape <- "cb_2018_us_state_20m"
  #   remote <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/"
  #   zipped <- paste(shape,".zip", sep = "")
  #   local_dir <- tempdir()
  #   utils::download.file(paste(remote,shape,".zip",sep = ""),
  #                        destfile = file.path(local_dir, zipped))
  #   unzip(file.path(local_dir, zipped), exdir = local_dir)
  #   sf::st_read(file.path(local_dir, paste(shape,".shp", sep = "") ) )
  # }
  # us <- get_US_county_shape()
  # # project it to Lambert Azimuthal Equal Area, EPSG:2163
  # us_laea <- sf::st_transform(us, epsg)
  # save(us_laea,file = "data/us_laea.rda")

  # us_laea <- get('us_laea')
  #
  # # remove old states and put new ones back in
  # us_laea_mod <- filter(us_laea, ! STATEFP %in% c("02", "15", "72") )
  #
  # # usc_laea_mod <- usc_laea %>%
  # #   filter(! STATEFP %in% c("02", "15", "72") )
  #
  #
  # if('AK' %in% incl) {
  #   # extract, then rotate, shrink & move alaska (and reset projection)
  #   # need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
  #   alaska <- us_laea[us_laea$STATEFP == "02", ] %>%
  #     sf::st_simplify(preserveTopology = TRUE, dTolerance = 5e3 )
  #   shift = c(1.2e6, -4.9e6)
  #   shift = c(0.6e6, -2.e6)
  #   alaska$geometry = (alaska$geometry) * rot(-50 * pi/180) * .45
  #   sf::st_crs(alaska) <- sf::st_crs(us_laea)
  #   alaska$geometry[[1]] <- alaska$geometry[[1]] + sf::st_point(shift)
  #   sf::st_crs(alaska) <- sf::st_crs(us_laea)
  #   us_laea_mod <- sf::st_union(us_laea_mod, alaska)
  #   # plot(sf::st_geometry(us_laea_mod2), border = "green")
  #  }
  #
  # if('HI' %in% incl) {
  #   hawaii <- us_laea[us_laea$STATEFP == "15", ]
  #   shift = c(3.8e6, 1.8e6)
  #   hawaii$geometry = (hawaii$geometry ) * rot(-35 * pi/180)
  #   sf::st_crs(hawaii) <- sf::st_crs(us_laea)
  #   hawaii$geometry[[1]] <- hawaii$geometry[[1]] + sf::st_point(shift)
  #   # Can add some offsets directly to the $geometry list to #  move the feature
  #   sf::st_crs(hawaii) <- sf::st_crs(us_laea)
  #   us_laea_mod <- sf::st_union(us_laea_mod, hawaii)
  #   # plot(st_geometry(us_laea_mod), border = "blue")
  # }
  #
  # if('PR' %in% incl) {
  #   # extract, then rotate & shift pr
  #   pr <- us_laea[us_laea$STATEFP == "72", ]
  #   shift = c( -1.4e6, 2e3)
  #   pr$geometry[[1]] <- pr$geometry[[1]] + sf::st_point(shift)
  #   # Can add some offsets directly to the $geometry list to #  move the feature
  #   sf::st_crs(pr) <- sf::st_crs(us_laea)
  #   us_laea_mod <- sf::st_union(us_laea_mod, pr)
  #   # plot(st_geometry(us_laea_mod), border = "red")
  # }
  # -------------------------------------
  library(maps)

  fips <- state.fips %>%
    tidyr::separate(polyname, sep = ":", into = c("state",NA),
                    fill = "right", remove = TRUE) %>%
    transmute(fips = sprintf("%02d",fips), state, abb) %>%
    unique()

    if(agg_county) {
    print("Warning: County names/boundaries only available for CONUS")
    statesc <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>%
      separate(ID, sep = ",", into = c("state","county")) %>%
      left_join(fips, by = "state")
  } else {
    states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
      transmute(state = ID) %>%
      left_join(fips, by = "state")
  }


  # The {maps} state and countylibrary only has data for CONUS, incl. DC
  # so get a "usa" data set from the world data, which includes HI and AK, but
  # has CONUS without interior boundaries.
  usa <- subset(world, admin == "United States of America")

  # Lastly, grab Puerto Rico from the same world database
  puertorico <- subset(world, admin == "Puerto Rico")


  # Now we have the data, all in WGS84, EPSG=4326
  # Figure out fill colors as needed:
  if(is.null(highlight_states)) {
    states$flag <- FALSE
  } else {
    states$flag <- ifelse(states$fips %in% highlight_states, TRUE, FALSE)

    gg <- gg + geom_map(data = map, map = map
                        , aes_string('long', 'lat', map_id = 'id', fill = 'flag')
                        , color = '#999999', size = 0.15, show.legend = FALSE) +
      scale_fill_manual(values = c('#f8f8f8', '#cccccc'))
  }

  fill_colors  <-  c('#f8f8f8', '#cccccc') #'#f8f8f8'
  line_color  <-  '#999999'

  # Project as needed and create area-appropriate maps

  mainland <- ggplot(data = states) +
    geom_sf(fill = flag, color = '#999999', size = 0.15, show.legend = FALSE) +
    scale_fill_manual(values = fill_colors) +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000),
               ylim = c(-2300000, 730000))

  alaska <- ggplot(data = usa) +
      geom_sf(fill = "cornsilk") +
      coord_sf(crs = st_crs(3467), xlim = c(-2400000, 1600000),
               ylim = c(200000, 2500000), expand = FALSE, datum = NA)

  hawaii  <- ggplot(data = usa) +
      geom_sf(fill = "cornsilk") +
      coord_sf(crs = st_crs(4135), xlim = c(-161, -154),
               ylim = c(18, 23), expand = FALSE, datum = NA)


  pr <- ggplot(data = puertorico) +
    geom_sf(fill = "cornsilk") +
    coord_sf(crs = st_crs(4437),xlim = c(12000,350000),
             ylim = c(160000, 320000), expand = FALSE, datum = NA)
  # 440489.48 332912.68
  # 11900.57 149804.42


  # -------------------------------------
  # get ready for ggplotting it... this takes a cpl seconds ----
  us_laea_mod$id <- us_laea_mod$STATEFP
  map <- ggplot2::fortify(us_laea_mod, region = "GEOID")

  # Prep reserve locations for plotting
  reserve_locations <- reserve_locs(incl = incl)

  # plot it----

  # highlight some states
  gg <- ggplot()
  gg <- gg + coord_equal()
  gg <- gg + ggthemes::theme_map()
  gg <- gg + theme(plot.margin = unit(c(0, 0, 0, 0), "points")) #trbl

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
