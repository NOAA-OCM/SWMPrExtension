#' Reserve National Map with Seasonal Kendall Results
#'
#' Create a base map for NERRS reserves in ggplot with seasonal kendall results
#'
#' @param incl chr vector to include AK, HI , and PR (case sensitive)
#' @param highlight_states chr vector of state FIPS codes
#' @param sk_reserves chr vector of 3 letter reserve codes that have seasonal kendall results
#' @param sk_results chr vector of seasonal kendall results. Results can be 'inc', 'dec', 'insig', or 'insuff' which stand for 'increasing trend', 'decreasing trend', 'statistically insignificant trend', or 'insufficient data to detect trend'
#' @param sk_fill_colors chr vector of colors used to fill seasonal kendall result markers
#' @param agg_county logical, should counties be aggregated to the state-level? Defaults to \code{TRUE}
#'
#' @import ggplot2
#'
#' @importFrom dplyr left_join
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
#' To ensure the proper plotting of results, the order of the results vector for \code{sk_results} should match the order of the reserves vector for \code{sk_reserves}.
#'
#' @author Bob Rudis, Julie Padilla
#'
#' @concept analyze
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @references
#' Rudis, Bob. 2014. "Moving The Earth (well, Alaska & Hawaii) With R". rud.is (blog). November 16, 2014. https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/
#'
#' @examples
#' \dontrun{
#'
#'
#' ##National map highlighting west coast states and NERRS (including AK)
#' nerr_states_west <- c('02', '06', '41', '53')
#'
#' nerrs_codes <- c('pdb', 'sos', 'sfb', 'elk', 'tjr', 'kac')
#' nerrs_sk_results <- c('inc', 'inc', 'dec', 'insig', 'insuff', 'dec')
#'
#' national_sk_map(highlight_states = nerr_states_west,
#' sk_reserve = nerrs_codes, sk_results = nerrs_sk_results)
#' }
#'
national_sk_map <- function(incl = c('contig', 'AK', 'HI', 'PR')
                        , highlight_states = NULL
                        , sk_reserves = NULL
                        , sk_results = NULL
                        , sk_fill_colors = c('#247BA0', '#A3DFFF', '#444E65', '#595959')
                        , agg_county = T) {

  if(length(sk_reserves) != length(sk_results))
    stop('A seasonal kendall result is required for each reserve specified in sk_reserve')

  get_US_county_2010_shape <- function() {
    dir <- tempdir()
    utils::download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
    unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
    rgdal::readOGR(file.path(dir, "gz_2010_us_050_00_20m.shp"))
  }

  us <- get_US_county_2010_shape()

  # convert it to Albers equal area
  us_aea <- sp::spTransform(us, sp::CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
  us_aea@data$id <- rownames(us_aea@data)

  # remove old states and put new ones back in
  us_aea_mod <- us_aea[!us_aea$STATE %in% c("02", "15", "72"),]

  if('AK' %in% incl) {
    # extract, then rotate, shrink & move alaska (and reset projection)
    # need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
    alaska <- us_aea[us_aea$STATE == "02", ]
    alaska <- maptools::elide(alaska, rotate = -50)
    alaska <- maptools::elide(alaska, scale = max(apply(sp::bbox(alaska), 1, diff)) / 2.3)
    alaska <- maptools::elide(alaska, shift = c(-2100000, -2500000))
    sp::proj4string(alaska) <- sp::proj4string(us_aea)

    us_aea_mod <- maptools::spRbind(us_aea_mod, alaska)
  }

  if('HI' %in% incl) {
    # extract, then rotate & shift hawaii
    hawaii <- us_aea[us_aea$STATE == "15",]
    hawaii <- maptools::elide(hawaii, rotate = -35)
    hawaii <- maptools::elide(hawaii, shift=c(5400000, -1400000))
    sp::proj4string(hawaii) <- sp::proj4string(us_aea)

    us_aea_mod <- maptools::spRbind(us_aea_mod, hawaii)
  }

  if('PR' %in% incl) {
    # extract, then rotate & shift pr
    pr <- us_aea[us_aea$STATE == "72", ]
    pr <- maptools::elide(pr, shift = c(-1400000,2000))
    sp::proj4string(pr) <- sp::proj4string(us_aea)

    us_aea_mod <- maptools::spRbind(us_aea_mod, pr)
  }

  if(agg_county) {
    us_aea.diss <- maptools::unionSpatialPolygons(us_aea_mod, IDs = us_aea_mod@data$STATE)
    us_aea_mod <- us_aea.diss
  }

  # get ready for ggplotting it... this takes a cpl seconds ----
  map <- ggplot2::fortify(us_aea_mod, region = "GEO_ID")

  # Prep reserve locations for plotting
  df_loc <- data.frame(NERR.Site.ID = sk_reserves, sk_res = sk_results, stringsAsFactors = F)

  reserve_locations <- reserve_locs(incl = incl)
  reserve_locations <- reserve_locations[reserve_locations$NERR.Site.ID %in% sk_reserves, ]
  reserve_locations <- dplyr::left_join(reserve_locations, df_loc)

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
                        , size = 0.15, show.legend = F)
  } else {
    map$flag <- ifelse(map$id %in% highlight_states, TRUE, FALSE)

    gg <- gg + geom_map(data = map, map = map
                        , aes_string('long', 'lat', map_id = 'id', fill = 'flag')
                        , color = '#999999', size = 0.15, show.legend = F) +
      scale_fill_manual(values = c('#f8f8f8', '#BBBBBB'))
  }

  # add reserves with insufficient data for trend
  if('insuff' %in% sk_results) {

    df <- reserve_locations[reserve_locations$sk_res == 'insuff', ]

    gg <-
      gg +
      geom_point(data = df
                 , aes_string(x = 'Longitude', y = 'Latitude'), shape = 4
                 , color = sk_fill_colors[4], size = 3, stroke = 1.5)
  }

  # add reserves with insignificant trend
  if('insig' %in% sk_results) {

    df <- reserve_locations[reserve_locations$sk_res == 'insig', ]

    gg <-
      gg +
      geom_point(data = df
                 , aes_string(x = 'Longitude', y = 'Latitude'), shape = 45
                 , color = sk_fill_colors[3], size = 4, stroke = 20)
  }

  # add reserves with increasing trend
  if('inc' %in% sk_results) {

    df <- reserve_locations[reserve_locations$sk_res == 'inc', ]

    gg <-
      gg +
      geom_point(data = df
                 , aes_string(x = 'Longitude', y = 'Latitude'), shape = 24
                 , color = sk_fill_colors[1] , fill = sk_fill_colors[1], size = 5)
  }

  # add reserves with decreasing trend
  if('dec' %in% sk_results) {

    df <- reserve_locations[reserve_locations$sk_res == 'dec', ]

    gg <-
      gg +
      geom_point(data = df
                 , aes_string(x = 'Longitude', y = 'Latitude'), shape = 25
                 , color = sk_fill_colors[2] , fill = sk_fill_colors[2], size = 5)
  }

  return(gg)
}
