#' Reserve National Map
#'
#' Create a base map for NERRS reserves in ggplot
#'
#' @param incl chr vector to include AK, HI , and PR (case sensitive)
#' @param highlight_states chr vector of state FIPS codes
#' @param highlight_reserves chr vector of 3 letter reserve codes
#' @param agg_county logical, should counties be aggregated to the state-level? Defaults to \code{TRUE}
#'
#' @import ggplot2
#'
#' @importFrom dplyr filter left_join transmute
#' @importFrom ggthemes theme_map
#' @importFrom magrittr "%>%"
#' @importFrom maps county map state state.fips world
#' @importFrom rgdal readOGR
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_crs
#' @importFrom tidyr separate
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Create a base map of the US with options for including AK, HI, and PR. The user can choose which states and NERRS reserves to highlight.
#' An early {sp}-based version of this function by Julie Padilla was developed, in part, from a blog post by Bob Rudis. The current {sf}-based version,
#' by Dave Eslinger,  uses the approach in the r-spatial tutorial by Mel Moreno and Mathieu Basille.
#'
#' @author Julie Padilla, Dave Eslinger
#' Maintainer: Dave Eslinger
#'
#' @concept analyze
#'
#' @return Returns a \code{\link[ggplot2]{ggplot}} object
#'
#' @references
#' Rudis, Bob. 2014. "Moving The Earth (well, Alaska & Hawaii) With R". rud.is (blog). November 16, 2014. https://rud.is/b/2014/11/16/moving-the-earth-well-alaska-hawaii-with-r/
#' Moreno, Mel, and Basille, Mathieu Basille. 2018. "Drawing beautiful maps programmatically with R, sf and ggplot2 — Part 3: Layouts" r-spatial (blog). October 25, 2018. https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
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

    fips <- maps::state.fips %>%
    tidyr::separate(polyname, sep = ":", into = c("state",NA),
                    fill = "right", remove = TRUE) %>%
    dplyr::transmute(fips = sprintf("%02d",fips), state, abb) %>%
    unique()

    if(agg_county) {
      conus <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
        dplyr::transmute(state = as.character(ID)) %>%
        dplyr::left_join(fips, by = "state")
    } else {
      print("Warning: County names/boundaries only available for CONUS")
      conus <- sf::st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>%
        tidyr::separate(ID, sep = ",", into = c("state","county")) %>%
        dplyr::left_join(fips, by = "state")
  }

  # The {maps} state and countylibrary only has data for CONUS, incl. DC
  # so get a "usa" data set from the world data, which includes HI and AK, but
  # has CONUS without interior boundaries.
  usa <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>%
    dplyr::transmute(admin = as.character(ID)) %>%
    dplyr::filter(admin == "USA")

  # Lastly, grab Puerto Rico from the same world database
  puertorico <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE)) %>%
    dplyr::transmute(admin = as.character(ID)) %>%
    dplyr::filter(admin == "Puerto Rico")

  # debugging highlight_states
  highlight_states <- c("02","12","15", "16","06","72")
  highlight_reserves <- c('pdb', 'sos', 'sfb', 'elk', 'tjr', 'kac')

  # Get reserve locations for plotting
  res_locations <- reserve_locs(incl = incl)

  # Define fill colors as needed, colors 1 & 2 are for states, 3 and 4 are for reserve locations:
  fill_colors  <-  c('#f8f8f8', '#cccccc', '#444e65', 'yellow')
  # fill_colors  <-  c('red', 'blue', 'green', 'yellow')
  line_color  <-  '#999999'
  res_point_size <- c(0, 0, 2, 3)
  res_point_shape <- c(0, 0, 21, 21)

  # These are the codes for the fill color, size and shape legends.
  break_vals <- c("0","1","3","4")

  # Add fields for reserve point color and size, depending on highlight value
  if(is.null(highlight_states)) {
    conus$flag <- ("0")
    usa$ak_flag <- ("0")
    usa$hi_flag <- ("0")
    puertorico$flag <- ("0")
  } else {
    conus$flag <- ifelse(conus$fips %in% highlight_states, "1", "0")
    usa$ak_flag <-  ifelse("02" %in% highlight_states, "1", "0")
    usa$hi_flag <-  ifelse("15" %in% highlight_states, "1", "0")
    puertorico$flag <-  ifelse("72" %in% highlight_states, "1", "0")
  }

  if(is.null(highlight_reserves)) {
    res_locations$rflag <- "3"
  } else {
    res_locations$rflag <- ifelse(res_locations$NERR.Site.ID
                                  %in% highlight_reserves, "4", "3")
  }

  # Create area-appropriate maps using WGS84 = EPSG:4326
  # These will be projected and properly bounded once Reserve locations are added
  mainland <- ggplot(data = conus) +
    geom_sf(aes(fill = flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    geom_sf(data = res_locations, aes(fill = rflag, shape = rflag, size = rflag), show.legend = FALSE) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals) +
    coord_sf(crs = sf::st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))

  alaska <- ggplot(data = usa) +
    geom_sf(aes(fill = ak_flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    geom_sf(data = res_locations, aes(fill = rflag, shape = rflag, size = rflag), show.legend = FALSE) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals) +
    coord_sf(crs = sf::st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000),
             expand = FALSE, datum = NA)

  hawaii  <- ggplot(data = usa) +
    geom_sf(aes(fill = hi_flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    geom_sf(data = res_locations, aes(fill = rflag, shape = rflag, size = rflag), show.legend = FALSE) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals) +
    coord_sf(crs = sf::st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA)

  pr <- ggplot(data = puertorico) +
    geom_sf(aes(fill = flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    geom_sf(data = res_locations, aes(fill = rflag, shape = rflag, size = rflag), show.legend = FALSE) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals) +
    coord_sf(crs = sf::st_crs(4437),xlim = c(12000,350000), ylim = c(160000, 320000),
             expand = FALSE, datum = NA)

  # Now combine maps, as grobs, with annotation_custom into final object "gg"
  gg <- mainland +
    annotation_custom(
      grob = ggplotGrob(alaska),
      xmin = -2750000,
      xmax = -2750000 + (1600000 - (-2400000)) / 2.0,
      ymin = -2450000,
      ymax = -2450000 + (2500000 - 200000) / 2.0) +
    annotation_custom(
      grob = ggplotGrob(hawaii),
      xmin = -1000000,
      xmax = -1000000 + (-154 - (-161)) * 135000,
      ymin = -2450000,
      ymax = -2450000 + (23 - 18) * 135000) +
    annotation_custom(
      grob = ggplotGrob(pr),
      xmin = 600000,
      xmax = 600000 + (350000 - 12000) * 3,
      ymin = -2390000,
      ymax = -2390000 + (320000 - 160000) * 3)

  return(gg)
}
