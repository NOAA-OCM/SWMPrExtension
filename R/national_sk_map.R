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
#' This function was developed, in part, from a blog post by Bob Rudis.
#'
#' To ensure the proper plotting of results, the order of the results vector for \code{sk_results} should match the order of the reserves vector for \code{sk_reserves}.
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
#' ##National map highlighting west coast states and NERRS (including AK)
#' nerr_states_west <- c('02', '06', '41', '53')
#'
#' nerrs_codes <- c('pdb', 'sos', 'sfb', 'elk', 'tjr', 'kac')
#'
#' nerrs_sk_results <- c('inc', 'inc', 'dec', 'insig', 'insuff', 'dec')
#'
#' national_sk_map(highlight_states = nerr_states_west, sk_reserve = nerrs_codes, sk_results = nerrs_sk_results)
#'
national_sk_map <- function(incl = c('contig', 'AK', 'HI', 'PR')
                        , highlight_states = NULL
                        , sk_reserves = NULL
                        , sk_results = NULL
                        , sk_fill_colors = c('#247BA0', '#A3DFFF', '#444E65', '#0a0a0a')
                        , agg_county = TRUE) {

  if(length(sk_reserves) != length(sk_results))
    stop('A seasonal kendall result is required for each reserve specified in sk_reserve')

  # ========================================================================================================
  # Get Census geometry
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
  # us_4269 <- get_US_county_shape() %>%
  #   select(fips = STATEFP)
  # # Keep in native lat/lon, NAD83 projection, EPSG:4269.
  # save(us_4269, file = "data/us_4269.rda")
  # ========================================================================================================

  # read in saved US Census geometry {sf} object
  get('us_4269')

  # -----------------------------------------------------
  if(agg_county) {
    usa <- us_4269 %>%
      dplyr::group_by(fips) %>%
      dplyr::summarise()
  } else {
    usa <- us_4269
  }

  # test values for debugging
  # highlight_states <- c("02","12","15", "16","06","72")
  # sk_reserves  <- c('pdb', 'sos', 'sfb', 'elk', 'tjr', 'kac')
  # sk_results <- c('inc', 'inc', 'dec', 'insig', 'insuff', 'insuff')
  # sk_fill_colors = c('#247BA0', '#A3DFFF', '#444E65', '#595959')

  # Get reserve locations for plotting
  # Prep reserve locations for plotting
  df_loc <- data.frame(NERR.Site.ID = sk_reserves, sk_res = sk_results, stringsAsFactors = FALSE)

  res_locations <- reserve_locs(incl = incl) %>%
    filter(NERR.Site.ID %in% sk_reserves) %>%
    dplyr::left_join(df_loc)


  # Define vectors for the colors, shapes and sizes as needed:
  #   first and second entries are for states, "regular" and "highlighted" respecitvely;
  #   third and forth entries are for reserve locations, "regular" and "highlighted" respecitvely;
  #   5 - 8 are for showing S-K trend results: 5 = increasing trend, 6 = decreasing, 7 = insignificant,
  #   and 8 = insufficient data.
  # This convention holds for colors, shapes and size parameters. The order is
  #   consistent with the original order.

  fill_colors  <-  c( c('#f8f8f8', '#cccccc', '#444e65', 'yellow'), sk_fill_colors)
  # fill_colors  <-  c('red', 'blue', 'green', 'yellow')
  line_color  <-  '#999999'
  res_point_size <-   c(0, 0,  2,  3,  4.9,  4.9,  4.5, 4.2)
  res_point_shape <-  c(0, 0, 21, 21, 24, 25, 21, 13)
 # res_point_stroke <- c(1, 1,  1,  1,  1,  1, 20, 1.5)

  # These are the codes for the fill color, size and shape legends.
  break_vals <- c("0", "1", "3", "4", "inc", "dec", "insig", "insuff")

  # Add fields for reserve point color and size, depending on highlight value
  if(is.null(highlight_states)) {
    usa$flag <- ("0")
  } else {
    usa$flag <- ifelse(usa$fips %in% highlight_states, "1", "0")
  }

  # Create area-appropriate maps using the lat lon data.  These will
  # be projected and properly bounded once Reserve locations are added
  mainland <- ggplot(data = usa) +
    geom_sf(aes(fill = flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    geom_sf(data = res_locations, aes(color = sk_res, fill = sk_res, shape = sk_res, size = sk_res), show.legend = FALSE) +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals) +
    coord_sf(crs = sf::st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000))

  alaska <- ggplot(data = usa) +
    geom_sf(aes(fill = flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    geom_sf(data = res_locations, aes(color = sk_res, fill = sk_res, shape = sk_res, size = sk_res), show.legend = FALSE) +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals) +
    coord_sf(crs = sf::st_crs(3467), xlim = c(-2400000, 1600000), ylim = c(200000, 2500000),
             expand = FALSE, datum = NA)

  hawaii  <- ggplot(data = usa) +
    geom_sf(aes(fill = flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    geom_sf(data = res_locations, aes(color = sk_res, fill = sk_res, shape = sk_res, size = sk_res), show.legend = FALSE) +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals) +
    coord_sf(crs = sf::st_crs(4135), xlim = c(-161, -154), ylim = c(18, 23), expand = FALSE, datum = NA)

  pr <- ggplot(data = usa) +
    geom_sf(aes(fill = flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    geom_sf(data = res_locations, aes(color = sk_res, fill = sk_res, shape = sk_res, size = sk_res), show.legend = FALSE) +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
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
