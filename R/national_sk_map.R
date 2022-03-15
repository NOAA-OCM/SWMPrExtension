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
## @import dplyr
#' @import ggplot2
#'
#' @importFrom dplyr filter left_join summarise transmute
#' @importFrom ggthemes theme_map
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom sf read_sf st_as_sf st_coordinates st_crop st_crs st_transform
#' @importFrom tidyr separate
#' @importFrom utils download.file unzip
#'
#' @export
#'
#' @details Create a base map of the US with options for including AK, HI, and PR. The user can choose which states and NERRS reserves to highlight. An early {sp}-based version of this function by Julie Padilla was developed, in part, from a blog post by Bob Rudis. The current {sf}-based version, by Dave Eslinger, uses an approach from the r-spatial tutorial by Mel Moreno and Mathieu Basille.
#'
#' To ensure the proper plotting of results, the order of the results vector for \code{sk_results} should match the order of the reserves vector for \code{sk_reserves}.
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
#' ##National map highlighting west coast and non-CONUS states and NERRS.
#' nerr_states_west <- c('02', '06', '41', '53', '72', '15')
#'
#' nerrs_codes <- c('pdb', 'sos', 'sfb', 'elk', 'tjr', 'kac', 'job', 'hee')
#'
#' nerrs_sk_results <- c('inc', 'inc', 'dec', 'insig', 'insuff', 'dec', 'inc', 'dec')
#'
#' national_sk_map(highlight_states = nerr_states_west,
#'                 sk_reserve = nerrs_codes, sk_results = nerrs_sk_results)
#'
national_sk_map <- function(incl = c('contig', 'AK', 'HI', 'PR')
                        , highlight_states = NULL
                        , sk_reserves = NULL
                        , sk_results = NULL
                        , sk_fill_colors = c('#444E65', '#A3DFFF',
                                             '#247BA0', '#0a0a0a')
                        , agg_county = TRUE) {

  loc_subsample <- function(loc, box, crs) {
    # Internal function to crop the SK data frame to fit
    #  within the sub-map bounding box.

    # Reproject location sf object
    loc_trans <- sf::st_transform(loc, crs)
    # Crop reproject locs to bbox
    loc_trans <- sf::st_crop(loc_trans, box)
    # Add projected X and Y coordinates
    loc_trans <- cbind(loc_trans, sf::st_coordinates(loc_trans))
    # Return new locs
    return(loc_trans)
  }

  if(length(sk_reserves) != length(sk_results))
    stop('A seasonal kendall result is required for each reserve in sk_reserve')

  # # ==========================================================================
  # Get Census geometry
  #  get_US_county_shape <- function() {
  #   shape <- "cb_2018_us_county_20m"
  #   # shape <- "cb_2018_us_state_20m"
  #
  #   remote <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/"
  #   zipped <- paste(shape,".zip", sep = "")
  #   local_dir <- tempdir()
  #   utils::download.file(paste(remote,shape,".zip",sep = ""),
  #                        destfile = file.path(local_dir, zipped))
  #   unzip(file.path(local_dir, zipped), exdir = local_dir)
  #   sf::read_sf(file.path(local_dir, paste(shape,".shp", sep = "") ) )
  # }
  # counties_4269 <- get_US_county_shape() %>%
  #   dplyr::transmute(fips = STATEFP, county_fips = GEOID, area = ALAND)
  # # Keep in native lat/lon, NAD83 projection, EPSG:4269.
  # save(counties_4269, file = "data/counties_4269")
  # us_4269 <- counties_4269 %>%
  #   dplyr::group_by(.data$fips) %>%
  #   dplyr::summarise(area = sum(.data$area))
  # save(us_4269, file = "data/us_4269.rda")
  #
# # ==========================================================================

  # read in saved US Census geometry {sf} object
  if(agg_county) {
    usa <- get('us_4269')
  } else {
    usa <- get('counties_4269')
  }
  # define local variables  to remove `check()` warnings
  X <- Y <- NULL


  # Get reserve locations for plotting
  # Prep reserve locations for plotting
  df_loc <- data.frame(NERR.Site.ID = sk_reserves, sk_results = sk_results,
                       stringsAsFactors = FALSE)

  res_locations <- reserve_locs(incl = incl) %>%
    dplyr::filter(.data$NERR.Site.ID %in% sk_reserves) %>%
    dplyr::left_join(df_loc)


  # Define vectors for the colors, shapes and sizes as needed:
  #   first and second entries are for states, "regular" and "highlighted,"
  #   respectively; third and forth entries are for reserve locations,
  #   "regular" and "highlighted" respecitvely;
  #   5 - 8 are for showing S-K trend results: 5 = increasing, 6 = decreasing,
  #   7 = insignificant, and 8 = insufficient data.
  # This convention holds for colors, shapes and size parameters. The order is
  #   consistent with the original order.

  fill_colors <-  c(c('#f8f8f8', '#cccccc', '#444e65', 'yellow'), sk_fill_colors)
  # fill_colors  <-  c('red', 'green', 'blue', 'yellow')
  line_color  <-  '#999999'
  res_point_size <-   c(0, 0,  2,  3,  4.9,  4.9,  4.5, 4.2)
  res_point_shape <-  c(0, 0, 21, 21, 24, 25, 21, 13)

  # These are the codes for the fill color, size and shape legends.
  break_vals <- c("0", "1", "3", "4", "inc", "dec", "insig", "insuff")
  res_png_shape <-  c(system.file("extdata", "up_arrow.png", package="SWMPrExtension"),
                      system.file("extdata", "down_arrow.png", package="SWMPrExtension"),
                      system.file("extdata", "dash.png", package="SWMPrExtension"),
                      system.file("extdata", "ex_square.png", package="SWMPrExtension"))

  master_key <- as.data.frame(cbind(break_vals, res_png_shape))
  loc_keys <- merge(res_locations, master_key, by.x = "sk_results", by.y = "break_vals") %>%
    select(-c(2:18))


  # Add fields for reserve point color and size, depending on highlight value
  if(is.null(highlight_states)) {
    usa$flag <- ("0")
  } else {
    usa$flag <- ifelse(usa$fips %in% highlight_states, "1", "0")
  }

  # Master map ----
  # Create master map with appropriate styles in lat-lon space.  Then create smaller maps
  # projected and properly bounded for their region.
  us_base <- ggplot(data = usa) +
    geom_sf(aes(fill = .data$flag), color = line_color, size = 0.15, show.legend = FALSE) +
    ggthemes::theme_map() +
    scale_color_manual(values = fill_colors, breaks = break_vals) +
    scale_fill_manual(values = fill_colors, breaks = break_vals) +
    scale_size_manual(values = res_point_size, breaks = break_vals) +
    scale_shape_manual(values = res_point_shape, breaks = break_vals)


  # Create sub-maps ----
  # Clip master map using coord_sf() and plot sk values.  Prior to plotting
  #  the SK values, crop them to the sub-map bounding box.
  loc_subsample <- function(loc, box, crs) {
    # Reproject location sf object
    loc_trans <- sf::st_transform(loc, crs)
    # Crop reproject locs to bbox
    loc_trans <- sf::st_crop(loc_trans, box)
    # Add projected X and Y coordinates
    loc_trans <- cbind(loc_trans, sf::st_coordinates(loc_trans))
    # Return new locs
    return(loc_trans)
  }

  main_bb <- c(xmin = -2500000, xmax = 2500000,  ymin = -2300000, ymax = 730000)
  main_pts <- loc_subsample(loc_keys, main_bb, 2163)
  mainland <- us_base +
    coord_sf(crs = sf::st_crs(2163), xlim = main_bb[1:2],
             ylim = main_bb[3:4])
  if(nrow(main_pts) > 0){
    mainland <- mainland +
    geom_image(data = main_pts,
               aes(x = X, y = Y, by = "height", image = res_png_shape),
               size = 0.04)
  }

  ak_bb <- c(xmin = -2400000, xmax = 1600000, ymin = 200000, ymax = 2500000)
  ak_pts <- loc_subsample(loc_keys, ak_bb, 3467)
  alaska <- us_base +
    coord_sf(crs = sf::st_crs(3467), xlim = ak_bb[1:2],
             ylim = ak_bb[3:4], expand = FALSE, datum = NA)
  if(nrow(ak_pts) > 0){
    alaska <- alaska  +
    geom_image(data = ak_pts,
               aes(x = X, y = Y, image = res_png_shape), by = "height",
               size = 1.7*0.04/(((ak_bb[4] - ak_bb[3])/2.0)/
                                  (main_bb[4] - main_bb[3]))) #0.14)
  }

  hi_bb <- c(xmin = -161, xmax = -154, ymin = 18, ymax = 23)
  hi_pts <- loc_subsample(loc_keys, hi_bb, 4135)
  hawaii  <- us_base +
    coord_sf(crs = sf::st_crs(4135), xlim = hi_bb[1:2],
             ylim = hi_bb[3:4], expand = FALSE, datum = NA)
  if(nrow(hi_pts) > 0){
    hawaii <- hawaii +
      geom_image(data = hi_pts,
                 aes(x = X, y = Y, image = res_png_shape), by = "height",
                 size = 2.1*0.04/(((hi_bb[4] - hi_bb[3])*135000)/
                                    (main_bb[4] - main_bb[3])) )# 0.25)
  }

  pr_bb <- c(xmin = 12000, xmax = 350000, ymin = 160000, ymax = 320000)
  pr_pts <- loc_subsample(loc_keys, pr_bb, 4437)
  pr <- us_base +
    coord_sf(crs = sf::st_crs(4437),xlim = pr_bb[1:2],
             ylim = pr_bb[3:4], expand = FALSE, datum = NA)
  if(nrow(pr_pts) > 0){
    pr <- pr +
    geom_image(data = pr_pts,
               aes(x = X, y = Y, image = res_png_shape), by = "height",
               size = 1.7*0.04/(((pr_bb[4] - pr_bb[3])*3)/
                              (main_bb[4] - main_bb[3])))#0.3)
  }

  # Now combine the smaller maps, as grobs, with annotation_custom into final object "gg"
  gg <- mainland +
    annotation_custom(grob = ggplotGrob(alaska),
                      xmin = -2750000, xmax = -2750000 + (1600000 - (-2400000)) / 2.0,
                      ymin = -2450000, ymax = -2450000 + (2500000 - 200000) / 2.0) +
    annotation_custom(grob = ggplotGrob(hawaii),
                      xmin =  -900000, xmax =  -900000 + (-154 - (-161)) * 135000,
                      ymin = -2450000, ymax = -2450000 + (23 - 18) * 135000) +
    annotation_custom(grob = ggplotGrob(pr),
                      xmin =   600000, xmax =   600000 + (350000 - 12000) * 3,
                      ymin = -2390000, ymax = -2390000 + (320000 - 160000) * 3)

  return(gg)
}
