#### SWMPrExtension 1.1.2
* Fixed annotation error in seasonal_dot.R; changed from annotate() to geom_text().

#### SWMPrExtension 1.1.1
* Updates to allow user to specify trend colors for `create_sk_flextable_list` function
* Additional updates to compensate for changes in Officer 0.3.3 flextable structure
* adding `free_y` argument to `threshold_percentile_plot`
* Minor fix to help files if searching by concept, e.g., `help.search('analyze', package = "SWMPrExtension")

#### SWMPrExtension 1.1.0
* Updates to plot legends for compatibility with ggplot2 3.0.0
* Updates to `threshold_percentile_plot()` to handle issues with the y-axis
* Updates to `threshold_summary()` to work with dplyr 0.8.0
* Added `remove_inf_and_nan()` to fix issues with range plots and dot_plot
* Corrections to custom mapping functions (label placement)

#### SWMPrExtension 1.0
* package maintainer changed from Julie Padilla to Dave Eslinger
* license changed to NOAA approved language

#### SWMPrExtension 0.3.16
* Updates to historical_range and historical_daily_range: analysis now allows for comparisons between a target year and a historical rnage that does not include the target year.

#### SWMPrExtension 0.3.15
* Updates to y-axis for threshold_percentile_plot

#### SWMPrExtension 0.3.14
* Changes to leaflet package resulted in station labels without formats. Currently this bug cannot be fully addressed. Basic formatting has been implemented for reserve level mapping functions
* Updates to:
 * res_sk_map
 * res_sk_custom_map
 * res_local_map
 * res_custom_map

#### SWMPrExtension 0.3.13
* Documentation updates and the addition of toy examples for testing
* Updates to historical_daily_range.swmpr
 * legend order when criteria = NULL is now equivalent to legend order when criteria argument is not null.

#### SWMPrExtension 0.3.12

* Added Bob Rudis and Marcus Beck as authors
