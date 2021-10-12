### SWMPrExtension 2.0.0.1
* remove tmap, add vector based background maps
* going back to ggmap, but with optional vector map if ggmap not installed.

### SWMPrExtension 2.0.0
* Public release after merge of sf_transition and tmap branches. 
* Addresses issue #29: replacing {sp} with {sf} 
* Addresses issue #44: PhantomJS replacement

### SWMPrExtension 1.1.8.2
* Replacing {ggmap} functionality with {tmap} functionality.

### SWMPrExtension 1.1.8.1
* Addressing ISSUE #29: replacing {sp} with {sf}
* Changed source file for national map to Long/Lat, NAD83, i.e., EPSG = 4269.
* Switch national map projection used in mapping to EPSG=2163, a standard (i.e., has an EPSG code), spherical Lambert Azimuthal Equal Area projection. Note that HI, PR, and AK all us different projections, as appropriate.
* Removed all {sp}, {leaflet} and {tmap} uses.  Replaced with {sf} and {ggmap}.
* Added ability to use stamen maps for local maps.
* Lost ability to put a scalebar on reserve-level maps.

### SWMPrExtension 1.1.8
* Fixing issue with x-axis labels overwriting themselves.  Adding a variable major tick capability.

### SWMPrExtension 1.1.7
* Added update_sampling_stations.R function to update reserve-specific station data when a new reserve is added, or SWMP stations are moved.
* Description files for the data files needed for the above change.
* Changes to left_join() calls
* changes to "guide = " values

### SWMPrExtension 1.1.6.5
* Changed gather() to pivot_longer in summarise_handoff_files.R

### SWMPrExtension 1.1.6.4
* Change units for converted wind speed

### SWMPrExtension 1.1.6.3
* Specify extra keys to reduce informational messages from joins and geom_smooth.
* Changed NA test and replacement for lm_p_labs.R p-value labeling.
* Fixed errors in threshold_percentile_plot.R when using `by_month = TRUE`. Also changed example times to remove error messages.

#### SWMPrExtension 1.1.6
* Fixed bug in threshold_identification.R with multi-variable nutrient calls
* Changes to most `dplyr::summarise` calls to add `.groups = "drop_last"`. In previous versions of {dplyr}, this was the default, but with changes in dplyr 1.0, it broke the threshold_summary code.  To stay consistent with previous default behavior, all instances of `group_by` on more than one group had the `.groups = "drop_last"` argument added if summarise was invoked.
* Changes to CRS definitions in spatial data files to add a comment containing
a WKT2 CRS representation.
* Changes to examples to make naming more consistent and less duplicative within the same function.

#### SWMPrExtension 1.1.5
* Changes in lm_pLlabs.R to prepare for upcoming release of {broom} 0.7, which removed rowwise tidier functions.

#### SWMPrExtension 1.1.4
* Updates for R 4.0 and associated changes
* Fixed {tibble} related issues in seasonal_dot.R and historical_range.R
* Fixed pre-existing error in threshold_percentile_plot.R if target year is outside of historical range.

#### SWMPrExtension 1.1.3
* Fixed ISSUE# 46: Addressed {rgdal} & {sp} issues when using PROJ > 6. 
* Corrected mismatched and mislabeled map projections in national mapping code so that all national-level maps use a Lambert Azimuthal Equal Area projections.  This is the projection that was being used for some shapefiles, but was mislabelled as Albers Equal Area, which then lead to some projection mismatch errors.

#### SWMPrExtension 1.1.2
* Fixed annotation error in seasonal_dot.R; changed from `annotate()` to `geom_text()`.

#### SWMPrExtension 1.1.1
* Updates to allow user to specify trend colors for `create_sk_flextable_list` function
* Additional updates to compensate for changes in Officer 0.3.3 flextable structure
* adding `free_y` argument to `threshold_percentile_plot`
* Minor fix to help files if searching by concept, e.g., `help.search('analyze', package = "SWMPrExtension")`

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
* Updates to historical_range and historical_daily_range: analysis now allows for comparisons between a target year and a historical range that does not include the target year.

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
