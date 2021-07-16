#' Update reserve sampling stations
#'
#' Script to modify the internal sampling_sites.rda file for new reserve or changes to existing sampling stations
#'
#'
#' @export
#'
#' @details Standalone script to add and/or modify the internal SWMPrExtension sampling_sites data table used by the get_sites.R function. It reads the current station information from `data/sampling_stations.rda`, writes out a backup, then modifies and writes out the data file with new station information appended.  Changes to be made are entered into `data/sampling_station_updates.csv`.  New stations will be added, existing stations will be overwritten with the new data.
#'
#' @author Dave Eslinger
#'
#' @concept data maintenance
#'

# Needs imports
#
# # Read in existing data
# # Create backup of .rda file.
# # Read in new information
# # Replace existing information with new information
# # Append new stations, updating Row count as needed
# # Write out new rda file.
#
