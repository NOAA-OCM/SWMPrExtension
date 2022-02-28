#' Update reserve sampling stations
#'
#' Script to modify the internal sampling_sites.rda file for adding new reserves or for making changes to existing sampling station locations.
#'
#' @param file_path path to directory with new file
#' @param file_name name of new csv file
#'
#' @importFrom dplyr across case_when everything mutate select
#' @importFrom magrittr %>%
#' @importFrom tidyselect all_of
#'
#' @export
#'
#' @details This is a standalone function used to replace the internal SWMPrExtension sampling sites data table used by the get_sites.R function. It reads a csv-formatted file of all NERRS SWMP stations that the user has downloaded from the CDMO SWMP station website: \url{https://cdmo.baruch.sc.edu/data/swmp-stations/}.
#'
#' This downloaded csv-formatted file and its location are the only input arguments to \code{update_sampling_station()}. The current station information is loaded from \code{data/sampling_stations.rda}, a copy of which is written out as \code{data/sampling_stations_backup.rda}, and the new data file is read, formatted appropriately, and written out as \code{data/sampling_stations.rda}.
#'
#'Note: This function need only be run when new reserves are added, stations are moved, etc.

#'
#' @author Dave Eslinger
#'
#' @return Returns TRUE on a successful run, FALSE on a failure.
#'
#' @concept data maintenance
#'
#' @examples
#' # Provide a bad file name to get error message
#' x <- update_sampling_stations("data","bad_file_name.csv")
#' print(x)
#'
#'

update_sampling_stations <- function(file_path = "inst/extdata",
                                     file_name = "sampling_stations.csv") {


  # Check that file exists
  new_file <- paste(file_path, file_name, sep = "/")

  if(file.exists(new_file)){  # Update file exists, so continue
    #
    # Read in existing data
    #
    load("data/sampling_stations.rda")
    # # Create backup of .rda file.
    #
    sampling_stations_backup <- sampling_stations
    save(sampling_stations_backup, file = "data/sampling_stations_backup.rda")

    # Read in new information
    new_stns <- read.csv(new_file) %>%
      select(-"Lat.Long")  %>%             # Drop unneeded column
      mutate(across(everything(), trimws), # trim white spaces
             State = tolower(.data$State),       # fix abbrevs
             across(c(.data$Latitude, .data$Longitude,
                      .data$GMT.Offset), as.double),
             across(c(.data$Station.Type, .data$Region,
                      .data$Row), as.integer))



    # Check columns
    #
    # Get column names and compare
    cnam_new <- colnames(new_stns)
    cnam_old <- colnames(sampling_stations_backup)
    same_vars <- cnam_new == cnam_old

    # Identify different columns (should only be last one)
    new_mismatch_vars <- cnam_new[!same_vars]
    old_mismatch_vars <- cnam_old[!same_vars]

    if(length(new_mismatch_vars) != 1 | length(old_mismatch_vars) != 1) {
      warning("New csv file has columns which differ from the old file by more than the single one that is expected.  Check columns manually to verify. \nNo update made.")
    } else {
      # drop missing columns
      new_stns1 <- select(new_stns, -all_of(new_mismatch_vars))
      sampling_stations_backup1 <- select(sampling_stations_backup, -all_of(old_mismatch_vars))

      # Append missing color column to new data
      #
      sampling_stations <- new_stns1 %>%
        mutate(color = case_when(
          Station.Type == 0 ~ "#BF9000",
          Station.Type == 1 ~ "#444E65",
          Station.Type == 2 ~ "#444E65"
        ))

      # # Write out new rda file.
      #
      save(sampling_stations, file = "data/sampling_stations.rda")
      print("Sampling stations successfully updated.")
      return(TRUE)
    }
  } else {
    warning(paste0("File/path ", new_file," does not exist."))
    return(FALSE)
  }
}
