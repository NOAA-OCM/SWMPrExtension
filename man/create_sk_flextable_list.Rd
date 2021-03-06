% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_sk_flextable_list.R
\name{create_sk_flextable_list}
\alias{create_sk_flextable_list}
\title{Create a List of Flextable Objects}
\usage{
create_sk_flextable_list(
  sk_result,
  stations,
  param,
  trend_col = c("#247BA0", "#A3DFFF", "#D9D9D9", "white"),
  font_col_default = "#444E65",
  font_sz_stn = 6,
  font_sz_result = 12,
  font_sz_head = 6,
  ht_head = 0.28,
  ht_body = 0.202,
  is_swmp = TRUE,
  stn_name = NULL,
  stn_abbrev = NULL,
  par_name = NULL
)
}
\arguments{
\item{sk_result}{a \code{data.frame} of reformatted results from \code{\link{sk_seasonal}}}

\item{stations}{chr, vector of stations to be displayed}

\item{param}{chr, vector of parameters to be displayed}

\item{trend_col}{chr, a four element vector that specifies colors for increasing, decreasing, no change, and insufficient data trends}

\item{font_col_default}{chr, default color to be used for trend table}

\item{font_sz_stn}{int, specify the font size of displayed station names}

\item{font_sz_result}{int, specify the font size of the displayed results}

\item{font_sz_head}{int, specify the font size of the table header row}

\item{ht_head}{num, specify the cell height of the table body rows. Units for this parameter are in inches.}

\item{ht_body}{num, specify the cell height of the table header row. Units for this parameter are in inches.}

\item{is_swmp}{logical, are the station names and parameter names consistent with SWMP station and parameter names? If either of these conditions is false then this parameter should be set to \code{FALSE} and then the user should define \code{stn_name}, \code{stn_abbrev}, and \code{par_name}. default is \code{TRUE}.}

\item{stn_name}{chr, a list of full station names that the user would like to add to the trend table (e.g., "Cat Point")}

\item{stn_abbrev}{chr, a list of station abbreviations that the user would like to add to the trend table (e.g., "CP" as an abbreviation for Cat Point).}

\item{par_name}{chr, a list of parameter names to be used if the names to not match standard CDMO parameters.}
}
\value{
Returns a list of \code{\link[flextable]{flextable}} objects
}
\description{
Create a list of flextable objects to display Seasonal Kendall results in the NERRS reserve level template
}
\details{
This function is intended for internal use with the NERRS reserve level reporting scripts. Using the results from the reserve level trend analysis, \code{create_sk_flextable_list} creates a list of two \code{flextable} objects to be displayed in the NERRS reserve level template. The first \code{flextable} in the list contains the two-letter station IDs for each station and the full location name of each station. The second table lists the seasonal kendall results and the names of the parameters of interest.
}
\author{
Julie Padilla
}
\concept{reporting}
