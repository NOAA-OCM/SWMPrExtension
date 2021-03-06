% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/historical_range.R
\name{historical_range}
\alias{historical_range}
\alias{historical_range.swmpr}
\title{Historical Monthly/Seasonal Range Timeseries}
\usage{
historical_range(swmpr_in, ...)

\method{historical_range}{swmpr}(
  swmpr_in,
  param = NULL,
  hist_rng = NULL,
  target_yr = NULL,
  criteria = NULL,
  free_y = FALSE,
  log_trans = FALSE,
  converted = FALSE,
  criteria_lab = "WQ Threshold",
  plot_title = FALSE,
  plot = TRUE,
  ...
)
}
\arguments{
\item{swmpr_in}{input swmpr object}

\item{...}{additional arguments passed to other methods. See \code{\link{assign_season}}}

\item{param}{chr string of variable to plot}

\item{hist_rng}{numeric vector, if historic range is not specified then the min/max values of the data set will be used.}

\item{target_yr}{numeric, the target year that should be compared against the historic range. If target year is not specified then dot will not be plotted}

\item{criteria}{numeric, a numeric criteria that will be plotted as a horizontal line}

\item{free_y}{logical, should the y-axis be free? Defaults to \code{FALSE}. If \code{FALSE}, defaults to zero, unless negative values are present. If \code{TRUE}, y-axis limits are selected by \code{ggplot}}

\item{log_trans}{logical, should y-axis be log? Defaults to \code{FALSE}}

\item{converted}{logical, were the units converted from the original units used by CDMO? Defaults to \code{FALSE}. See \code{y_labeler} for details.}

\item{criteria_lab}{chr, label for the threshold criteria defined in \code{criteria}. Defaults to "WQ Threshold"}

\item{plot_title}{logical, should the station name be included as the plot title? Defaults to \code{FALSE}}

\item{plot}{logical, should a plot be returned? Defaults to \code{TRUE}}
}
\value{
Returns a \code{\link[ggplot2]{ggplot}} object
}
\description{
Compare seasonal averages/minimums/maximums for a target year to historical seasonal averages/minimums/maximums
}
\details{
This function summarizes average daily values and average daily minimums/maximums across user-defined seasons for a target year (\code{target_yr}) and for a historical range (\code{hist_rng}). If \code{hist_rng} is not specified then the minimum and maximum years within the data set will be used. If \code{target_yr} is not specified then only the results for the \code{hist_rng} will be returned.

The user also has the option to add a threshold hold line using the \code{criteria} argument. Typically, this value is a water quality threshold, which is why \code{criteria_lab} defaults to \code{'WQ Threshold'}. However, the user has the option to specify any other type of threshold they wish. when doing so, the value for \code{criteria_lab} should be changed accordingly.
}
\examples{
\dontshow{
data(apacpwq)

dat <- qaqc(apacpwq, qaqc_keep = c('0', '3', '5'))

x <- historical_range(dat, param = 'do_mgl')
}

\donttest{
data(elksmwq)

dat <- qaqc(elksmwq, qaqc_keep = c('0', '3', '5'))
# with criteria
y <- historical_range(dat, param = 'do_mgl', target_yr = 2013, criteria = 2)

# w/o criteria
z <- historical_range(dat, param = 'do_mgl', target_yr = 2013)

# add a y label
zz <- z + labs(x = NULL, y = "Dissolved Oxygen (mg/L)")
}
}
\seealso{
\code{\link[ggplot2]{ggplot}}, \code{\link{assign_season}}, \code{\link{y_labeler}}
}
\author{
Julie Padilla, Kimberly Cressman
}
\concept{analyze}
