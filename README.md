![](swmprExtension_logo.png)

<!-- [![Travis-CI Build Status](https://travis-ci.org/LimnoTech/SWMPrExtension.svg?branch=master)](https://travis-ci.org/LimnoTech/SWMPrExtension)-->

<!-- Start badges -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/SWMPrExtension)](https://cran.r-project.org/package=SWMPrExtension)[![Downloads
from the RStudio CRAN
mirror](http://cranlogs.r-pkg.org/badges/grand-total/SWMPrExtension)](http://cran.rstudio.com/package=SWMPrExtension)
<!-- End badges -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

The System Wide Monitoring Program
([SWMP](http://nerrs.noaa.gov/RCDefault.aspx?ID=18)) was implemented by
the National Estuarine Research Reserve System
([NERRS](http://nerrs.noaa.gov/)) in 1995 to provide continuous
monitoring data at over 140 continuous monitoring stations in 28
estuaries across the United States. SWMPrExtension (pronounced “swamper
extension”) is an R package that provides additional functions to
organize and analyze SWMP data and is intended as a companion package
for [SWMPr](https://github.com/fawda123/SWMPr) (pronounced “swamper”).
Currently, there is no citation for SWMPrExtension.

[SWMPr](https://github.com/fawda123/SWMPr) is an R package for
retrieving, organizing, and analyzing estuary monitoring data from SWMP.
SWMPr can be cited as follows:

*Beck MW. 2016. SWMPr: An R package for retrieving, organizing, and
analyzing environmental data for estuaries. The R Journal. 8(1):219-232.
<https://journal.r-project.org/archive/2016-1/beck.pdf>*

# NOAA Open Source Disclaimer

This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ?as is? basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.

# Installing the package

Install the package from CRAN:

``` r
install.packages('SWMPrExtension')
library(SWMPrExtension)
```

Install the development (unstable) version from Github:

``` r
install.packages('devtools')
library(devtools)
install_github('NOAA-OCM/SWMPrExtension')
library(SWMPrExtension)
```

# Using the package

Documentation for SWMPrExtension is currently in development.

A quick summary of the SWMPr package can be found
[here](https://github.com/fawda123/SWMPr). A detailed manuscript
describing full use of the SWMPr package is available from the [R
Journal](https://journal.r-project.org/archive/accepted/beck.pdf). All
source materials for the manuscript are available
[here](https://github.com/fawda123/swmpr_manu).

SWMPrExtension adds several functions to existing concepts in SWMPr and
introduces a new concept called “Reporting”.

<h3>

Analyze

</h3>

The core analyses available within the SWMPrExtension R package and used
to create the reserve level annual report fall into four general
categories: boxplots and barplots, range plots, threshold plots, and
trend plots. There is also an additional category called “mapping” which
contains functions that generate many of maps associated with the
reserve-level reports. The analyses, methods, and maps in this section
were selected by and approved by a technical advisory committee composed
of NERR research coordinators, SWMP technicians and CDMO staff.

<h4>

Core Analyses

</h4>

<table>

<tr>

<td>

<code>annual\_range.swmpr</code>

</td>

<td>

For a user-specified year, calculate averages, average ranges, and
min/max observed ranges the on a monthly or seasonal basis.

</td>

</tr>

<tr>

<td>

<code>historical\_daily\_range.swmpr</code>

</td>

<td>

Compare a user-specified year against historical data on a daily basis.

</td>

</tr>

<tr>

<td>

<code>historical\_range.swmpr</code>

</td>

<td>

Compare a user-specified year against historical data on a
monthly/seasonal basis.

</td>

</tr>

<tr>

<td>

<code>raw\_boxplot.swmpr</code>

</td>

<td>

Generate a monthly/seasonal boxplots of raw data for a target year.

</td>

</tr>

<tr>

<td>

<code>seasonal\_barplot.swmpr</code>

</td>

<td>

Generate monthly/seasonal barplot for parameters that are better viewed
in on a cumulative basis (e.g. precipitation).

</td>

</tr>

<tr>

<td>

<code>seasonal\_boxplot.swmpr</code>

</td>

<td>

Generate monthly/seasonal boxplots for daily average statistics
(min/average/max) across a user-specified time period.Includes the
option to calculate a median value for a target year and include a line
for a water quality threshold.

</td>

</tr>

<tr>

<td>

<code>seasonal\_dot.swmpr</code>

</td>

<td>

Plot average/min/max seasonal values faceted by season.

</td>

</tr>

<tr>

<td>

<code>sk\_seasonal.swmpr</code>

</td>

<td>

Seasonal Kendall non-parametric test for monotonic seasonal trends.

</td>

</tr>

<tr>

<td>

<code>threshold\_criteria\_plot.swmpr</code>

</td>

<td>

Compare raw data against user-specified water quality thresholds

</td>

</tr>

<tr>

<td>

<code>threshold\_identification.swmpr</code>

</td>

<td>

Identify dates and times that a user-specified water quality threshold
is exceeded. For continuous monitoring data, the user can also specify
the length of time the threshold must be exceeded for the event to be
included (e.g. DO must be \< 2 mg/L for at least 2 hours).

</td>

</tr>

<tr>

<td>

<code>threshold\_percentile\_plot.swmpr</code>

</td>

<td>

Compare raw data against user-specified percentiles calculated from
historical data. User has the option to calculate percentiles on a
monthly basis.

</td>

</tr>

<tr>

<td>

<code>threshold\_summary.swmpr</code>

</td>

<td>

Summarize the results from <code>threshold\_identification</code> in
either a plot or tabular format. Results can be aggregated on a monthly,
seasonal, or annual basis.

</td>

</tr>

</table>

<h4>

Mapping

</h4>

<table>

<tr>

<td>

<code>national\_sk\_map</code>

</td>

<td>

Create a base map for NERRS reserves in ggplot with seasonal kendall
results.

</td>

</tr>

<tr>

<td>

<code>res\_custom\_map</code>

</td>

<td>

Create a stylized reserve-level map of custom station locations for use
with the reserve level reporting template.

</td>

</tr>

<tr>

<td>

<code>res\_custom\_sk\_map</code>

</td>

<td>

Create a stylized reserve-level map of seasonal kendall results from
custom station locations for use with the reserve level reporting
template.

</td>

</tr>

<tr>

<td>

<code>res\_local\_map</code>

</td>

<td>

Create a stylized reserve-level map for use with the reserve level
reporting template.

</td>

</tr>

<tr>

<td>

<code>res\_national\_map</code>

</td>

<td>

Create a base map for NERRS reserves in ggplot.

</td>

</tr>

<tr>

<td>

<code>res\_sk\_map</code>

</td>

<td>

Create a stylized reserve-level map of seasonal kendall results for use
with the reserve level reporting template.

</td>

</tr>

</table>

<h3>

Retrieve

</h3>

in the <code>SWMPr</code> package, retrieve functions help the user load
SWMP data into R. </br>

<table>

<tr>

<td>

<code>import\_local\_nut</code>

</td>

<td>

A modified version of <code>import\_local</code> from the
<code>SWMPr</code> package. This version allows the user to specify the
collection method (<code>CollMethd</code>) argument to separate monthly
nutrient sampling data from monthly 24-hr nutrient sampling data.

</td>

</tr>

</table>

<h3>

Reporting

</h3>

The new concept of reporting refers to functions that were specifically
developed for use with NERRS reserve-level & national-level reporting
scripts that are used to generate the reserve-level and national-level
annual reports. They are included as part of this package in case users
find them useful for their own purposes.

<h4>

Reserve Level Template

</h4>

<table>

<tr>

<td>

<code>create\_sk\_flextable\_list</code>

</td>

<td>

Create a list of <code>flextable</code> objects to display Seasonal
Kendall results in the NERRS reserve level template.

</td>

</tr>

<tr>

<td>

<code>geographic\_unique\_stations</code>

</td>

<td>

Creates an alphabetically sorted, vector of geographically unique
stations for mapping. Intended for use with
<code>res\_local\_map</code>.

</td>

</tr>

<tr>

<td>

<code>get\_reserve</code>

</td>

<td>

Return the full name of the reserve associated with the data files in
the ‘data’ folder of the reserve level template.

</td>

</tr>

<tr>

<td>

<code>get\_shp\_name</code>

</td>

<td>

Return the name of the shape file associated with the data files in the
‘data’ folder of the reserve level template.

</td>

</tr>

<tr>

<td>

<code>get\_site\_code</code>

</td>

<td>

Return the 3 letter reserve code associated with the reserve.

</td>

</tr>

<tr>

<td>

<code>get\_site\_coordinates</code>

</td>

<td>

Return the station coordinates for stations associated with the data
files in the ‘data’ folder of the reserve level template.

</td>

</tr>

<tr>

<td>

<code>get\_sites</code>

</td>

<td>

Return the stations associated with the data files in ‘data’ folder of
the reserve level template.

</td>

</tr>

<tr>

<td>

<code>load\_shp\_file</code>

</td>

<td>

Load and format shapefile for use with res\_local\_map.

</td>

</tr>

</table>

<h4>

National Level Template

</h4>

<table>

<tr>

<td>

<code>create\_sk\_national\_ft\_reserves</code>

</td>

<td>

Create a <code>flextable</code> of reserve names for use with the NERRS
national level template.

</td>

</tr>

<tr>

<td>

<code>create\_sk\_national\_ft\_results</code>

</td>

<td>

Create a <code>flextable</code> object to display Seasonal Kendall
results for each reserve in the NERRS national level template.

</td>

</tr>

<tr>

<td>

<code>summarise\_handoff\_files</code>

</td>

<td>

Summarise the seasonal kendall results from reserve level report
hand-off files.

</td>

</tr>

</table>

<h3>

Miscellaneous

</h3>

Miscellaneous functions are generally helper functions that are called
internally by other analysis functions.

<h4>

Analyze

</h4>

<table>

<tr>

<td>

<code>assign\_season</code>

</td>

<td>

Assign seasons to SWMPr sampling data on a monthly basis or user-defined
basis. Used by multiple analysis functions to group sampling data into
user-defined seasons.

</td>

</tr>

<tr>

<td>

<code>lm\_p\_labs</code>

</td>

<td>

Generate a dataframe of p-value labels based on p-values from linear
regression. Used internally by <code>seasonal\_dot.swmpr</code> to add
linear regression results to the plot.

</td>

</tr>

<tr>

<td>

<code>set\_date\_break\_labs</code>

</td>

<td>

Select reasonable labels for breaks used in
<code>scale\_x\_datetime</code>. Used internally by several analysis
functions.

</td>

</tr>

<tr>

<td>

<code>set\_date\_breaks</code>

</td>

<td>

A helper function to select reasonable breaks for
<code>scale\_x\_datetime</code>. Used internally by several analysis
functions.

</td>

</tr>

<tr>

<td>

<code>sk\_tidy</code>

</td>

<td>

Tidy results from <code>EnvStats::kendallSeasonalTrendTest</code>. Used
by <code>sk\_seasonal.swmpr</code> to tidy seasonal kendall test
results.

</td>

</tr>

<tr>

<td>

<code>std\_param\_check</code>

</td>

<td>

Determine if a parameter is one of the standard SWMP parameters.

</td>

</tr>

<tr>

<td>

<code>title\_labeler</code>

</td>

<td>

Generate a plot title based on SWMP station abbreviation. Used
internally by several analysis functions.

</td>

</tr>

<tr>

<td>

<code>y\_count\_labeler</code>

</td>

<td>

Generate a y-axis label based on SWMP parameter abbreviation. Used
internally by several analysis functions.

</td>

</tr>

<tr>

<td>

<code>y\_labeler</code>

</td>

<td>

Generate a y-axis label based on SWMP parameter abbreviation and
threshold criteria. Used internally by several analysis functions.

</td>

</tr>

</table>

<h4>

Mapping

</h4>

<table>

<tr>

<td>

<code>reserve\_locs</code>

</td>

<td>

Create a dataframe of selected NERRS locations for plotting with
<code>res\_national\_map</code>.

</td>

</tr>

</table>

<h4>

Reporting

</h4>

<table>

<tr>

<td>

<code>ft\_col\_names</code>

</td>

<td>

Convert <code>SWMPr</code> parameter abbreviations into formats
appropriate for use with NERRS reserve level template
<code>flextable</code>. Used internally by
<code>create\_sk\_flextable\_list</code>.

</td>

</tr>

<tr>

<td>

<code>generate\_results\_table</code>

</td>

<td>

Filters a <code>data.frame</code> of user-specified results for display
in the NERRS reserve level report. Used internally by
<code>create\_sk\_flextable\_list</code>.

</td>

</tr>

<tr>

<td>

<code>generate\_station\_table</code>

</td>

<td>

Filters a <code>data.frame</code> of user-specified results for display
in the NERRS reserve level report. Used internally by
<code>create\_sk\_flextable\_list</code>.

</td>

</tr>

<tr>

<td>

<code>remove\_inf\_and\_nan</code>

</td>

<td>

Removes <code>-Inf</code>, <code>Inf</code>, and <code>NaN</code> from a
matrix. Used internally by several functions to prevent plotting errors.

</td>

</tr>

</table>

<h3>

Datasets

</h3>

The following data sets are included within the
<code>SWMPrExtension</code> package. They are used for examples and for
mapping. </br>

<table>

<tr>

<td>

<code>cbm\_spatial</code>

</td>

<td>

A shapefile of the reserve boundary for Chesapeake Bay-Maryland NERR.
Available from: <code><http://cdmo.baruch.sc.edu/></code>.

</td>

</tr>

<tr>

<td>

<code>elk\_spatial</code>

</td>

<td>

A shapefile of the reserve boundary for Elkhorn Slough NERR. Available
from: <code><http://cdmo.baruch.sc.edu/></code>.

</td>

</tr>

<tr>

<td>

<code>elknmnut</code>

</td>

<td>

Nutrient data (2007-2016) from North Marsh station at Elkhorn Slough
NERR.

</td>

</tr>

<tr>

<td>

<code>elksmwq</code>

</td>

<td>

Water quality data (2007-2016) from South Marsh station at Elkhorn
Slough NERR.

</td>

</tr>

<tr>

<td>

<code>sampling\_stations</code>

</td>

<td>

Metadata on NERRS stations provided by the Central Data Management
Office (CDMO) when data is downloaded. Additional formatting applied in
order to make the dataset useful to several <code>SWMPrExtension</code>
functions.

</td>

</tr>

<tr>

<td>

<code>us\_laea</code>

</td>

<td>

US County boundaries from the US Census Bureau’s MAF/TIGER geographic
database, modified to remove non-ASCII characters. Reprojected using
Lambert Azimuthal Equal Area. Used to create national-level maps.

</td>

</tr>

</table>
