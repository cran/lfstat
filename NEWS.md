<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->
lfstat 0.9.0
============

New Functionality
-----------------

-   new function `apply.seasonal()`, eg for computing seasonal extreme values

-   drought summaries now report the days below threshold (dbt), the minimum discharge (qmin) as well as the date of the minimum discharge (tqmin)

-   Support for vardat2 file format: `read.vardat2()`

-   new function `fill_na()` for interpolating short gaps in a time series

-   discharge units of a time series can be set and retrieved with `flowunit()`

Changes
-------

-   bugfixes in `summary.deficit()`

-   being more relaxed when converting an lfobj to xts via `as.xts.lfobj()`. If there is no unit stored in the attributes "m^3/s" is assumed and a warning is given.
