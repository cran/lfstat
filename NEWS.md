<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->
lfstat 0.8.0
============

New Functionality
-----------------

-   new function `evfit()` for fitting of extreme value distributions

-   offering a "reversed" GEV suited for minima

-   providing wrappers `{pel,qua,cdf}_ev` around functions from lmom

-   new function `evquantile()` for estimating quantiles of an ev-distribution

-   several new functions for annotating plots like `trace_value()`, `rpline()`

-   new function for calculating the `gringorten()` plotting position

-   offering functions `pool_{ic,ma,sp}` for pooling of droughts

-   `vary_threshold()` for daily, weekly, monthly, seasonal and year varying thresholds

-   new function `tyearsS` for estimating deficit volume and deficit duration quantiles of dry spells

-   streamflow hydrographs can now be stored as objects of class xts, including methods for conversion `as.xts.lfobj()`

-   calculation of the `water_year()` for a give time

Changes
-------

-   speeding up function `baseflow()` by using matrix algebra

-   replacing function `streamdef()` with `find_droughts()`

-   vectorizing of function `tyears()` for estimating flow quantiles for given return periods

-   the start of the hydrological year is now stored as an attribute, can be retrieved via `hyear_start`
