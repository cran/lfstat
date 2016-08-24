<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->
lfstat 0.9.4
============

New Functionality
-----------------

-   new function `ev_return_period()` for computing return periods of flow quantiles

-   the start of a hydrogical year can now be set and retrieved for individual objects with `hyear_start()`

-   discharge units of a time series can be set and retrieved with `flowunit()`

Changes
-------

-   fixing imports/exports for reverse dependency RcmdrPlugin.lfstat

-   correcting internal data sets

-   improve creation and coercion to class lfobj

-   `tyears()` and `tyearsn()` now have a default argument for `dist`

-   `find_droughts()` accepts threshold quantiles as character strings

-   `summary.lfobj` is more verbose

-   `MAM()` issues a warning in case of to few observations
