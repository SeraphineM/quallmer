## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* NOTE: The dependency ‘ellmer’ on my local machine shows a minor version mismatch warning (“built under R 4.5.2”). This does not occur on CRAN since dependencies are built from source there.

## Test environments
* Local macOS R 4.5.x: `devtools::check(--as-cran)` — OK

## Additional comments
* Attempts to run devtools::check_win_devel() failed with an FTP 550 error on win-builder.r-project.org during upload. This appears to be a temporary server-side issue; local and R-hub checks are clean.
