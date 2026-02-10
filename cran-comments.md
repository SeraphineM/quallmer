Resubmission: (for the first time on CRAN)

## Addressed the following issues flagged by first submission

The first submission in December 2025 was rejected, citiing several reasons, which we have now addressed.

* Package size: The CMD CHECK now generates no warnings about the package size exceeding 5MB.  
* References: We now include references to all sources, including DOIs, as requested.  

## R CMD check results

0 errors | 0 warnings | 1 notes

### Notes

**CRAN incoming feasibility**: Found URLs with non-standard status codes:
    - `https://dataverse.harvard.edu/dataverse/sfm` returns status 202 (Accepted)
    - This is a valid HTTP status code used by Harvard Dataverse for asynchronous content processing
    - The URL is accessible and working correctly

## Test environments

* Local macOS R 4.5.2: `devtools::check(--as-cran)` — OK
* devtools::check_win_release()
* devtools::check_win_oldrelease()
* devtools::check_win_devel()
* GitHub actions checking on several flavours of Linux and macOS


