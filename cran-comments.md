Resubmission: (for the first time on CRAN)

## Addressed the following issues flagged by first submission

The first submission in December 2025 was rejected, citiing several reasons, which we have now addressed.

* Package size: The CMD CHECK now generates no warnings about the package size exceeding 5MB.  
* References: We now include references to all sources, including DOIs, as requested.  

## R CMD check results

0 errors | 0 warnings | 2 notes

### Notes

1. **CRAN incoming feasibility**: Found one URL that redirects:
   - `https://github.com/SeraphineM/quallmer.app` redirects to `https://github.com/quallmer/quallmer.app`
   - This is documented in NEWS.md as the package repository was moved to the quallmer organization
   - The URL returns 200 OK and works correctly

2. **HTML version of manual**: Skipping HTML validation due to local HTML Tidy version
   - This is a local environment issue and does not affect the package itself
   - CRAN's check systems have appropriate HTML Tidy versions

## Test environments

* Local macOS R 4.5.2: `devtools::check(--as-cran)` — OK
* devtools::check_win_release()
* devtools::check_win_oldrelease()
* devtools::check_win_devel()
* GitHub actions checking on several flavours of Linux and macOS


