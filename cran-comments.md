## Test environments

- ubuntu 22.04 (local), R 4.2.3
- ubuntu 20.04 (github actions), R oldrel, release, and devel
- macos (github actions), R 4.2
- windows (win-builder), R release and devel

## R CMD check results

0 errors | 0 warnings | 2 notes

- There are two (related) NOTEs about a non-CRAN dependency - this package 'surveyspec' is only used conditionally, providing additional but not essential functionality. 'Additional_repositories' points to a repository where 'surveyspec' can be found.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

- We saw 0 new problems
- We failed to check 0 packages

## Notes

This release fixes a bug in a function triggered by the latest R-devel.
