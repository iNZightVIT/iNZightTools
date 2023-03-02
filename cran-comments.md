## Test environments

- ubuntu 22.04 (local), R 4.2.2
- ubuntu 20.04 (github actions), R 4.1, release, and devel
- macos (github actions), R release
- windows (win-builder), R release and devel

## R CMD check results

0 errors | 0 warnings | 1 notes

- There are two (related) NOTEs about a non-CRAN dependency - this package 'surveyspec' is only used conditionally, providing additional but not essential functionality. 'Additional_repositories' points to a repository where 'surveyspec' can be found.

## revdepcheck results

We checked 2 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

- We saw 0 new problems
- We failed to check 0 packages
