This patch fixes issues in tests exposed by M1 mac causing `R CMD check` failure.
Asked to fix by CRAN before 2021-07-18.

## Test environments
* ubuntu 21.04 (local), R 4.1.0
* ubuntu 20.04 (github actions), R release and devel
* macos (github actions), R release
* windows (win-builder), R release and devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Downstream dependencies

I have run R CMD CHECK on downstream dependencies of iNZightTools, and all passed.
