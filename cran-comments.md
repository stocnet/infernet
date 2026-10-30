## Submission

This is a first submission of `infernet` to CRAN.

## Test environments

* local R installation, aarch64-apple-darwin20, R 4.6.1
* macOS (on Github Actions), R release
* Microsoft Windows Server 2022 (on Github Actions), R release
* Ubuntu 24.04 (on Github Actions), R release

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes for reviewers

* The package Depends on `manynet` and `netrics`, both on CRAN.
* The packages `ergm`, `fixest`, `lme4`, `MASS`, and `pscl` are in Suggests.
  Each code path that needs one of them checks for it with `requireNamespace()`.
* Examples keep the number of permutations low. The slowest takes about 2 s.
* The tests take about 26 s on the two processes that DESCRIPTION asks for.
  Tests that take longer run only where `INFERNET_SLOW_TESTS=true`.
