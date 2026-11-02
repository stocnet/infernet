## Resubmission

This is a resubmission of a first submission.
The incoming checks of version 0.6.0 reported that the examples of `test_fit`
took longer than the limit.
This version reduces the number of simulations in those examples.

## Test environments

* local R installation, aarch64-apple-darwin20, R 4.6.1
* macOS (on Github Actions), R release
* Microsoft Windows Server 2022 (on Github Actions), R release
* Ubuntu 24.04 (on Github Actions), R release
* CRAN incoming checks of version 0.6.0: Windows Server 2022 and Debian, R-devel

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.
* The incoming checks also list possibly misspelled words in DESCRIPTION.
  Dekker, Krackhardt, and Snijders are the authors of the cited references.
  MRQAP is the abbreviation of the method, which the Description defines.

## Notes for reviewers

* The package Depends on `manynet` and `netrics`, both on CRAN.
* The packages `ergm`, `fixest`, `lme4`, `MASS`, and `pscl` are in Suggests.
  Each code path that needs one of them checks for it with `requireNamespace()`.
* Examples keep the number of permutations low.
* The tests took 146 s on Windows and 59 s on Debian in the incoming checks of
  version 0.6.0. Tests that take longer run only where `INFERNET_SLOW_TESTS=true`.
