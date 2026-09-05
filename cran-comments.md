## Submission

This is a first submission of `infernet` to CRAN.

## Test environments

* local R installation, aarch64-apple-darwin20, R 4.6.1
* macOS (on Github Actions), R release
* Microsoft Windows Server 2022 (on Github Actions), R release
* Ubuntu 24.04 (on Github Actions), R release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes for reviewers

* The package Depends on `manynet` and `netrics`, both on CRAN.
* All estimator-specific packages (`lme4`, `glmmTMB`, `fixest`, `gmm`, `MASS`,
  `pscl`, `nnet`, `torch`) are in Suggests, and every code path that needs one
  guards with `requireNamespace()`.
* Examples keep the number of permutations low so that they run quickly.
