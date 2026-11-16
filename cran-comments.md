## Submission

This is a new submission. It replaces version 0.6.1, which waits in the
newbies queue. CRAN advised us to submit now if we planned significant
changes within 10 days.

Version 0.7.0 needs 'manynet' 2.5.0, which CRAN published on 15 November 2026.
'netrics' 1.2.0 and 'autograph' 1.3.0, which also build on 'manynet' 2.5.0,
are on CRAN from 16 November 2026.

## Main changes since 0.6.1

* `estimate_mrqap()` and the `test_*()` functions count the observed
  statistic as one of the draws when they calculate p-values.
* New functions to set coefficients (`declare_coefs()`), simulate networks
  from a fitted MRQAP (`simulate()`), compare nested fits (`anova()`),
  and find outliers in a fit (`diagnose_outliers()`).
* New `specify_saom()`, which writes a formula for `RSiena::siena()`.
  'RSiena' is in Suggests, and the function checks for it with
  `requireNamespace()`.
* New computation controls (`control_computation()`,
  `set_control_computation()`) for the seed, the number of cores, and the
  parallel strategy.
* New `sweep_diffusion()` and `sweep_opinion()`.
* Updated and new tutorials.

## Test environments

* local R installation, aarch64-apple-darwin23, R 4.6.1,
  with 'manynet' 2.5.0, 'netrics' 1.2.0, and 'autograph' 1.3.0
* macOS Tahoe 26 (on GitHub Actions), aarch64, R 4.6.2
* Microsoft Windows Server 2025 (on GitHub Actions), x86_64, R 4.6.2
* Ubuntu 26.04 (on GitHub Actions), x86_64, R 4.6.2

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes for reviewers

* The packages `ergm`, `fixest`, `lme4`, `MASS`, `pscl`, and `RSiena` are in
  Suggests. Each code path that needs one of them checks for it with
  `requireNamespace()`.
* The help page `diagnose_time` has no examples. It documents the S3
  generics `diagnose_changepoints()` and `diagnose_onset()`, which 'infernet'
  holds so that 'goldfish' and 'RSiena' can share them and register methods
  for their own fits. 'infernet' has no method for them yet, and their only
  method is a default that gives an error, so an example cannot show a
  result. The help page explains this.
* Each example runs in less than 5 s.
* With two processes, the tests take 34 s (61 s of CPU time).
  Tests that take longer run only where `INFERNET_SLOW_TESTS=true`.
