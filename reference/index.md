# Package index

## Testing

Functions for testing a network statistic against random networks,
networks with the same degrees, or permutations of the network, and for
testing the parameters and the fit of a model:

- [`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md)
  [`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md)
  : Tests of model fit
- [`test_random()`](https://stocnet.github.io/infernet/reference/test_measures.md)
  [`test_configuration()`](https://stocnet.github.io/infernet/reference/test_measures.md)
  [`test_permutation()`](https://stocnet.github.io/infernet/reference/test_measures.md)
  : Tests of network measures
- [`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)
  [`test_time()`](https://stocnet.github.io/infernet/reference/test_parameter.md)
  : Tests of a model's parameters

## Specifying

The terms of an MRQAP formula and what each measures, the terms that
suit a network, and functions for reading one formula for an MRQAP or an
ERGM, and for seeing what each model makes of each term:

- [`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
  : Terms of an MRQAP formula
- [`formula(`*`<specify_terms>`*`)`](https://stocnet.github.io/infernet/reference/specify_terms.md)
  [`tidy(`*`<specify_terms>`*`)`](https://stocnet.github.io/infernet/reference/specify_terms.md)
  : The terms that suit a network
- [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify.md)
  [`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify.md)
  [`tidy(`*`<mrqap_formula>`*`)`](https://stocnet.github.io/infernet/reference/specify.md)
  [`tidy(`*`<ergm_formula>`*`)`](https://stocnet.github.io/infernet/reference/specify.md)
  : Specify a model from a formula written for many models

## Regressing

Functions for regressing a network on nodal and dyadic covariates by
multiple regression quadratic assignment procedure (MRQAP), with random
or fixed effects, missing data, panels, and cognitive social structures:

- [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  [`print(`*`<mrqap_control>`*`)`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  : Controls for an MRQAP
- [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/regression.md)
  : Regression for network data

## Diffusing

Functions for simulating many diffusions on a network, to test a model
of diffusion against:

- [`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
  [`summary(`*`<diffs_model>`*`)`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
  [`print(`*`<diffs_model>`*`)`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
  : Simulating many diffusions

## Diagnosing

Functions for diagnosing model results:

- [`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  [`print(`*`<diagnose_confusion>`*`)`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  [`tidy(`*`<diagnose_confusion>`*`)`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  [`glance(`*`<diagnose_confusion>`*`)`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  : Diagnosing how well a model classifies ties
- [`predict(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/predict.md)
  [`predict(`*`<netlm>`*`)`](https://stocnet.github.io/infernet/reference/predict.md)
  [`predict(`*`<netlogit>`*`)`](https://stocnet.github.io/infernet/reference/predict.md)
  : Predicting from a network regression

## Reporting

Functions for reporting and reading the results of regressions and
tests, for tidying the results of other network models beside them, for
predicting from a regression, and for diagnosing how well it classifies
ties:

- [`print(`*`<netreg_lm>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`print(`*`<netreg_glm>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`print(`*`<netreg_css>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`print(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`tidy(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`summary(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`print(`*`<summary_netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`glance(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`glance(`*`<netreg_lm>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`glance(`*`<netreg_glm>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`glance(`*`<netreg_css>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`nobs(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`logLik(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`deviance(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`fitted(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`residuals(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`formula(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  [`terms(`*`<netreg>`*`)`](https://stocnet.github.io/infernet/reference/netreg_results.md)
  : Reporting network regression results
- [`tidy(`*`<network_test>`*`)`](https://stocnet.github.io/infernet/reference/report_tests.md)
  [`glance(`*`<network_test>`*`)`](https://stocnet.github.io/infernet/reference/report_tests.md)
  [`print(`*`<network_test>`*`)`](https://stocnet.github.io/infernet/reference/report_tests.md)
  : Reporting network test results
- [`tidy(`*`<netlm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`tidy(`*`<netlogit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`glance(`*`<netlm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`glance(`*`<netlogit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`print(`*`<netlm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`print(`*`<netlogit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`tidy(`*`<ergm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`glance(`*`<ergm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`tidy(`*`<sienaFit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`glance(`*`<sienaFit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  : Tidying the results of other network models
