# Package index

## Testing

Functions for testing one network statistic against random networks,
networks with the same degrees, or permutations of the network:

- [`test_random()`](https://stocnet.github.io/infernet/reference/test_measures.md)
  [`test_configuration()`](https://stocnet.github.io/infernet/reference/test_measures.md)
  [`test_permutation()`](https://stocnet.github.io/infernet/reference/test_measures.md)
  [`tidy(`*`<network_test>`*`)`](https://stocnet.github.io/infernet/reference/test_measures.md)
  [`summary(`*`<network_test>`*`)`](https://stocnet.github.io/infernet/reference/test_measures.md)
  [`glance(`*`<network_test>`*`)`](https://stocnet.github.io/infernet/reference/test_measures.md)
  : Tests of network measures

## Specifying

Functions for reading one formula for an MRQAP or an ERGM, for listing
the terms that suit a network, the terms of an MRQAP formula and what
each measures, and the coefficients of a model:

- [`specify_ergm()`](https://stocnet.github.io/infernet/reference/specify_ergm.md)
  [`summary(`*`<ergm_formula>`*`)`](https://stocnet.github.io/infernet/reference/specify_ergm.md)
  [`tidy(`*`<ergm_formula>`*`)`](https://stocnet.github.io/infernet/reference/specify_ergm.md)
  : Specify an ERGM from a formula written for any model
- [`specify_mrqap()`](https://stocnet.github.io/infernet/reference/specify_mrqap.md)
  [`summary(`*`<mrqap_formula>`*`)`](https://stocnet.github.io/infernet/reference/specify_mrqap.md)
  [`tidy(`*`<mrqap_formula>`*`)`](https://stocnet.github.io/infernet/reference/specify_mrqap.md)
  : Specify an MRQAP from a formula written for any model
- [`specify_saom()`](https://stocnet.github.io/infernet/reference/specify_saom.md)
  [`summary(`*`<saom_formula>`*`)`](https://stocnet.github.io/infernet/reference/specify_saom.md)
  : Specify a SAOM from a formula written for any model
- [`formula(`*`<specify_terms>`*`)`](https://stocnet.github.io/infernet/reference/specify_terms.md)
  [`summary(`*`<specify_terms>`*`)`](https://stocnet.github.io/infernet/reference/specify_terms.md)
  [`tidy(`*`<specify_terms>`*`)`](https://stocnet.github.io/infernet/reference/specify_terms.md)
  : The terms that suit a network
- [`terms_mrqap`](https://stocnet.github.io/infernet/reference/terms_mrqap.md)
  : Terms of an MRQAP formula
- [`declare_coefs()`](https://stocnet.github.io/infernet/reference/declare_coefs.md)
  [`coef(`*`<infernetMrqapCoefs>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs.md)
  [`coef(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs.md)
  : Declare the coefficients of a model
- [`declare_coefs(`*`<ergm_formula>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md)
  [`declare_coefs(`*`<ergm>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md)
  [`coef(`*`<infernetErgmCoefs>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md)
  [`simulate(`*`<infernetErgmCoefs>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md)
  : Declare the coefficients of an ERGM

## Estimating

Functions for regressing a network on nodal and dyadic covariates by
multiple regression quadratic assignment procedure (MRQAP), with random
or fixed effects, missing data, panels, and cognitive social structures,
the controls of a model, and the tidy results of other network models:

- [`print(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
  [`estimate_mrqap()`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
  [`tidy(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
  [`summary(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
  [`glance(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/estimate_mrqap.md)
  : Regression for network data
- [`set_control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md)
  [`control_computation()`](https://stocnet.github.io/infernet/reference/control_computation.md)
  [`c(`*`<mrqap_control>`*`)`](https://stocnet.github.io/infernet/reference/control_computation.md)
  : Computation controls for the session or for one call
- [`control_mrqap()`](https://stocnet.github.io/infernet/reference/control_mrqap.md)
  : Controls for an MRQAP
- [`tidy(`*`<netlm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`tidy(`*`<netlogit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`glance(`*`<netlm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`glance(`*`<netlogit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`tidy(`*`<ergm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`glance(`*`<ergm>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`tidy(`*`<sienaFit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  [`glance(`*`<sienaFit>`*`)`](https://stocnet.github.io/infernet/reference/other_results.md)
  : Tidying the results of other network models

## Evaluating

Functions for diagnosing issues with a fitted model and its residuals,
for testing its fit against its simulations, for testing its
coefficients, and for comparing models by their likelihood:

- [`diagnose_confusion()`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  [`tidy(`*`<infernetConfusion>`*`)`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  [`glance(`*`<infernetConfusion>`*`)`](https://stocnet.github.io/infernet/reference/diagnose_confusion.md)
  : Diagnosing how well a model classifies ties
- [`diagnose_outliers()`](https://stocnet.github.io/infernet/reference/diagnose_outliers.md)
  : Finding the outliers of a fitted model
- [`fitted(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/diagnose_residuals.md)
  [`residuals(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/diagnose_residuals.md)
  : The fitted values and residuals of a network regression
- [`diagnose_changepoints()`](https://stocnet.github.io/infernet/reference/diagnose_time.md)
  [`diagnose_onset()`](https://stocnet.github.io/infernet/reference/diagnose_time.md)
  : Diagnosing a model of a sequence over time
- [`test_gof()`](https://stocnet.github.io/infernet/reference/test_fit.md)
  [`test_distributions()`](https://stocnet.github.io/infernet/reference/test_fit.md)
  : Tests of model fit
- [`test_parameter()`](https://stocnet.github.io/infernet/reference/test_parameter.md)
  [`anova(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/test_parameter.md)
  [`test_time()`](https://stocnet.github.io/infernet/reference/test_parameter.md)
  : Tests of a model's coefficients
- [`nobs(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md)
  [`logLik(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md)
  [`deviance(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/evaluate_likelihood.md)
  : Comparing network regressions by their likelihood

## Interpreting

Functions for predicting from a model and reading its terms, for
simulating networks from a model, for simulating many diffusions on a
network, and for sweeping a diffusion or an opinion dynamic over its
settings:

- [`predict(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/predict_mrqap.md)
  [`predict(`*`<netlm>`*`)`](https://stocnet.github.io/infernet/reference/predict_mrqap.md)
  [`predict(`*`<netlogit>`*`)`](https://stocnet.github.io/infernet/reference/predict_mrqap.md)
  [`formula(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/predict_mrqap.md)
  [`terms(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/predict_mrqap.md)
  : Predicting from a network regression, and its terms
- [`declare_coefs(`*`<ergm_formula>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md)
  [`declare_coefs(`*`<ergm>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md)
  [`coef(`*`<infernetErgmCoefs>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md)
  [`simulate(`*`<infernetErgmCoefs>`*`)`](https://stocnet.github.io/infernet/reference/declare_coefs_ergm.md)
  : Declare the coefficients of an ERGM
- [`simulate(`*`<infernetMrqapFit>`*`)`](https://stocnet.github.io/infernet/reference/simulate_mrqap.md)
  [`simulate(`*`<infernetMrqapCoefs>`*`)`](https://stocnet.github.io/infernet/reference/simulate_mrqap.md)
  [`simulate(`*`<mrqap_formula>`*`)`](https://stocnet.github.io/infernet/reference/simulate_mrqap.md)
  : Simulate networks from an MRQAP
- [`play_diffusions()`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
  [`summary(`*`<diffs_model>`*`)`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
  [`` `[`( ``*`<diffs_model>`*`)`](https://stocnet.github.io/infernet/reference/play_diffusions.md)
  : Simulating many diffusions
- [`sweep_diffusion()`](https://stocnet.github.io/infernet/reference/sweep.md)
  [`sweep_opinion()`](https://stocnet.github.io/infernet/reference/sweep.md)
  [`summary(`*`<diffusionSweep>`*`)`](https://stocnet.github.io/infernet/reference/sweep.md)
  [`summary(`*`<opinionSweep>`*`)`](https://stocnet.github.io/infernet/reference/sweep.md)
  : Sweeping a diffusion or an opinion dynamic over its settings
