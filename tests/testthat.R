library(testthat)
library(infernet)

stocnet_theme("default")
test_check("infernet")
devtools::test_coverage(pkg = "infernet", type = "tests")
