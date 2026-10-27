test_that("play_diffusions() binds the runs into a diffs_model", {
  set.seed(21)
  g <- manynet::generate_random(12, 0.25)
  res <- play_diffusions(g, transmissibility = 0.4, times = 6,
                         control = list(seed = 1))
  expect_s3_class(res, "diffs_model")
  expect_equal(sort(unique(res$sim)), 1:6)
  # `{manynet}` names the step `t` on some networks; `{autograph}` reads `time`
  expect_true(all(c("sim", "time", "n", "S", "I") %in% names(res)))
  expect_false("t" %in% names(res))
  expect_true(all(tapply(res$time, res$sim, min) == 0))
  toa <- summary(res)
  expect_equal(nrow(toa), 6)
  expect_true(all(toa$toa >= 1))
  expect_output(print(res), "sim")
})

test_that("play_diffusions() reads a stocnet network", {
  res <- play_diffusions(manynet::ison_networkers, times = 2,
                         control = list(seed = 1))
  expect_s3_class(res, "diffs_model")
  expect_true("time" %in% names(res))
})

test_that("play_diffusions() reads its old arguments as controls", {
  g <- manynet::generate_random(10, 0.3)
  expect_warning(
    old <- play_diffusions(g, times = 3, strategy = "sequential",
                           control = list(seed = 4)),
    "control")
  new <- play_diffusions(g, times = 3, control = list(seed = 4))
  expect_equal(old, new)
  expect_warning(play_diffusions(g, times = 2, verbose = TRUE),
                 "snet_verbosity")
})

test_that("play_diffusions() binds runs that report different columns", {
  runs <- list(data.frame(sim = 1, time = 0:1, I = 1:2),
               NULL,
               data.frame(sim = 3, time = 0, I = 1, S_new = 2))
  out <- infernet:::.bind_reports(runs)
  expect_equal(nrow(out), 3)
  expect_equal(out$S_new, c(0, 0, 2))
})
