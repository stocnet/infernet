# ============================================================
# Tests for utility helpers:
#   net_from_edgelist() (public wrapper of df_to_mat())
#   probabilistic_confusion_matrix()
# ============================================================


# ---- net_from_edgelist -----------------------------------------------------

test_that("net_from_edgelist converts directed edge list to a matrix", {
  df <- data.frame(
    from   = c("A", "A", "B", "B", "C", "C"),
    to     = c("B", "C", "A", "C", "A", "B"),
    weight = c(1, 2, 3, 4, 5, 6)
  )
  mats <- net_from_edgelist(df, sender = "from", receiver = "to")
  expect_type(mats, "list")
  expect_named(mats, "weight")
  expect_true(is.matrix(mats$weight))
  expect_equal(dim(mats$weight), c(3L, 3L))
  expect_equal(mats$weight["A", "B"], 1)
  expect_equal(mats$weight["B", "A"], 3)
})

test_that("net_from_edgelist with undirected mode symmetrises", {
  df <- data.frame(
    from   = c("A", "B", "A"),
    to     = c("B", "C", "C"),
    weight = c(1, 2, 3)
  )
  mats <- net_from_edgelist(df, sender = "from", receiver = "to",
                            mode = "undirected")
  expect_equal(mats$weight["A", "B"], mats$weight["B", "A"])
  expect_equal(mats$weight["B", "C"], mats$weight["C", "B"])
})

test_that("net_from_edgelist diagonal is NA when loops = FALSE", {
  df <- data.frame(
    from   = c("A", "A", "B", "B", "C", "C"),
    to     = c("B", "C", "A", "C", "A", "B"),
    weight = c(1, 2, 3, 4, 5, 6)
  )
  mats <- net_from_edgelist(df, sender = "from", receiver = "to",
                            loops = FALSE)
  expect_true(is.na(mats$weight["A", "A"]))
  expect_true(is.na(mats$weight["B", "B"]))
})

test_that("net_from_edgelist handles multiple value columns", {
  df <- data.frame(
    from    = c("A", "A", "B", "B", "C", "C"),
    to      = c("B", "C", "A", "C", "A", "B"),
    weight  = c(1, 2, 3, 4, 5, 6),
    strength = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
  )
  mats <- net_from_edgelist(df, sender = "from", receiver = "to")
  expect_named(mats, c("weight", "strength"))
  expect_true(is.matrix(mats$strength))
})

test_that("net_from_edgelist warns on incomplete dyadic data", {
  df <- data.frame(
    from   = c("A", "B"),
    to     = c("B", "A"),
    weight = c(1, 2)
  )
  expect_warning(
    net_from_edgelist(df, sender = "from", receiver = "to"),
    regexp = "Incomplete"
  )
})

test_that("net_from_edgelist split_by returns a list of lists", {
  df <- data.frame(
    from   = c("A", "A", "B", "B", "A", "A", "B", "B"),
    to     = c("B", "C", "A", "C", "B", "C", "A", "C"),
    weight = c(1, 2, 3, 4, 5, 6, 7, 8),
    time   = c(1, 1, 1, 1, 2, 2, 2, 2)
  )
  # Suppress "Incomplete" warnings from the split subsets
  result <- suppressWarnings(
    net_from_edgelist(df, sender = "from", receiver = "to",
                      split_by = "time")
  )
  expect_type(result, "list")
  expect_named(result, "weight")
  expect_length(result$weight, 2)
})


# ---- probabilistic_confusion_matrix ----------------------------------------

test_that("probabilistic_confusion_matrix returns a QAPConfusionMatrix", {
  actual        <- c(0, 0, 1, 1, 0, 1, 1, 0, 0, 1)
  predicted_prob <- c(0.1, 0.2, 0.8, 0.9, 0.3, 0.7, 0.6, 0.4, 0.2, 0.85)
  cm <- infernet:::probabilistic_confusion_matrix(actual, predicted_prob,
                                                  n_draws = 100, seed = 1)
  expect_s3_class(cm, "QAPConfusionMatrix")
})

test_that("probabilistic_confusion_matrix accuracy is in [0, 1]", {
  actual        <- c(0, 1, 1, 0, 1)
  predicted_prob <- c(0.2, 0.8, 0.7, 0.3, 0.9)
  cm <- infernet:::probabilistic_confusion_matrix(actual, predicted_prob,
                                                  n_draws = 100, seed = 42)
  expect_gte(cm$accuracy, 0)
  expect_lte(cm$accuracy, 1)
})

test_that("probabilistic_confusion_matrix sensitivity and specificity in [0,1]", {
  actual        <- c(0, 1, 1, 0, 1, 0, 0, 1)
  predicted_prob <- c(0.1, 0.9, 0.8, 0.2, 0.7, 0.3, 0.4, 0.6)
  cm <- infernet:::probabilistic_confusion_matrix(actual, predicted_prob,
                                                  n_draws = 200, seed = 5)
  expect_gte(cm$sensitivity, 0)
  expect_lte(cm$sensitivity, 1)
  expect_gte(cm$specificity, 0)
  expect_lte(cm$specificity, 1)
})

test_that("probabilistic_confusion_matrix confusion_matrix has correct dims", {
  actual        <- c(0, 1, 0, 1)
  predicted_prob <- c(0.3, 0.7, 0.4, 0.6)
  cm <- infernet:::probabilistic_confusion_matrix(actual, predicted_prob,
                                                  n_draws = 50, seed = 10)
  expect_equal(dim(cm$confusion_matrix), c(2L, 2L))
})

test_that("print.QAPConfusionMatrix produces expected output", {
  actual        <- c(0, 1, 1, 0, 1, 0, 0, 1)
  predicted_prob <- c(0.1, 0.9, 0.8, 0.2, 0.7, 0.3, 0.4, 0.6)
  cm <- infernet:::probabilistic_confusion_matrix(actual, predicted_prob,
                                                  n_draws = 100, seed = 3)
  expect_output(print(cm), "Probabilistic Confusion Matrix")
  expect_output(print(cm), "Accuracy")
})
