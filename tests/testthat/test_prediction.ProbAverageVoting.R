testthat::test_that("ProbAverageVoting: initialize", {

  cutoff <- 0.5
  class.tie.character <- "Positive"
  majority.class <- "Positive"

  testthat::expect_is(ProbAverageVoting$new(cutoff = cutoff,
                                            class.tie = class.tie.character,
                                            majority.class = majority.class),
                      "ProbAverageVoting")

  class.tie.numeric <- 1

  testthat::expect_is(ProbAverageVoting$new(cutoff = cutoff,
                                            class.tie = class.tie.numeric,
                                            majority.class = majority.class),
                      "ProbAverageVoting")

  class.tie.null <- NULL

  testthat::expect_is(ProbAverageVoting$new(cutoff = cutoff,
                                            class.tie = class.tie.null,
                                            majority.class = majority.class),
                      "ProbAverageVoting")
})

testthat::test_that("ProbAverageVoting: initialize checks parameter type", {

  cutoff <- 0.5
  class.tie <- list()
  majority.class <- "Positive"

  testthat::expect_error(ProbAverageVoting$new(cutoff = cutoff,
                                               class.tie = class.tie,
                                               majority.class = majority.class),
                         "[ProbAverageVoting][FATAL] Invalid class tie value. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ProbAverageVoting: getMajorityClass function works", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_equal(ProbAverageVoting$new(cutoff = cutoff,
                                               class.tie = class.tie,
                                               majority.class = majority.class)$getMajorityClass(),
                         "Positive")
})

testthat::test_that("ProbAverageVoting: getClassTie function works", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_equal(ProbAverageVoting$new(cutoff = cutoff,
                                               class.tie = class.tie,
                                               majority.class = majority.class)$getClassTie(),
                         "Positive")
})

testthat::test_that("ProbAverageVoting: execute function checks parameter type", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  voting <- ProbAverageVoting$new(cutoff = cutoff,
                                  class.tie = class.tie,
                                  majority.class = majority.class)

  testthat::expect_error(voting$execute(predictions = NULL,
                                        verbose = FALSE),
                         "[ProbAverageVoting][FATAL] Predictions parameter must be defined as 'ClusterPrediction' type. Aborting...",
                         fixed = TRUE)

  predictions  <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                         positive.class = 1)
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[ProbAverageVoting][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})
