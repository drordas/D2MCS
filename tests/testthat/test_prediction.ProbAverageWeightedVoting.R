testthat::test_that("ProbAverageWeightedVoting: initialize", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_is(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                    weights = weights),
                      "ProbAverageWeightedVoting")
})

testthat::test_that("ProbAverageWeightedVoting: getWeights function works", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_equal(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                       weights = weights)$getWeights(),
                         weights)
})

testthat::test_that("ProbAverageWeightedVoting: setWeights function works", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_silent(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                        weights = weights)$setWeights(weights = weights))
})

testthat::test_that("ProbAverageWeightedVoting: setWeights function checks parameter type", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_message(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                         weights = weights)$setWeights(weights = NULL),
                           "[ProbAverageWeightedVoting][WARNING] Weights values not changed due to inconsistency error",
                           fixed = TRUE)
})

testthat::test_that("ProbAverageWeightedVoting: execute function checks parameter type", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  voting <- ProbAverageWeightedVoting$new(cutoff = cutoff,
                                          weights = weights)

  testthat::expect_error(voting$execute(predictions = NULL,
                                        verbose = FALSE),
                         "[ProbAverageWeightedVoting][FATAL] Predictions parameter must be defined as 'ClusterPrediction' type. Aborting...",
                         fixed = TRUE)

  predictions  <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                         positive.class = 1)
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[ProbAverageWeightedVoting][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})
