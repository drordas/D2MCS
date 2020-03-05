testthat::test_that("ClassWeightedVoting: initialize", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_is(ClassWeightedVoting$new(cutoff = cutoff,
                                              weights = weights),
                      "ClassWeightedVoting")
})

testthat::test_that("ClassWeightedVoting: getWeights function works", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_equal(ClassWeightedVoting$new(cutoff = cutoff,
                                                 weights = weights)$getWeights(),
                         weights)
})

testthat::test_that("ClassWeightedVoting: setWeights function works", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_silent(ClassWeightedVoting$new(cutoff = cutoff,
                                                  weights = weights)$setWeights(weights = weights))
})

testthat::test_that("ClassWeightedVoting: setWeights function checks parameter type", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  testthat::expect_message(ClassWeightedVoting$new(cutoff = cutoff,
                                                   weights = weights)$setWeights(weights = NULL),
                           "[ClassWeightedVoting][WARNING] Weights values not changed due to inconsistency error",
                           fixed = TRUE)
})

testthat::test_that("ClassWeightedVoting: execute function checks parameter type", {

  cutoff <- 0.5
  weights <- c(0.6, 0.5)

  voting <- ClassWeightedVoting$new(cutoff = cutoff,
                                    weights = weights)

  testthat::expect_error(voting$execute(predictions = NULL,
                                        verbose = FALSE),
                         "[ClassWeightedVoting][FATAL] Predictions parameter must be defined as 'ClusterPrediction' type. Aborting...",
                         fixed = TRUE)

  predictions  <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                         positive.class = 1)
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[ClassWeightedVoting][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})
