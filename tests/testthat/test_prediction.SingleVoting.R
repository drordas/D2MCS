testthat::test_that("SingleVoting: initialize checks parameter type", {

  voting.schemes <- NULL
  metrics <- c("MCC")
  testthat::expect_error(SingleVoting$new(voting.schemes = voting.schemes,
                                          metrics = metrics),
                         "[SingleVoting][FATAL] Voting schemes parameter must be a list comprised of 'SimpleVoting' objects. Aborting...",
                         fixed = TRUE)

  voting.schemes <- c(ClassWeightedVoting$new(cutoff = 0.7))
  metrics <- NULL
  testthat::expect_error(SingleVoting$new(voting.schemes = voting.schemes,
                                          metrics = metrics),
                         "[SingleVoting][FATAL] Metrics parameter must be a list comprised of 'character' objects. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("SingleVoting: execute function checks parameter type", {

  voting.schemes <- c(ClassWeightedVoting$new(cutoff = 0.7))
  metrics <- c("MCC")
  voting <- SingleVoting$new(voting.schemes = voting.schemes,
                            metrics = metrics)
  predictions <- NULL
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[SingleVoting][FATAL] Predictions parameter must be a list comprised of 'ClusterPredictions' objects. Aborting...",
                         fixed = TRUE)

  predictions <- list(ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                             positive.class = 1))
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[SingleVoting][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})
