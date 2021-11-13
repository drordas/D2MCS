testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: initialize function works", {

  cutoff <- 0.5
  class.tie <- 1
  weights <- c(0.6, 0.5)

  testthat::expect_is(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                    class.tie = class.tie,
                                                    weights = weights),
                      "ProbAverageWeightedVoting")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: initialize function checks parameter type", {

  cutoff <- 0.5
  class.tie <- list()
  weights <- c(0.6, 0.5)

  testthat::expect_error(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                       class.tie = class.tie,
                                                       weights = weights),
                         "[ProbAverageWeightedVoting][initialize][FATAL] Invalid class tie value. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: getClassTie function works", {

  cutoff <- 0.5
  class.tie <- "Positive"
  weights <- c(0.6, 0.5)

  testthat::expect_equal(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                       class.tie = class.tie,
                                                       weights = weights)$getClassTie(),
                         "Positive")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: getWeights function works", {

  cutoff <- 0.5
  class.tie <- 1
  weights <- c(0.6, 0.5)

  testthat::expect_equal(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                       class.tie = class.tie,
                                                       weights = weights)$getWeights(),
                         weights)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: setWeights function works", {

  cutoff <- 0.5
  class.tie <- 1
  weights <- c(0.6, 0.5)

  testthat::expect_silent(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                        class.tie = class.tie,
                                                        weights = weights)$setWeights(weights = weights))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: setWeights function checks parameter type", {

  cutoff <- 0.5
  class.tie <- 1
  weights <- c(0.6, 0.5)

  testthat::expect_message(ProbAverageWeightedVoting$new(cutoff = cutoff,
                                                         class.tie = class.tie,
                                                         weights = weights)$setWeights(weights = NULL),
                           "[ProbAverageWeightedVoting][setWeights][ERROR] Weights values not changed due to inconsistency error",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: execute function works", {

  cutoff <- 0.5
  class.tie <- 1
  weights <- NULL

  voting <- ProbAverageWeightedVoting$new(cutoff = cutoff,
                                          class.tie = class.tie,
                                          weights = weights)

  predictions <- readRDS(file.path("resourceFiles",
                                   "testVotings",
                                   "predictions.rds"))

  verbose <- TRUE

  testthat::expect_warning(voting$execute(predictions = predictions,
                                          verbose = verbose),
                           "[ProbAverageWeightedVoting][execute][WARN] Weight values are missing or incorrect. Assuming default model performance values",
                           fixed = TRUE)

  testthat::expect_message(voting$execute(predictions = predictions,
                                          verbose = verbose),
                           "[ProbAverageWeightedVoting][execute][INFO] Performing voting using '1' as tie solving",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: execute function works (tie)", {

  cutoff <- 0.5
  class.tie <- "1"
  weights <- c(0.6, 0.5)

  voting <- ProbAverageWeightedVoting$new(cutoff = cutoff,
                                          class.tie = class.tie,
                                          weights = weights)

  predictions <- readRDS(file.path("resourceFiles",
                                   "testVotings",
                                   "predictions.rds"))

  pred <- predictions$.__enclos_env__$private$pred[1]
  pred[[1]]$.__enclos_env__$private$results$prob[1, ] <- c(0.5, 0.5)
  predictions$.__enclos_env__$private$pred <- pred

  verbose <- TRUE
  testthat::expect_message(suppressWarnings(voting$execute(predictions = predictions,
                                                           verbose = verbose)),
                           "[ProbAverageWeightedVoting][execute][INFO] Tie solver found. Resolving tie using '1'",
                           fixed = TRUE)

  cutoff <- 0.5
  class.tie <- NULL
  weights <- c(0.6, 0.5)

  voting <- ProbAverageWeightedVoting$new(cutoff = cutoff,
                                          class.tie = class.tie,
                                          weights = weights)

  predictions <- readRDS(file.path("resourceFiles",
                                   "testVotings",
                                   "predictions.rds"))

  pred <- predictions$.__enclos_env__$private$pred[1]
  pred[[1]]$.__enclos_env__$private$results$prob[1, ] <- c(0.5, 0.5)
  predictions$.__enclos_env__$private$pred <- pred

  verbose <- TRUE
  testthat::expect_message(suppressWarnings(voting$execute(predictions = predictions,
                                                           verbose = verbose)),
                           "[ProbAverageWeightedVoting][execute][INFO] Tie solver not found. Resolving tie using first occurrence",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ProbAverageWeightedVoting: execute function checks parameter type", {

  cutoff <- 0.5
  class.tie <- 1
  weights <- c(0.6, 0.5)

  voting <- ProbAverageWeightedVoting$new(cutoff = cutoff,
                                          class.tie = class.tie,
                                          weights = weights)

  testthat::expect_error(voting$execute(predictions = NULL,
                                        verbose = FALSE),
                         "[ProbAverageWeightedVoting][execute][FATAL] Predictions parameter must be defined as 'ClusterPrediction' type. Aborting...",
                         fixed = TRUE)

  predictions  <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                         positive.class = 1)
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[ProbAverageWeightedVoting][execute][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
