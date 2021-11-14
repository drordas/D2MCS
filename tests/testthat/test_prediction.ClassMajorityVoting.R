testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassMajorityVoting: initialize function works", {

  cutoff <- 0.5
  class.tie.character <- "Positive"
  majority.class <- "Positive"

  testthat::expect_is(ClassMajorityVoting$new(cutoff = cutoff,
                                              class.tie = class.tie.character,
                                              majority.class = majority.class),
                      "ClassMajorityVoting")

  class.tie.numeric <- 1

  testthat::expect_is(ClassMajorityVoting$new(cutoff = cutoff,
                                              class.tie = class.tie.numeric,
                                              majority.class = majority.class),
                      "ClassMajorityVoting")

  class.tie.null <- NULL

  testthat::expect_is(ClassMajorityVoting$new(cutoff = cutoff,
                                              class.tie = class.tie.null,
                                              majority.class = majority.class),
                      "ClassMajorityVoting")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassMajorityVoting: initialize function checks parameter type", {

  cutoff <- 0.5
  class.tie <- list()
  majority.class <- "Positive"

  testthat::expect_error(ClassMajorityVoting$new(cutoff = cutoff,
                                                 class.tie = class.tie,
                                                 majority.class = majority.class),
                         "[ClassMajorityVoting][initialize][FATAL] Invalid class tie value. Aborting...",
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

testthat::test_that("ClassMajorityVoting: getMajorityClass function works", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_equal(ClassMajorityVoting$new(cutoff = cutoff,
                                                 class.tie = class.tie,
                                                 majority.class = majority.class)$getMajorityClass(),
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

testthat::test_that("ClassMajorityVoting: getClassTie function works", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_equal(ClassMajorityVoting$new(cutoff = cutoff,
                                                 class.tie = class.tie,
                                                 majority.class = majority.class)$getClassTie(),
                         "Positive")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog(threshold = "DEBUG")
})

testthat::test_that("ClassMajorityVoting: execute function works", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  voting <- ClassMajorityVoting$new(cutoff = cutoff,
                                    class.tie = class.tie,
                                    majority.class = majority.class)

  predictionts <- readRDS(file.path("resourceFiles",
                                    "testVotings",
                                    "predictions.rds"))

  testthat::expect_message(voting$execute(predictions = predictionts),
                         "[ClassMajorityVoting][execute][DEBUG] Performing voting using '1' as majority class",
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

testthat::test_that("ClassMajorityVoting: execute function works (tie)", {

  cutoff <- 1
  class.tie <- "1"
  majority.class <- "1"

  voting <- ClassMajorityVoting$new(cutoff = cutoff,
                                    class.tie = class.tie,
                                    majority.class = majority.class)

  predictions <- readRDS(file.path("resourceFiles",
                                   "testVotings",
                                   "predictions.rds"))

  predictions$add(prediction = predictions$get(1))

  testthat::expect_message(suppressWarnings(voting$execute(predictions = predictions)),
                           "[ClassMajorityVoting][execute][INFO] Found Tie. Resolving using 'majority class' solver",
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

testthat::test_that("ClassMajorityVoting: execute function checks parameter type", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  voting <- ClassMajorityVoting$new(cutoff = cutoff,
                                    class.tie = class.tie,
                                    majority.class = majority.class)

  testthat::expect_error(voting$execute(predictions = NULL),
                         "[ClassMajorityVoting][execute][FATAL] Predictions parameter must be defined as 'ClusterPrediction' type. Aborting...",
                         fixed = TRUE)

  predictions  <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                         positive.class = 1)
  testthat::expect_error(voting$execute(predictions = predictions),
                         "[ClassMajorityVoting][execute][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
