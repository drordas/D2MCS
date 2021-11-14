testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("SimpleVoting: initialize function works", {

  cutoff <- 0.5
  testthat::expect_is(SimpleVoting$new(cutoff = cutoff),
                      "SimpleVoting")

  testthat::expect_is(SimpleVoting$new(),
                      "SimpleVoting")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("SimpleVoting: initialize function checks parameter type", {

  cutoff <- "a"
  testthat::expect_error(SimpleVoting$new(cutoff = cutoff),
                         "[SimpleVoting][initialize][FATAL] Invalid values of cutoff. Aborting...",
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

testthat::test_that("SimpleVoting: getCutoff function works", {

  cutoff <- 0.5
  testthat::expect_equal(SimpleVoting$new(cutoff = cutoff)$getCutoff(),
                         0.5)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("SimpleVoting: execute function works", {

  cutoff <- 0.5
  testthat::expect_error(SimpleVoting$new(cutoff = cutoff)$execute(predictions = NULL),
                         "[SimpleVoting][execute][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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

testthat::test_that("SimpleVoting: getFinalPred function works", {

  type <- NULL
  target <- NULL
  filter <- NULL
  testthat::expect_is(SimpleVoting$new()$getFinalPred(type = type,
                                                      target = target,
                                                      filter = filter),
                      "FinalPred")
  type <- "raw"
  target <- NULL
  filter <- NULL
  testthat::expect_warning(SimpleVoting$new()$getFinalPred(type = type,
                                                           target = target,
                                                           filter = filter),
                           "[SimpleVoting][getFinalPred][WARN] Filter parameter must be defined as 'logical' type. Assuming FALSE value",
                           fixed = TRUE)
  type <- "raw"
  target <- NULL
  filter <- TRUE
  testthat::expect_null(SimpleVoting$new()$getFinalPred(type = type,
                                                        target = target,
                                                        filter = filter))

  type <- "raw"
  target <- 1
  filter <- TRUE
  testthat::expect_null(SimpleVoting$new()$getFinalPred(type = type,
                                                        target = target,
                                                        filter = filter))

  type <- "prob"
  target <- NULL
  filter <- FALSE
  testthat::expect_null(suppressWarnings(SimpleVoting$new()$getFinalPred(type = type,
                                                                         target = target,
                                                                         filter = filter)))

  type <- "prob"
  target <- NULL
  filter <- TRUE
  testthat::expect_null(suppressWarnings(SimpleVoting$new()$getFinalPred(type = type,
                                                                         target = target,
                                                                         filter = filter)))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
