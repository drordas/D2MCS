testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("PredictionOutput: initialize function works", {

  testthat::expect_is(PredictionOutput$new(predictions = NULL,
                                           type = NULL,
                                           target = NULL),
                      "PredictionOutput")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("PredictionOutput: getPredictions function works", {

  testthat::expect_null(PredictionOutput$new(predictions = NULL,
                                             type = NULL,
                                             target = NULL)$getPredictions())
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("PredictionOutput: getType function works", {

  testthat::expect_null(PredictionOutput$new(predictions = NULL,
                                             type = NULL,
                                             target = NULL)$getType())
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("PredictionOutput: getTarget function works", {

  testthat::expect_null(PredictionOutput$new(predictions = NULL,
                                             type = NULL,
                                             target = NULL)$getTarget())
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
