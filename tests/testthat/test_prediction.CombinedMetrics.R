testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("CombinedMetrics: initialize function works", {

  testthat::expect_is(CombinedMetrics$new(required.metrics = c("MCC", "PPV")),
                      "CombinedMetrics")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("CombinedMetrics: initialize function checks parameter type", {

  testthat::expect_error(CombinedMetrics$new(required.metrics = NULL),
                         "[CombinedMetrics][initialize][FATAL] The required.metrics parameter must be defined as 'character' type. Aborting...",
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

testthat::test_that("CombinedMetrics: getRequiredMetrics function works", {

  testthat::expect_equal(CombinedMetrics$new(required.metrics = c("MCC", "PPV"))$getRequiredMetrics(),
                         c("MCC", "PPV"))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("CombinedMetrics: getFinalPrediction function works", {

  testthat::expect_error(CombinedMetrics$new(required.metrics = c("MCC", "PPV"))$getFinalPrediction(raw.pred = NULL,
                                                                                                    prob.pred = NULL,
                                                                                                    positive.class = NULL,
                                                                                                    negative.class = NULL),
                         "[CombinedMetrics][getFinalPrediction][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
