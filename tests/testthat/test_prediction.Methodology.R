testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Methodology: initialize function works", {

  testthat::expect_is(Methodology$new(required.metrics = c("MCC", "PPV")),
                      "Methodology")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Methodology: initialize function checks parameter type", {

  testthat::expect_error(Methodology$new(required.metrics = NULL),
                         "[Methodology][initialize][FATAL] required.metrics parameter must be defined as 'character' type. Aborting...",
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

testthat::test_that("Methodology: getRequiredMetrics function works", {

  testthat::expect_equal(Methodology$new(required.metrics = c("MCC", "PPV"))$getRequiredMetrics(),
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

testthat::test_that("Methodology: compute function works", {

  testthat::expect_error(Methodology$new(required.metrics = c("MCC", "PPV"))$compute(raw.pred = NULL,
                                                                                     prob.pred = NULL,
                                                                                     positive.class = NULL,
                                                                                     negative.class = NULL),
                         "[Methodology][compute][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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
