testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("VotingStrategy: initialize function works", {

  testthat::expect_is(VotingStrategy$new(),
                      "VotingStrategy")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("VotingStrategy: getVotingSchemes function works", {

  testthat::expect_null(VotingStrategy$new()$getVotingSchemes())
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("VotingStrategy: getMetrics function works", {

  testthat::expect_null(VotingStrategy$new()$getMetrics())
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("VotingStrategy: execute function works", {

  testthat::expect_error(VotingStrategy$new()$execute(predictions = NULL),
                         "[VotingStrategy][execute][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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

testthat::test_that("VotingStrategy: getName function works", {

  testthat::expect_equal(VotingStrategy$new()$getName(),
                         "VotingStrategy",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
