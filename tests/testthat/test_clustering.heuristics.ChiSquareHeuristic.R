testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ChiSquareHeuristic: heuristic function works", {

  heuristic <- ChiSquareHeuristic$new()

  col1 <- c(1, 0, 2)
  col2 <- c(3, 2, 3)
  column.names <- c("ex", "Class")
  testthat::expect_type(suppressWarnings(heuristic$heuristic(col1 = col1,
                                                             col2 = col2,
                                                             column.names = column.names)),
                        "double")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog(threshold = "DEBUG")
})

testthat::test_that("ChiSquareHeuristic: heuristic function checks parameter", {

  heuristic <- ChiSquareHeuristic$new()

  col1 <- "1"
  col2 <- "2"
  column.names <- c("ex", "Class")
  testthat::expect_equal(suppressWarnings(heuristic$heuristic(col1 = col1,
                                                              col2 = col2,
                                                              column.names = column.names)),
                         NA)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
