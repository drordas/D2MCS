testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("MultinformationHeuristic: heuristic function works", {

  heuristic <- MultinformationHeuristic$new()

  col1 <- c(1, 0, 1)
  col2 <- c(1, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_type(heuristic$heuristic(col1 = col1,
                                            col2 = col2,
                                            column.names = column.names),
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

testthat::test_that("MultinformationHeuristic: heuristic function checks parameter type", {

  heuristic <- MultinformationHeuristic$new()

  col1 <- c(2, 0, 1)
  col2 <- c(1, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[MultinformationHeuristic][DEBUG] Columns must be binary. Returning NA",
                           fixed = TRUE)

  col1 <- c(1, 0, 1)
  col2 <- c(2, 1, 0)
  column.names <- c("ex", "Class")
  testthat::expect_message(heuristic$heuristic(col1 = col1,
                                               col2 = col2,
                                               column.names = column.names),
                           "[MultinformationHeuristic][DEBUG] Columns must be binary. Returning NA",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
