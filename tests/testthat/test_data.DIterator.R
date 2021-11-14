testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("DIterator: initialize function works", {

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 1

  testthat::expect_is(DIterator$new(data = data,
                                    chunk.size = chunk.size),
                      "DIterator")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog(threshold = "DEBUG")
})

testthat::test_that("DIterator: getNext function works", {

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 4

  dIterator  <- DIterator$new(data = data,
                              chunk.size = chunk.size)

  dIterator$getNext()
  testthat::expect_null(dIterator$getNext())

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 2

  dIterator  <- DIterator$new(data = data,
                              chunk.size = chunk.size)
  testthat::expect_is(dIterator$getNext(),
                      "data.frame")

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 1

  dIterator  <- DIterator$new(data = data,
                              chunk.size = chunk.size)
  testthat::expect_message(dIterator$getNext(),
                           "[DIterator][getNext][DEBUG] Read lines 0 to 1 [1]",
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

testthat::test_that("DIterator: isLast function works", {

  data <- data.frame(c(1, 0, 1), c(0, 0, 1))
  chunk.size <- 4

  dIterator  <- DIterator$new(data = data,
                              chunk.size = chunk.size)

  dIterator$getNext()
  testthat::expect_true(dIterator$isLast())
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
