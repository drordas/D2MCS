testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("FIterator: initialize function works", {

  file.path <- file.path("resourceFiles",
                        "data",
                        "hcc-data-complete-balanced.csv")

  sep <- ","
  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = sep))
  feature.names <- setNames(data.frame(matrix(ncol = length(column.names),
                             nrow = 0)),
                            column.names)

  config.params <- list(file.path = file.path,
                        feature.names = feature.names,
                        start = 0, sep = sep)

  chunk.size <- 1

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size)
  testthat::expect_is(fIterator,
                      "FIterator")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog(threshold = "DEBUG")
})

testthat::test_that("FIterator: getNext function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  sep <- ","
  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = sep))
  feature.names <- setNames(data.frame(matrix(ncol = length(column.names),
                                              nrow = 0)),
                            column.names)

  config.params <- list(file.path = file.path,
                        feature.names = feature.names,
                        start = 0, sep = sep)

  chunk.size <- 1000000

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size)

  fIterator$getNext()
  testthat::expect_null(fIterator$getNext())

  chunk.size <- 100

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size)

  fIterator$getNext()
  testthat::expect_is(fIterator$getNext(),
                      "data.frame")

  chunk.size <- 100

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size)

  fIterator$getNext()
  testthat::expect_message(fIterator$getNext(),
                           "[FIterator][getNext][DEBUG] Read lines 100 to 200 [100]",
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

testthat::test_that("FIterator: isLast function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  sep <- ","
  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = sep))
  feature.names <- setNames(data.frame(matrix(ncol = length(column.names),
                                              nrow = 0)),
                            column.names)

  config.params <- list(file.path = file.path,
                        feature.names = feature.names,
                        start = 0, sep = sep)

  chunk.size <- 1000000

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size)

  fIterator$getNext()
  testthat::expect_true(fIterator$isLast())
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("FIterator: finalize function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  sep <- ","
  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = sep))
  feature.names <- setNames(data.frame(matrix(ncol = length(column.names),
                                              nrow = 0)),
                            column.names)

  config.params <- list(file.path = file.path,
                        feature.names = feature.names,
                        start = 0, sep = sep)

  chunk.size <- 1000000

  fIterator <- FIterator$new(config.params = config.params,
                             chunk.size = chunk.size)
  fIterator$finalize()

  testthat::expect_null(fIterator$.__enclos_env__$private$con)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
