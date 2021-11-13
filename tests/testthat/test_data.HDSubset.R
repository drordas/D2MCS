testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("HDSubset: initialize function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path,
                     header = TRUE,
                     nrows = 0,
                     sep = ",",
                     stringsAsFactors = FALSE)

  feature.id <- 1
  start.at <- 0
  sep <- ","
  chunk.size <- 10

  testthat::expect_is(HDSubset$new(file.path = file.path,
                                   feature.names = corpus,
                                   feature.id = feature.id,
                                   start.at = start.at,
                                   sep = sep,
                                   chunk.size = chunk.size),
                      "HDSubset")

  testthat::expect_is(HDSubset$new(file.path = file.path,
                                   feature.names = corpus,
                                   feature.id = FALSE,
                                   start.at = start.at,
                                   sep = sep,
                                   chunk.size = chunk.size),
                      "HDSubset")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("HDSubset: initialize function checks parameter type", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path,
                     header = TRUE,
                     nrows = 0,
                     sep = ",",
                     stringsAsFactors = FALSE)

  feature.id <- 1
  start.at <- 0
  sep <- ","
  chunk.size <- 10

  testthat::expect_error(HDSubset$new(file.path = file.path,
                                      feature.names = NULL,
                                      feature.id = feature.id,
                                      start.at = start.at,
                                      sep = sep,
                                      chunk.size = chunk.size),
                         "[HDSubset][initialize][FATAL] Dataset has not being preloaded. Aborting...",
                         fixed = TRUE)

  testthat::expect_warning(HDSubset$new(file.path = file.path,
                                        feature.names = corpus,
                                        feature.id = feature.id,
                                        start.at = -1,
                                        sep = sep,
                                        chunk.size = chunk.size),
                           "[HDSubset][initialize][WARN] Starting point must be a non-negative numeric value. Assuming 0 as default value",
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

testthat::test_that("HDSubset: getColumnNames function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path,
                     header = TRUE,
                     nrows = 0,
                     sep = ",",
                     stringsAsFactors = FALSE)

  feature.id <- 1
  start.at <- 0
  sep <- ","
  chunk.size <- 10

  hdSubset <- HDSubset$new(file.path = file.path,
                           feature.names = corpus,
                           feature.id = feature.id,
                           start.at = start.at,
                           sep = sep,
                           chunk.size = chunk.size)

  testthat::expect_equal(hdSubset$getColumnNames(),
                         names(corpus))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("HDSubset: getNcol function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path,
                     header = TRUE,
                     nrows = 0,
                     sep = ",",
                     stringsAsFactors = FALSE)

  feature.id <- 1
  start.at <- 0
  sep <- ","
  chunk.size <- 10

  hdSubset <- HDSubset$new(file.path = file.path,
                           feature.names = corpus,
                           feature.id = feature.id,
                           start.at = start.at,
                           sep = sep,
                           chunk.size = chunk.size)

  testthat::expect_equal(hdSubset$getNcol(),
                         length(names(corpus)))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("HDSubset: getID function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path,
                     header = TRUE,
                     sep = ",",
                     stringsAsFactors = FALSE)

  feature.id <- 1
  start.at <- 0
  sep <- ","
  chunk.size <- 10

  hdSubset <- HDSubset$new(file.path = file.path,
                           feature.names = corpus,
                           feature.id = feature.id,
                           start.at = start.at,
                           sep = sep,
                           chunk.size = chunk.size)

  testthat::expect_equal(hdSubset$getID(),
                         names(corpus)[feature.id])
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("HDSubset: getNcol function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path,
                     header = TRUE,
                     nrows = 0,
                     sep = ",",
                     stringsAsFactors = FALSE)

  feature.id <- 1
  start.at <- 0
  sep <- ","
  chunk.size <- 10

  hdSubset <- HDSubset$new(file.path = file.path,
                           feature.names = corpus,
                           feature.id = feature.id,
                           start.at = start.at,
                           sep = sep,
                           chunk.size = chunk.size)

  testthat::expect_is(hdSubset$getIterator(chunk.size = 100,
                                           verbose = TRUE),
                      "FIterator")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("HDSubset: getIterator function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path,
                     header = TRUE,
                     nrows = 0,
                     sep = ",",
                     stringsAsFactors = FALSE)

  feature.id <- 1
  start.at <- 0
  sep <- ","
  chunk.size <- 10

  hdSubset <- HDSubset$new(file.path = file.path,
                           feature.names = corpus,
                           feature.id = feature.id,
                           start.at = start.at,
                           sep = sep,
                           chunk.size = chunk.size)

  testthat::expect_warning(hdSubset$getIterator(chunk.size = "wrong",
                                                verbose = TRUE),
                           "[HDSubset][getIterator][WARN] Chunk size is not valid. Assuming default value",
                           fixed = TRUE)
  testthat::expect_warning(hdSubset$getIterator(chunk.size = 100,
                                                verbose = "wrong"),
                           "[HDSubset][getIterator][WARN] Verbose type is not valid. Assuming 'FALSE' as default value",
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

testthat::test_that("HDSubset: isBlinded function works", {

  file.path <- file.path("resourceFiles",
                         "data",
                         "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path,
                     header = TRUE,
                     nrows = 0,
                     sep = ",",
                     stringsAsFactors = FALSE)

  feature.id <- 1
  start.at <- 0
  sep <- ","
  chunk.size <- 10

  hdSubset <- HDSubset$new(file.path = file.path,
                           feature.names = corpus,
                           feature.id = feature.id,
                           start.at = start.at,
                           sep = sep,
                           chunk.size = chunk.size)

  testthat::expect_true(hdSubset$isBlinded())
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
