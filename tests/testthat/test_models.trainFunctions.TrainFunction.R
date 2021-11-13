testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: initialize function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_is(TrainFunction$new(method = method,
                                        number = number,
                                        savePredictions = savePredictions,
                                        classProbs = classProbs,
                                        allowParallel = allowParallel,
                                        verboseIter = verboseIter,
                                        seed = seed),
                      "TrainFunction")

  testthat::expect_is(TrainFunction$new(method = method,
                                        number = number,
                                        savePredictions = savePredictions,
                                        classProbs = classProbs,
                                        allowParallel = allowParallel,
                                        verboseIter = verboseIter,
                                        seed = set.seed(2)),
                      "TrainFunction")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: create function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_error(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$create(NULL, NULL, NULL),
                         "[TrainFunction][create][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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

testthat::test_that("TrainFunction: getResamplingMethod function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_equal(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getResamplingMethod(),
                         method)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: getNumberFolds function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_equal(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getNumberFolds(),
                         number)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: getSavePredictions function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_equal(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getSavePredictions(),
                         savePredictions)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: getClassProbs function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_equal(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getClassProbs(),
                         classProbs)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: getAllowParallel function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_equal(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getAllowParallel(),
                         allowParallel)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: getVerboseIter function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_equal(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getVerboseIter(),
                         verboseIter)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: getTrFunction function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_error(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getTrFunction(),
                         "[TrainFunction][getTrFunction][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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

testthat::test_that("TrainFunction: getMeasures function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_error(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getMeasures(),
                         "[TrainFunction][getMeasures][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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

testthat::test_that("TrainFunction: getType function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_error(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getType(),
                         "[TrainFunction][getType][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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

testthat::test_that("TrainFunction: getSeed function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- .Random.seed

  testthat::expect_equal(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$getSeed(),
                         seed)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TrainFunction: setSummaryFunction function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_error(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$setSummaryFunction(NULL),
                         "[TrainFunction][setSummaryFunction][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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

testthat::test_that("TrainFunction: setClassProbs function works", {

  method <- "nb"
  number <- "2"
  savePredictions <- TRUE
  classProbs <- TRUE
  allowParallel <- TRUE
  verboseIter <- TRUE
  seed <- NULL

  testthat::expect_error(TrainFunction$new(method = method,
                                           number = number,
                                           savePredictions = savePredictions,
                                           classProbs = classProbs,
                                           allowParallel = allowParallel,
                                           verboseIter = verboseIter,
                                           seed = seed)$setClassProbs(NULL),
                         "[TrainFunction][setClassProbs][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
