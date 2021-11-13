testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Prediction: initialize function works", {
  testthat::skip_if_not_installed("ranger")
  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  testthat::expect_is(Prediction$new(model = model,
                                     feature.id = feature.id),
                      "Prediction")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Prediction: initialize function checks parameter type", {

  feature.id <- "feature.id"

  testthat::expect_error(Prediction$new(model = NULL,
                                        feature.id = feature.id),
                         "[Prediction][initialize][FATAL] Model parameter must be defined as a list of five elements. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Prediction$new(model = list(),
                                        feature.id = feature.id),
                         "[Prediction][initialize][FATAL] Model parameter must be defined as a list of five elements. Aborting...",
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

testthat::test_that("Prediction: execute function works (classProbs=TRUE)", {
  testthat::skip_if_not_installed("ranger")
  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  pred.values <- readRDS(file.path("resourceFiles",
                                   "testPrediction",
                                   "predvalues.rds"))
  class.values <- c("1", "0")
  positive.class <- 1

  testthat::expect_silent(suppressWarnings(prediction$execute(pred.values = pred.values,
                                                              class.values = class.values,
                                                              positive.class = positive.class)))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Prediction: execute function works (classProbs=TRUE)", {
  testthat::skip_if_not_installed("ranger")
  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  model$model.data$control$classProbs <- FALSE

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  pred.values <- readRDS(file.path("resourceFiles",
                                   "testPrediction",
                                   "predvalues.rds"))
  class.values <- c("1", "0")
  positive.class <- 1

  testthat::expect_warning(prediction$execute(pred.values = pred.values,
                                              class.values = class.values,
                                              positive.class = positive.class),
                           "[Prediction][execute][WARN] Model 'ranger' is not able to compute a-posteriori probabilities",
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

testthat::test_that("Prediction: execute function checks parameter type", {
  testthat::skip_if_not_installed("ranger")
  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  pred.values <- NULL
  class.values <- c("1", "0")
  positive.class <- 1

  testthat::expect_error(prediction$execute(pred.values = NULL,
                                            class.values = class.values,
                                            positive.class = positive.class),
                         "[Prediction][execute][FATAL] Prediction values parameter must be defined as 'data.frame' type. Aborting...",
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

testthat::test_that("Prediction: getPredicion function works", {
  testthat::skip_if_not_installed("ranger")
  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  pred.values <- readRDS(file.path("resourceFiles",
                                   "testPrediction",
                                   "predvalues.rds"))
  class.values <- c("1", "0")
  positive.class <- 1

  suppressWarnings(prediction$execute(pred.values = pred.values,
                                      class.values = class.values,
                                      positive.class = positive.class))

  type <- "raw"
  target <- 1
  testthat::expect_equal(class(prediction$getPrediction(type = type,
                                                  target = target)),
                         "data.frame")
  type <- "prob"
  testthat::expect_equal(class(prediction$getPrediction(type = type,
                                                        target = target)),
                         "data.frame")

})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Prediction: getPredicion function checks parameter type", {
  testthat::skip_if_not_installed("ranger")
  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  pred.values <- readRDS(file.path("resourceFiles",
                                   "testPrediction",
                                   "predvalues.rds"))
  class.values <- c("1", "0")
  positive.class <- 1

  suppressWarnings(prediction$execute(pred.values = pred.values,
                                      class.values = class.values,
                                      positive.class = positive.class))

  type <- "prob"
  target <- 1
  testthat::expect_warning(prediction$getPrediction(type = "wrong",
                                                    target = target),
                           "[Prediction][getPrediction][WARN] Probability type missing or incorrect. Should be 'raw' or 'prob'. Assuming 'raw' by default",
                           fixed = TRUE)

  testthat::expect_warning(prediction$getPrediction(type = type,
                                                    target = NULL),
                           "[Prediction][getPrediction][WARN] Target not specified or invalid. Using '1' as default value",
                           fixed = TRUE)

  testthat::expect_warning(prediction$getPrediction(type = type,
                                                    target = 100),
                           "[Prediction][getPrediction][WARN] Target not specified or invalid. Using '1' as default value",
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

testthat::test_that("Prediction: getModelName function works", {
  testthat::skip_if_not_installed("ranger")
  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  testthat::expect_equal(prediction$getModelName(), model$model.name)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Prediction: getModelPerformance function works", {
  testthat::skip_if_not_installed("ranger")
  model <- readRDS(file.path("resourceFiles",
                             "testPrediction",
                             "model.classProbsTrue.rds"))
  feature.id <- NULL

  prediction <- Prediction$new(model = model,
                               feature.id = feature.id)

  testthat::expect_equal(prediction$getModelPerformance(), model$model.performance)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
