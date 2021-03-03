testthat::test_that("Model: initialize function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- testthat::expect_message(Model$new(dir.path = dir.path,
                                                   model = model),
                                         "[Model][INFO] Save directory not exist. Creating...",
                                         fixed = TRUE)
  testthat::expect_is(modelClass,
                      "Model")
  testthat::expect_true(file.exists(file.path("resourceFiles",
                                              "testModel",
                                              "dirpath")))

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "initializeTest",
                        "model")

  testthat::expect_message(Model$new(dir.path = dir.path, model = model),
                           "[Model][INFO] Model 'lda' already exists. Loading...",
                           fixed = TRUE)

  testthat::expect_message(Model$new(dir.path = dir.path, model = model),
                           "[Model][INFO] 'lda', Linear Discriminant Analysis', Discriminant Analysis' has been succesfully loaded!",
                           fixed = TRUE)

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "initializeTest",
                        "wrongModel")

  testthat::expect_message(Model$new(dir.path = dir.path, model = model),
                           "[Model][ERROR] Unable to load trained model. Task not performed",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: initialize function checks parameter", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  testthat::expect_error(Model$new(dir.path = dir.path,
                                   model = NULL),
                         "[Model][FATAL] Model was not defined. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: isTrained function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_false(modelClass$isTrained())
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getDir function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_equal(modelClass$getDir(), dir.path)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getName function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_equal(modelClass$getName(), model$name)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getFamily function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_equal(modelClass$getFamily(), model$family)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: getDescription function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_equal(modelClass$getDescription(), model$description)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("Model: train function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_true(modelClass$isTrained())

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "[Model][INFO][lda] Model has already been trained",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }

  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})

testthat::test_that("Model: train function checks parameter", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_error(modelClass$train(train.set = NULL,
                                          fitting = fitting,
                                          trFunction = trFunction,
                                          metric = metric,
                                          logs = logs),
                         "[Model][FATAL][lda] Cannot perform trainning stage. Train set must be defined as 'data.frame' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(modelClass$train(train.set = data.frame(),
                                          fitting = fitting,
                                          trFunction = trFunction,
                                          metric = metric,
                                          logs = logs),
                         "[Model][FATAL][lda] Cannot perform trainning stage. Train set is empty. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(modelClass$train(train.set = train.set,
                                          fitting = fitting,
                                          trFunction = NULL,
                                          metric = metric,
                                          logs = logs),
                         "[Model][FATAL][lda] TrainFunction must be inherits from 'TrainFunction' class. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(modelClass$train(train.set = train.set,
                                          fitting = fitting,
                                          trFunction = trFunction,
                                          metric = "WRONG",
                                          logs = logs),
                         "[Model][FATAL][lda] Metric is not defined or unavailable. Must be a [ROC, Sens, Spec, Kappa, Accuracy, TCR_9, MCC, PPV] type. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})

testthat::test_that("Model: getTrainedModel function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$getTrainedModel(),
                           "[Model][WARNING] Model 'lda' is not trained. Task not performed",
                           fixed = TRUE)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_type(modelClass$getTrainedModel(), "list")
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})

testthat::test_that("Model: getExecutionTime function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$getExecutionTime(),
                           "[Model][WARNING] Model 'lda' is not trained. Task not performed",
                           fixed = TRUE)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_type(modelClass$getExecutionTime(), "double")
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})

testthat::test_that("Model: getPerformance function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_is(modelClass$getPerformance(), "numeric")
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})

testthat::test_that("Model: getPerformance function checks parameter", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_error(modelClass$getPerformance(metric = "WRONG"),
                         "[Model][FATAL] Metric is not defined or unavailable. Must be a [ROC, Sens, Spec, Kappa, Accuracy, TCR_9, MCC, PPV] type. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})

testthat::test_that("Model: getConfiguration function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpath")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$getConfiguration(),
                           "[Model][WARNING] Model 'lda' is not trained. Task not performed",
                           fixed = TRUE)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_type(modelClass$getConfiguration(), "list")
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpath"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpath"),
           recursive = TRUE,
           force = TRUE)
  }
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})

testthat::test_that("Model: save function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpathSave")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$save(),
                           "[Model][ERROR] Cannot save untrained model. Task not performed",
                           fixed = TRUE)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_message(modelClass$save(replace = FALSE),
                           "[Model][INFO][lda] Model succesfully saved at: ",
                           fixed = TRUE)

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "lda.rds")))

  testthat::expect_message(modelClass$save(replace = FALSE),
                           "[Model][INFO][lda] Model already exists. Model not saved",
                           fixed = TRUE)

  testthat::expect_message(modelClass$save(replace = TRUE),
                           "[Model][WARNING][lda] Model already exists. Replacing previous model",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpathSave"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpathSave"),
           recursive = TRUE,
           force = TRUE)
  }
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})

testthat::test_that("Model: remove function works", {

  dir.path <- file.path("resourceFiles",
                        "testModel",
                        "dirpathRemove")
  model <- readRDS(file.path("resourceFiles",
                             "testModel",
                             "model.rds"))

  modelClass <- Model$new(dir.path = dir.path,
                          model = model)

  testthat::expect_message(modelClass$save(),
                           "[Model][ERROR] Cannot save untrained model. Task not performed",
                           fixed = TRUE)

  train.set <- readRDS(file.path("resourceFiles",
                                 "testModel",
                                 "trainset.rds"))
  fitting <- readRDS(file.path("resourceFiles",
                               "testModel",
                               "fitting.rds"))
  trFunction <- readRDS(file.path("resourceFiles",
                                  "testModel",
                                  "trFunction.rds"))
  metric <- "PPV"
  logs <-file.path("resourceFiles",
                   "testModel")
  file.create(file.path(logs, "error.log"))

  testthat::expect_message(modelClass$train(train.set = train.set,
                                            fitting = fitting,
                                            trFunction = trFunction,
                                            metric = metric,
                                            logs = logs),
                           "\\[Model\\]\\[INFO\\]\\[lda\\] Finished in \\[[0-9.]+ segs\\]",
                           perl = TRUE)

  testthat::expect_message(modelClass$remove(),
                           "[Model][ERROR] Cannot remove unsaved model. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(modelClass$save(replace = FALSE),
                           "[Model][INFO][lda] Model succesfully saved at: ",
                           fixed = TRUE)

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "lda.rds")))

  modelClass$remove()

  testthat::expect_false(file.exists(file.path(dir.path,
                                               "lda.rds")))
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "dirpathRemove"))) {
    unlink(x = file.path("resourceFiles",
                         "testModel",
                         "dirpathRemove"),
           recursive = TRUE,
           force = TRUE)
  }
  if (file.exists(file.path("resourceFiles",
                            "testModel",
                            "error.log"))) {
    file.remove(file.path("resourceFiles",
                          "testModel",
                          "error.log"))
  }
})
