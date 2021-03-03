testthat::test_that("ExecutedModels: initialize function works", {

  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "dirpathEmpty")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_true(file.exists(file.path("resourceFiles",
                                              "testExecutedModels",
                                              "dirpathEmpty",
                                              ".executed")))

  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "initializeTest")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_true(executedModels$exist(model.name = "lda"))

  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "initializeWithoutModel")

  testthat::expect_message(ExecutedModels$new(dir.path = dir.path),
                           "[ExecutedModels][WARNING] Best model cannot be loaded.",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testExecutedModels",
                            "dirpathEmpty"))) {
    unlink(x = file.path("resourceFiles",
                         "testExecutedModels",
                         "dirpathEmpty"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("ExecutedModels: getNames function works", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "dirpathEmpty")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_null(executedModels$getNames())

  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "initializeTest")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_equal(executedModels$getNames(), c("lda", "nb"))
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testExecutedModels",
                            "dirpathEmpty"))) {
    unlink(x = file.path("resourceFiles",
                         "testExecutedModels",
                         "dirpathEmpty"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::test_that("ExecutedModels: getBest function works", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "initializeTest")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_is(executedModels$getBest(), "list")
  testthat::expect_length(executedModels$getBest(), 4)

  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "dirpathEmpty")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_message(executedModels$getBest(),
                           "[ExecutedModels][WARNING] Best model not found.",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testExecutedModels",
                            "dirpathEmpty"))) {
    unlink(x = file.path("resourceFiles",
                         "testExecutedModels",
                         "dirpathEmpty"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!file.exists(file.path("resourceFiles",
                            "testExecutedModels",
                            "addTest-keepBest-FALSE",
                            "lda.rds"))) {
    file.copy(from = file.path("resourceFiles",
                               "testExecutedModels",
                               "lda.rds"),
              to = file.path("resourceFiles",
                             "testExecutedModels",
                             "addTest-keepBest-FALSE",
                             "lda.rds"))
  }
})

testthat::test_that("ExecutedModels: add function works (keep.best = FALSE)", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "addTest-keepBest-FALSE")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  model <- readRDS(file.path("resourceFiles",
                             "testExecutedModels",
                             "modelClassTrained.rds"))

  model$.__enclos_env__$private$dir.path <- dir.path
  model$.__enclos_env__$private$RDS.path <- file.path(dir.path,
                                                      "newModel.rds")

  keep.best <- FALSE

  executedModels$add(model = model,
                     keep.best = keep.best)

  testthat::expect_equal(executedModels$getNames(), c("lda", "nb", "newModel"))
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testExecutedModels",
                            "addTest-keepBest-FALSE",
                            "newModel.rds"))) {
    file.remove(file.path("resourceFiles",
                          "testExecutedModels",
                          "addTest-keepBest-FALSE",
                          "newModel.rds"))
  }
})

testthat::setup({
  if (!file.exists(file.path("resourceFiles",
                             "testExecutedModels",
                             "addTest-keepBest-TRUE",
                             "lda.rds"))) {
    file.copy(from = file.path("resourceFiles",
                               "testExecutedModels",
                               "lda.rds"),
              to = file.path("resourceFiles",
                             "testExecutedModels",
                             "addTest-keepBest-TRUE",
                             "lda.rds"))
  }
})

testthat::test_that("ExecutedModels: add function works (keep.best = TRUE)", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "addTest-keepBest-TRUE")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  model <- readRDS(file.path("resourceFiles",
                             "testExecutedModels",
                             "modelClassTrained.rds"))

  model$.__enclos_env__$private$dir.path <- dir.path
  model$.__enclos_env__$private$RDS.path <- file.path(dir.path,
                                                      "newModel.rds")

  testthat::expect_true(file.exists(file.path("resourceFiles",
                                              "testExecutedModels",
                                              "addTest-keepBest-TRUE",
                                              "lda.rds")))

  testthat::expect_false(file.exists(file.path("resourceFiles",
                                               "testExecutedModels",
                                               "addTest-keepBest-TRUE",
                                               "newModel.rds")))

  keep.best <- TRUE

  testthat::expect_message(executedModels$add(model = model,
                                              keep.best = keep.best),
                           "[ExecutedModels][INFO] Best model found. Replacing 'lda' with 'newModel'",
                           fixed = TRUE)

  testthat::expect_false(file.exists(file.path("resourceFiles",
                                               "testExecutedModels",
                                               "addTest-keepBest-TRUE",
                                               "lda.rds")))

  testthat::expect_true(file.exists(file.path("resourceFiles",
                                              "testExecutedModels",
                                              "addTest-keepBest-TRUE",
                                              "newModel.rds")))

  testthat::expect_equal(executedModels$getNames(), c("lda", "nb", "newModel"))
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testExecutedModels",
                            "addTest-keepBest-TRUE",
                            "newModel.rds"))) {
    file.remove(file.path("resourceFiles",
                          "testExecutedModels",
                          "addTest-keepBest-TRUE",
                          "newModel.rds"))
  }
})

testthat::test_that("ExecutedModels: add function checks parameter", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "initializeTest")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  model <- NULL
  keep.best <- TRUE

  testthat::expect_message(executedModels$add(model = model,
                                              keep.best = keep.best),
                           "[ExecutedModels][ERROR] Model parameter must be defined as 'Model' type. Model not inserted. Task not performed",
                           fixed = TRUE)
})

testthat::test_that("ExecutedModels: exist function works", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "initializeTest")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_true(executedModels$exist(model.name = "lda"))
  testthat::expect_false(executedModels$exist(model.name = list()))
  testthat::expect_false(executedModels$exist(model.name = "wrong"))
})

testthat::test_that("ExecutedModels: size function works", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "initializeTest")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_equal(executedModels$size(), 2)
})

testthat::test_that("ExecutedModels: save function (file is empty)", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "dirpathEmpty")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_message(executedModels$save(),
                           "[ExecutedModels][ERROR] File is empty. Task not performed",
                           fixed = TRUE)
})

testthat::teardown({
  if (file.exists(file.path("resourceFiles",
                            "testExecutedModels",
                            "dirpathEmpty"))) {
    unlink(x = file.path("resourceFiles",
                         "testExecutedModels",
                         "dirpathEmpty"),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  if (!file.exists(file.path("resourceFiles",
                             "testExecutedModels",
                             "deleteTest",
                             "lda.rds"))) {
    file.copy(from = file.path("resourceFiles",
                               "testExecutedModels",
                               "lda.rds"),
              to = file.path("resourceFiles",
                             "testExecutedModels",
                             "deleteTest",
                             "lda.rds"))
  }
})

testthat::test_that("ExecutedModels: delete function works", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "deleteTest")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  executedModels$delete(model.name = "lda")

  testthat::expect_false(file.exists(file.path("resourceFiles",
                                               "testExecutedModels",
                                               "deleteTest",
                                               "lda.rds")))
})

testthat::test_that("ExecutedModels: delete function checks parameter", {
  dir.path <- file.path("resourceFiles",
                        "testExecutedModels",
                        "deleteTestError")

  executedModels <- ExecutedModels$new(dir.path = dir.path)

  testthat::expect_message(executedModels$delete(model.name = "wrong"),
                           "[ExecutedModels][ERROR] Cannot delete model. Model 'wrong' has not been executed. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(executedModels$delete(model.name = "nb"),
                           "[ExecutedModels][ERROR] Cannot delete model. Path for model 'nb' not found. Task not performed",
                           fixed = TRUE)
})
