testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: initialize function checks parameter type", {

  testthat::expect_error(ClassificationOutput$new(voting.schemes = NULL, models = list()),
                         "[ClassificationOutput][initialize][FATAL] Voting Schemes not executed. Aborting...",
                         fixed = TRUE)
  voting.schemes <- list("a")
  names(voting.schemes) <- "wrong"
  testthat::expect_error(ClassificationOutput$new(voting.schemes = voting.schemes, models = list()),
                         "[ClassificationOutput][initialize][FATAL] Voting Schemes must inherit from 'SimpleVoting' or 'CombinedVoting' classes. Aborting...",
                         fixed = TRUE)
  voting.schemes <- c(SingleVoting$new(voting.schemes = c(ClassWeightedVoting$new(cutoff = 0.7)),
                      metrics = c("MCC")))
  names(voting.schemes) <- "SingleVoting"
  testthat::expect_error(ClassificationOutput$new(voting.schemes = voting.schemes,
                                                  models = NULL),
                         "[ClassificationOutput][initialize][FATAL] Models parameter must be defined as 'list' type. Aborting...",
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

testthat::test_that("ClassificationOutput: getMetrics function works", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  testthat::expect_equal(classificationOutput$getMetrics(), c("MCC", "PPV"))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: getPositiveClass function works", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  testthat::expect_equal(classificationOutput$getPositiveClass(), 1)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: getModelInfo function works", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  testthat::expect_warning(classificationOutput$getModelInfo(),
                           "[ClassificationOutput][getModelInfo][WARN] Metrics are not defined or invalid. Asuming all metrics of clasification.output (MCC,PPV)",
                           fixed = TRUE)

  testthat::expect_is(suppressWarnings(classificationOutput$getModelInfo()),
                      "list")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: getPerformances function works", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  test.set <- data$createSubset(num.folds = 3,
                                class.index = "Class",
                                positive.class = "1")
  measures <- c(PPV$new(), MCC$new())

  testthat::expect_is(classificationOutput$getPerformances(test.set = test.set,
                                                           measures = measures,
                                                           voting.names = "ClassWeightedVoting",
                                                           metric.names = "MCC",
                                                           cutoff.values = 0.7),
                      "list")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: getPerformances function checks parameter type", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  measures <- c(PPV$new(), MCC$new())

  testthat::expect_error(classificationOutput$getPerformances(test.set = NULL,
                                                              measures = measures,
                                                              voting.names = "ClassWeightedVoting",
                                                              metric.names = "MCC",
                                                              cutoff.values = 0.7),
                         "[ClassificationOutput][getPerformances][FATAL] Testset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)


  subset <- Subset$new(dataset = data.frame(c(2, 1), c(2, 1)),
                       class.index = 1,
                       class.values = as.factor(c(2, 1)),
                       positive.class = 1)

  testthat::expect_error(classificationOutput$getPerformances(test.set = subset,
                                                              measures = measures,
                                                              voting.names = "ClassWeightedVoting",
                                                              metric.names = "MCC",
                                                              cutoff.values = 0.7),
                         "[ClassificationOutput][getPerformances][FATAL] Predicted values and Real values missmatch. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(classificationOutput$getPerformances(test.set = NULL,
                                                              measures = measures,
                                                              voting.names = "ClassWeightedVoting",
                                                              metric.names = "MCC",
                                                              cutoff.values = 0.7),
                         "[ClassificationOutput][getPerformances][FATAL] Testset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)
  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  test.set <- data$createSubset(num.folds = 3,
                                class.index = "Class",
                                positive.class = "1")

  testthat::expect_error(classificationOutput$getPerformances(test.set = test.set,
                                                              measures = NULL,
                                                              voting.names = "ClassWeightedVoting",
                                                              metric.names = "MCC",
                                                              cutoff.values = 0.7),
                         "[ClassificationOutput][getPerformances][FATAL] Measures should be a list comprised of 'MeasureFunction' objects. Aborting...",
                         fixed = TRUE)

  testthat::expect_warning(classificationOutput$getPerformances(test.set = test.set,
                                                                measures = measures,
                                                                voting.names = "ClassWeightedVoting",
                                                                metric.names = "MCC",
                                                                cutoff.values = 0),
                         "[ClassificationOutput][getPerformances][WARN] Defined cutoffs are not available. Using all cutoffs",
                         fixed = TRUE)

  testthat::expect_warning(classificationOutput$getPerformances(test.set = test.set,
                                                                measures = measures,
                                                                voting.names = "ClassWeightedVoting",
                                                                metric.names = "a",
                                                                cutoff.values = 0.7),
                           "[ClassificationOutput][getPerformances][WARN] Defined metrics are not available. Using all metrics",
                           fixed = TRUE)

  testthat::expect_warning(classificationOutput$getPerformances(test.set = test.set,
                                                                measures = measures,
                                                                voting.names = "a",
                                                                metric.names = "MCC",
                                                                cutoff.values = 0.7),
                           "[ClassificationOutput][getPerformances][WARN] Defined votings are not available. Using all votings",
                           fixed = TRUE)

  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  test.set <- data$createSubset(num.folds = 3,
                                class.index = "Class",
                                positive.class = "1")
  test.set$.__enclos_env__$private$positive.class <- "0"
  testthat::expect_error(classificationOutput$getPerformances(test.set = test.set,
                                                              measures = measures,
                                                              voting.names = "a",
                                                              metric.names = "MCC",
                                                              cutoff.values = 0.7),
                         "[ClassificationOutput][getPerformances][FATAL] Positive class values missmatch. ['0' vs '1'] used in classification and test respectively. Aborting...",
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

testthat::test_that("ClassificationOutput: savePerformances function works", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "ClassificationOutput"),
                            winslash = "/",
                            mustWork = FALSE)
  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  test.set <- data$createSubset(num.folds = 3,
                                class.index = "Class",
                                positive.class = "1")
  measures <- c(MCC$new())
  testthat::expect_message(classificationOutput$savePerformances(dir.path = dir.path,
                                                                 test.set = test.set,
                                                                 measures = measures),
                          "\\[ClassificationOutput\\]\\[savePerformances\\]\\[INFO\\] Classification performance saved at: [A-Za-z\\\\/_.]+")
  testthat::expect_message(classificationOutput$savePerformances(dir.path = dir.path,
                                                                 test.set = test.set,
                                                                 measures = measures),
                           "[ClassificationOutput][savePerformances][INFO] Folder already exists",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "ClassificationOutput"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "ClassificationOutput"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: savePerformances function checks parameter type", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "ClassificationOutput"),
                            winslash = "/",
                            mustWork = FALSE)
  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  test.set <- data$createSubset(num.folds = 3,
                                class.index = "Class",
                                positive.class = "1")
  measures <- c(MCC$new())
  testthat::expect_error(classificationOutput$savePerformances(dir.path = NULL,
                                                               test.set = test.set,
                                                               measures = measures),
                         "[ClassificationOutput][savePerformances][FATAL] Save folder not set. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "ClassificationOutput"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "ClassificationOutput"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: plotPerformances function works", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "ClassificationOutput"),
                            winslash = "/",
                            mustWork = FALSE)
  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  test.set <- data$createSubset(num.folds = 3,
                                class.index = "Class",
                                positive.class = "1")
  measures <- c(MCC$new())
  testthat::expect_message(classificationOutput$plotPerformances(dir.path = dir.path,
                                                                 test.set = test.set,
                                                                 measures = measures),
                           "\\[ClassificationOutput\\]\\[plotPerformances\\]\\[INFO\\] Plot has been succesfully saved at: [A-Za-z\\\\/_.]+")

  testthat::expect_message(classificationOutput$plotPerformances(dir.path = dir.path,
                                                                 test.set = test.set,
                                                                 measures = measures),
                           "[ClassificationOutput][plotPerformances][INFO] Folder already exists",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "ClassificationOutput"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "ClassificationOutput"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: plotPerformances function checks parameter type", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "ClassificationOutput"),
                            winslash = "/",
                            mustWork = FALSE)
  set.seed(1234)
  file.path <-  file.path("resourceFiles",
                          "data",
                          "hcc-data-complete-balanced.csv")

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4, class.balance = "Class")

  test.set <- data$createSubset(num.folds = 3,
                                class.index = "Class",
                                positive.class = "1")
  measures <- c(MCC$new())
  testthat::expect_error(classificationOutput$plotPerformances(dir.path = NULL,
                                                               test.set = test.set,
                                                               measures = measures),
                         "[ClassificationOutput][plotPerformances][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "ClassificationOutput"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "ClassificationOutput"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: getPredictions function works", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  voting.names <- NULL
  metric.names <- NULL
  cutoff.values <- NULL
  type <- NULL
  target <- NULL
  filter <- FALSE

  testthat::expect_is(suppressWarnings(classificationOutput$getPredictions(voting.names = voting.names,
                                                                           metric.names = metric.names,
                                                                           cutoff.values = cutoff.values,
                                                                           type = type,
                                                                           target = target,
                                                                           filter = filter)),
                      "PredictionOutput")

  testthat::expect_is(suppressWarnings(classificationOutput$getPredictions(voting.names = "ClassWeightedVoting",
                                                                           metric.names = "MCC",
                                                                           cutoff.values = 0.7,
                                                                           type = type,
                                                                           target = target,
                                                                           filter = filter)),
                      "PredictionOutput")

  testthat::expect_warning(classificationOutput$getPredictions(voting.names = voting.names,
                                                               metric.names = metric.names,
                                                               cutoff.values = cutoff.values,
                                                               type = "prob",
                                                               target = NULL,
                                                               filter = filter),
                           "[ClassificationOutput][getPredictions][WARN] Target value does not match with actual target values: '1, 0'. Assuming '1' as default value",
                           fixed = TRUE)

  testthat::expect_warning(classificationOutput$getPredictions(voting.names = voting.names,
                                                               metric.names = metric.names,
                                                               cutoff.values = cutoff.values,
                                                               type = type,
                                                               target = target,
                                                               filter = NULL),
                           "[ClassificationOutput][getPredictions][WARN] Filter parameter must be defined as 'logical' type. Assuming 'FALSE' as default value",
                           fixed = TRUE)

  testthat::expect_warning(classificationOutput$getPredictions(voting.names = voting.names,
                                                               metric.names = metric.names,
                                                               cutoff.values = 0.2,
                                                               type = type,
                                                               target = target,
                                                               filter = filter),
                           "[ClassificationOutput][getPredictions][WARN] Defined Cutoffs are not available. Using all available cutoffs",
                           fixed = TRUE)

  testthat::expect_warning(classificationOutput$getPredictions(voting.names = voting.names,
                                                               metric.names = "wrong",
                                                               cutoff.values = cutoff.values,
                                                               type = type,
                                                               target = target,
                                                               filter = filter),
                           "[ClassificationOutput][getPredictions][WARN] Defined Metrics are not available. Using all available metrics",
                           fixed = TRUE)

  testthat::expect_warning(classificationOutput$getPredictions(voting.names = "wrong",
                                                               metric.names = metric.names,
                                                               cutoff.values = cutoff.values,
                                                               type = type,
                                                               target = target,
                                                               filter = filter),
                           "[ClassificationOutput][getPredictions][WARN] Defined Votings are not available. Using all available votings",
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

testthat::test_that("ClassificationOutput: savePredictions function works", {

  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  dir.path <- normalizePath(path = file.path(tempdir(),
                                             "ClassificationOutput"),
                            winslash = "/",
                            mustWork = FALSE)
  voting.names <- NULL
  metric.names <- NULL
  cutoff.values <- NULL
  type <- NULL
  target <- NULL
  filter <- FALSE

  suppressWarnings(classificationOutput$savePredictions(dir.path = dir.path,
                                                        voting.names = voting.names,
                                                        metric.names = metric.names,
                                                        cutoff.values = cutoff.values,
                                                        type = type,
                                                        target = target,
                                                        filter = filter))

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "MCC_0.7_ClassWeightedVoting_raw_Predictions.csv")))

  testthat::expect_message(suppressWarnings(classificationOutput$savePredictions(dir.path = dir.path,
                                                                                 voting.names = voting.names,
                                                                                 metric.names = metric.names,
                                                                                 cutoff.values = cutoff.values,
                                                                                 type = type,
                                                                                 target = target,
                                                                                 filter = filter)),
                           "[ClassificationOutput][savePredictions][INFO] Folder already exists",
                           fixed = TRUE)

  suppressWarnings(classificationOutput$savePredictions(dir.path = dir.path,
                                                        voting.names = voting.names,
                                                        metric.names = metric.names,
                                                        cutoff.values = cutoff.values,
                                                        type = type,
                                                        target = "0",
                                                        filter = filter))

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "MCC_0.7_ClassWeightedVoting_raw_0.csv")))

  suppressWarnings(classificationOutput$savePredictions(dir.path = dir.path,
                                                        voting.names = voting.names,
                                                        metric.names = metric.names,
                                                        cutoff.values = cutoff.values,
                                                        type = "raw",
                                                        target = target,
                                                        filter = filter))

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "MCC_0.7_ClassWeightedVoting_1.csv")))

  suppressWarnings(classificationOutput$savePredictions(dir.path = dir.path,
                                                        voting.names = voting.names,
                                                        metric.names = metric.names,
                                                        cutoff.values = cutoff.values,
                                                        type = "raw",
                                                        target = "0",
                                                        filter = filter))

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "MCC_0.7_ClassWeightedVoting_0.csv")))

  classificationOutput$savePredictions(dir.path = dir.path,
                                       voting.names = "ClassWeightedVoting",
                                       metric.names = "MCC",
                                       cutoff.values = 0.7,
                                       type = type,
                                       target = target,
                                       filter = filter)

  testthat::expect_true(file.exists(file.path(dir.path,
                                              "MCC_0.7_ClassWeightedVoting_raw_Predictions.csv")))

  testthat::expect_warning(classificationOutput$savePredictions(dir.path = dir.path,
                                                                voting.names = voting.names,
                                                                metric.names = metric.names,
                                                                cutoff.values = 0.2,
                                                                type = type,
                                                                target = target,
                                                                filter = filter),
                           "[ClassificationOutput][savePredictions][WARN] Defined cutoffs are not available. Using all cutoffs",
                           fixed = TRUE)

  testthat::expect_warning(classificationOutput$savePredictions(dir.path = dir.path,
                                                                voting.names = voting.names,
                                                                metric.names = "wrong",
                                                                cutoff.values = cutoff.values,
                                                                type = type,
                                                                target = target,
                                                                filter = filter),
                           "[ClassificationOutput][savePredictions][WARN] Defined metrics are not available. Using all metrics",
                           fixed = TRUE)

  testthat::expect_warning(classificationOutput$savePredictions(dir.path = dir.path,
                                                                voting.names = "wrong",
                                                                metric.names = metric.names,
                                                                cutoff.values = cutoff.values,
                                                                type = type,
                                                                target = target,
                                                                filter = filter),
                           "[ClassificationOutput][savePredictions][WARN] Defined votings are not available. Using all votings",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "ClassificationOutput"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "ClassificationOutput"),
                             winslash = "/",
                             mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("ClassificationOutput: savePredictions function checks parameter type", {
  classWeightedVotingProb <- readRDS(file.path("resourceFiles",
                                               "data",
                                               "classWeightedVotingProb.rds"))

  classWeightedVotingRaw <- readRDS(file.path("resourceFiles",
                                              "data",
                                              "classWeightedVotingRaw.rds"))

  final.pred <- D2MCS:::FinalPred$new()
  final.pred$set(prob = classWeightedVotingProb,
                 raw = classWeightedVotingRaw,
                 class.values = c(1, 0),
                 positive.class = 1)

  classWeightedVoting <- ClassWeightedVoting$new(cutoff = 0.7,
                                                 weights = c(0.549, 0.382, 0.385))

  classWeightedVoting$.__enclos_env__$private$final.pred <- final.pred

  voting.schemes <- list("SingleVoting" =
                           list("MCC" =
                                  list("0.7" =
                                         list(ClassWeightedVoting = classWeightedVoting))))

  trained.models <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "trainedModels.rds"))

  classificationOutput <- ClassificationOutput$new(voting.schemes = voting.schemes,
                                                   models = trained.models)

  voting.names <- NULL
  metric.names <- NULL
  cutoff.values <- NULL
  type <- NULL
  target <- NULL
  filter <- FALSE

  testthat::expect_error(classificationOutput$savePredictions(dir.path = NULL,
                                                              voting.names = voting.names,
                                                              metric.names = metric.names,
                                                              cutoff.values = cutoff.values,
                                                              type = type,
                                                              target = target,
                                                              filter = filter),
                           "[ClassificationOutput][savePredictions][FATAL] Save folder not set. Aborting...",
                           fixed = TRUE)
})


testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
