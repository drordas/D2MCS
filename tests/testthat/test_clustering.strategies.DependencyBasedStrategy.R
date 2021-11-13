testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("DependencyBasedStrategy: initialize function works", {
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_is(DependencyBasedStrategy$new(subset = subset.cluster,
                                                  heuristic = heuristics,
                                                  configuration = configuration),
                      "DependencyBasedStrategy")

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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][initialize][INFO] Heuristic for binary data defined as 'ChiSquareHeuristic'",
                           fixed = TRUE)

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][initialize][INFO] Heuristic for real data defined as 'SpearmanHeuristic'",
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

testthat::test_that("DependencyBasedStrategy: initialize function checks parameter type", {

  subset.cluster <- NULL
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- NULL
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Heuristic parameter is not defined or incorrect. Must contain two elements. Aborting...",
                         fixed = TRUE, )

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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(1, 1)
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Defined heuristics are not correct. Must be inherit from 'GenericHeuristic' class. Aborting...",
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(NULL, SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_warning(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][initialize][WARN] Heuristic for binary data not defined",
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), NULL)
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_warning(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][initialize][WARN] Heuristic for real data not defined",
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- NULL

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Configuration parameter must be inherit from 'StrategyConfiguration' class. Aborting...",
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

testthat::test_that("DependencyBasedStrategy: checks configuration object", {

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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- R6::R6Class(classname = "TestConfiguration",
                               inherit = StrategyConfiguration,
                               public = list(
                                 initialize = function() { "test" },
                                 getRealCutoff = function() { "test" },
                                 tiebreak = function() { "test" },
                                 qualityOfCluster = function() { "test" },
                                 isImprovingClustering = function() { "test" }))$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Configuration parameter must have 'getBinaryCutoff' method. Aborting...",
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- R6::R6Class(classname = "TestConfiguration",
                               inherit = StrategyConfiguration,
                               public = list(
                                 initialize = function() { "test" },
                                 getBinaryCutoff = function() { "test" },
                                 tiebreak = function() { "test" },
                                 qualityOfCluster = function() { "test" },
                                 isImprovingClustering = function() { "test" }))$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Configuration parameter must have 'getRealCutoff' method. Aborting...",
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- R6::R6Class(classname = "TestConfiguration",
                               inherit = StrategyConfiguration,
                               public = list(
                                 initialize = function() { "test" },
                                 getBinaryCutoff = function() { "test" },
                                 getRealCutoff = function() { "test" },
                                 qualityOfCluster = function() { "test" },
                                 isImprovingClustering = function() { "test" }))$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Configuration parameter must have 'tiebreak' method. Aborting...",
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- R6::R6Class(classname = "TestConfiguration",
                               inherit = StrategyConfiguration,
                               public = list(
                                 initialize = function() { "test" },
                                 getBinaryCutoff = function() { "test" },
                                 getRealCutoff = function() { "test" },
                                 tiebreak = function() { "test" },
                                 isImprovingClustering = function() { "test" }))$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Configuration parameter must have 'qualityOfCluster' method. Aborting...",
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- R6::R6Class(classname = "TestConfiguration",
                               inherit = StrategyConfiguration,
                               public = list(
                                 initialize = function() { "test" },
                                 getBinaryCutoff = function() { "test" },
                                 getRealCutoff = function() { "test" },
                                 tiebreak = function() { "test" },
                                 qualityOfCluster = function() { "test" }))$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][initialize][FATAL] Configuration parameter must have 'isImprovingClustering' method. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "outputslfdc"),
                                winslash = "/",
                                mustWork = FALSE))) {
    dir.create(normalizePath(path = file.path(tempdir(),
                                              "outputslfdc"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("DependencyBasedStrategy works with 'lfdc' tiebreak method", {
  testthat::skip_if_not_installed("grDevices")
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new(tiebreakMethod = 'lfdc',
                                                            realCutoff = .9)

  strategy <- DependencyBasedStrategy$new(subset = subset.cluster,
                                          heuristic = heuristics,
                                          configuration = configuration)

  testthat::expect_error(strategy$getDistribution(),
                         "[DependencyBasedStrategy][getDistribution][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[DependencyBasedStrategy][createTrain][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(suppressWarnings(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputslfdc", "saveCSV"))),
                         "[DependencyBasedStrategy][saveCSV][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()),
                         4)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.clusters = 1))),
                         0)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.clusters = 1:2))),
                         2)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.clusters = 1:3))),
                         2)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.groups = 1))),
                         3)

  testthat::expect_equal(length(strategy$getDistribution(num.groups = c(1, 1))),
                         2)

  testthat::expect_warning(strategy$getDistribution(num.groups = c(5, 1)),
                           "[DependencyBasedStrategy][getDistribution][WARN] Number of clusters incorrect. Returning all groups...",
                           fixed = TRUE)

  testthat::expect_warning(strategy$getDistribution(num.groups = c(1, 5)),
                           "[DependencyBasedStrategy][getDistribution][WARN] Number of clusters incorrect. Returning all groups...",
                           fixed = TRUE)

  testthat::expect_equal(length(strategy$getDistribution(include.unclustered = TRUE)), 4)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[DependencyBasedStrategy][createTrain][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  grDevices::pdf(NULL)

  testthat::expect_equal(c("gtable", "gTree", "grob", "gDesc"), class(strategy$plot()))

  testthat::expect_message(strategy$plot(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                   "outputslfdc",
                                                                                   "plots"),
                                                                  winslash = "/",
                                                                  mustWork = FALSE),
                                         file.name = "TypeBasedStrategyPlot"),
                           "[DependencyBasedStrategy][plot][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[DependencyBasedStrategy][saveCSV][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputslfdc",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE)),
                           "[DependencyBasedStrategy][saveCSV][WARN] File name not defined. Using 'ChiSquareHeuristic-SpearmanHeuristic.csv'",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputslfdc",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE)),
                           "[DependencyBasedStrategy][saveCSV][WARN] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputslfdc",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = 2),
                           "[DependencyBasedStrategy][saveCSV][WARN] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputslfdc",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = list(list(2:60), list(2:60))),
                           paste0("[DependencyBasedStrategy][saveCSV][WARN] Number of clusters incorrect. Must be between 2 and ", max(strategy$.__enclos_env__$private$all.distribution[[2]]$k), ". Ignoring clustering for real type features..."),
                           fixed = TRUE)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                              "outputslfdc",
                                                                              "saveCSV"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(list(2:3),
                                                        list(2:3))))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputslfdc",
                                                                             "saveCSV",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         4)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                              "outputslfdc",
                                                                              "saveCSV2"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(NULL,
                                                        list(2:3))))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputslfdc",
                                                                             "saveCSV2",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         4)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                              "outputslfdc",
                                                                              "saveCSV3"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(list(2:3),
                                                        NULL)))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputslfdc",
                                                                             "saveCSV3",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         4)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "outputslfdc"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(normalizePath(path = file.path(tempdir(),
                                          "outputslfdc"),
                         winslash = "/",
                         mustWork = FALSE),
           recursive = TRUE,
           force = TRUE)
  }
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "outputsltdc"),
                                winslash = "/",
                                mustWork = FALSE))) {
    dir.create(normalizePath(path = file.path(tempdir(),
                                              "outputsltdc"),
                             winslash = "/",
                             mustWork = FALSE))
  }
})

testthat::test_that("DependencyBasedStrategy works with 'ltdc' tiebreak method", {
  testthat::skip_if_not_installed("grDevices")
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new(tiebreakMethod = "ltdc")

  strategy <- DependencyBasedStrategy$new(subset.cluster, heuristics, configuration)


  testthat::expect_error(strategy$getDistribution(),
                         "[DependencyBasedStrategy][getDistribution][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[DependencyBasedStrategy][createTrain][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)
  testthat::expect_error(suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                                     "outputsltdc",
                                                                                                     "saveCSV"),
                                                                                    winslash = "/",
                                                                                    mustWork = FALSE))),
                         "[DependencyBasedStrategy][saveCSV][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()),
                         4)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.clusters = 1))),
                         0)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.clusters = 1:2))),
                         2)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.groups = 1))),
                         3)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(include.unclustered = TRUE))),
                         4)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[DependencyBasedStrategy][createTrain][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  grDevices::pdf(NULL)

  testthat::expect_equal(c("gtable", "gTree", "grob", "gDesc"), class(strategy$plot()))

  testthat::expect_message(strategy$plot(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                   "outputsltdc",
                                                                                   "plots"),
                                                                  winslash = "/",
                                                                  mustWork = FALSE),
                                         file.name = "TypeBasedStrategyPlot"),
                           "[DependencyBasedStrategy][plot][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[DependencyBasedStrategy][saveCSV][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputsltdc",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE)),
                           "[DependencyBasedStrategy][saveCSV][WARN] File name not defined. Using 'ChiSquareHeuristic-SpearmanHeuristic.csv'",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputsltdc",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE)),
                           "[DependencyBasedStrategy][saveCSV][WARN] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputsltdc",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = 2),
                           "[DependencyBasedStrategy][saveCSV][WARN] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputsltdc",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = list(list(2:60), list(2:60))),
                           paste0("[DependencyBasedStrategy][saveCSV][WARN] Number of clusters incorrect. Must be between 2 and ", max(strategy$.__enclos_env__$private$all.distribution[[2]]$k), ". Ignoring clustering for real type features..."),
                           fixed = TRUE)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                             "outputsltdc",
                                                                             "saveCSV"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(list(2:3), list(2:3))))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputsltdc",
                                                                             "saveCSV",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         4)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                              "outputsltdc",
                                                                              "saveCSV2"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(NULL,
                                                        list(2:3))))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputsltdc",
                                                                             "saveCSV2",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         4)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                             "outputsltdc",
                                                             "saveCSV3"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(list(2:3),
                                                        NULL)))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputsltdc",
                                                                             "saveCSV3",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         4)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "outputsltdc"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "outputsltdc"),
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

testthat::test_that("DependencyBasedStrategy checks incompatible heuristics", {
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

  subset.cluster <- data$createSubset(num.folds = c(1, 2),
                                      class.index = "Class",
                                      positive.class = "1")

  configuration <- DependencyBasedStrategyConfiguration$new()

  strategyIncompatibleBinary <- suppressWarnings(DependencyBasedStrategy$new(subset = subset.cluster,
                                                                             heuristic = list(KendallHeuristic$new(),
                                                                                              NULL),
                                                                             configuration = configuration))

  strategyIncompatibleReal <- suppressWarnings(DependencyBasedStrategy$new(subset = subset.cluster,
                                                                           heuristic = list(NULL,
                                                                                            OddsRatioHeuristic$new()),
                                                                           configuration = configuration))

  testthat::expect_message(strategyIncompatibleBinary$execute(),
                           "[DependencyBasedStrategy][execute][INFO] 44 features were incompatible with 'KendallHeuristic' heuristic",
                           fixed = TRUE)

  testthat::expect_message(strategyIncompatibleReal$execute(),
                           "[DependencyBasedStrategy][execute][INFO] 50 features were incompatible with 'OddsRatioHeuristic' heuristic",
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

testthat::test_that("TypeBasedStrategy checks no binary or real features", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  subset.cluster.no.binary <- Subset$new(dataset = data.frame(c(0, 1, 0, 1),
                                                              c(0.2, 1.6, 5.12, 3.1),
                                                              c(0.1, 2.4, 6.89, 10.5)),
                                         class.index = 1,
                                         class.values = as.factor(c(0, 1)),
                                         positive.class = 1)

  strategyNoBinaryFeatures <- suppressWarnings(DependencyBasedStrategy$new(subset = subset.cluster.no.binary,
                                                                           heuristic = list(ChiSquareHeuristic$new(),
                                                                                            NULL),
                                                                           configuration = configuration))

  testthat::expect_message(suppressWarnings(strategyNoBinaryFeatures$execute()),
                           "[DependencyBasedStrategy][execute][INFO] Not binary features for clustering",
                           fixed = TRUE)

  subset.cluster.no.real <- Subset$new(dataset = data.frame(c(0, 1, 0, 1),
                                                            c(0, 1, 0, 1),
                                                            c(0, 1, 1, 1)),
                                       class.index = 1,
                                       class.values = as.factor(c(0, 1)),
                                       positive.class = 1)

  strategyNoRealFeatures <- suppressWarnings(DependencyBasedStrategy$new(subset = subset.cluster.no.real,
                                                                         heuristic = list(NULL,
                                                                                          SpearmanHeuristic$new()),
                                                                         configuration = configuration))

  testthat::expect_message(suppressWarnings(strategyNoRealFeatures$execute()),
                           "[DependencyBasedStrategy][execute][INFO] Not real features for clustering",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
