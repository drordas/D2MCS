testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TypeBasedStrategy: initialize function works", {

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
  configuration <- StrategyConfiguration$new()

  testthat::expect_is(suppressWarnings(TypeBasedStrategy$new(subset.cluster, heuristics, configuration)),
                      "TypeBasedStrategy")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TypeBasedStrategy: initialize function checks parameter type", {

  subset.cluster <- NULL
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(TypeBasedStrategy$new(subset.cluster, heuristics, configuration),
                         "[TypeBasedStrategy][initialize][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
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
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(TypeBasedStrategy$new(subset.cluster, heuristics, configuration),
                         "[TypeBasedStrategy][initialize][FATAL] Heuristic parameter is not defined or incorrect. Must contain two elements. Aborting...",
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
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(TypeBasedStrategy$new(subset.cluster, heuristics, configuration),
                         "[TypeBasedStrategy][initialize][FATAL] Defined heuristics are not correct. Must be inherit from 'GenericHeuristic' class. Aborting...",
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
  configuration <- StrategyConfiguration$new()

  testthat::expect_warning(TypeBasedStrategy$new(subset.cluster, heuristics, configuration),
                           "[TypeBasedStrategy][initialize][WARN] Heuristic for binary data defined",
                           fixed = TRUE)

  testthat::expect_warning(TypeBasedStrategy$new(subset.cluster, heuristics, configuration),
                           "[TypeBasedStrategy][initialize][WARN] Heuristic for real data defined",
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
  configuration <- StrategyConfiguration$new()

  testthat::expect_warning(TypeBasedStrategy$new(subset.cluster, heuristics, configuration),
                           "[TypeBasedStrategy][initialize][WARN] Heuristic for binary data not defined",
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
  configuration <- StrategyConfiguration$new()

  testthat::expect_warning(TypeBasedStrategy$new(subset.cluster, heuristics, configuration),
                           "[TypeBasedStrategy][initialize][WARN] Heuristic for real data not defined",
                           fixed = TRUE)
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "outputs"),
                                winslash = "/",
                                mustWork = FALSE))) {
    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "outputs"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)
  }
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("TypeBasedStrategy works", {
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
  configuration <- StrategyConfiguration$new()

  strategy <- suppressWarnings(TypeBasedStrategy$new(subset.cluster,
                                                     heuristics,
                                                     configuration))

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- StrategyConfiguration$new()

  strategyNoReal <- suppressWarnings(TypeBasedStrategy$new(subset.cluster,
                                                           list(ChiSquareHeuristic$new(),
                                                                NULL),
                                                           configuration))
  strategyNoBinary <- suppressWarnings(TypeBasedStrategy$new(subset.cluster,
                                                             list(NULL,
                                                                  SpearmanHeuristic$new()),
                                                             configuration))

  testthat::expect_message(suppressWarnings(strategyNoReal$execute()),
                           "[TypeBasedStrategy][execute][INFO] TypeBasedStrategy has not heuristic to real features. Assuming one cluster by default",
                           fixed = TRUE)

  testthat::expect_equal(c("gg", "ggplot"), class(strategyNoReal$plot()))

  testthat::expect_message(suppressWarnings(strategyNoBinary$execute()),
                           "[TypeBasedStrategy][execute][INFO] TypeBasedStrategy has not heuristic to binary features. Assuming one cluster by default",
                           fixed = TRUE)

  grDevices::pdf(NULL)

  testthat::expect_equal(c("gg", "ggplot"), class(strategyNoBinary$plot()))

  testthat::expect_equal(length(strategyNoReal$getDistribution(num.groups = c(1, 1))),
                         2)

  testthat::expect_warning(strategyNoReal$getDistribution(num.groups = c(5, 1)),
                           "[TypeBasedStrategy][getDistribution][WARN] Number of clusters incorrect. Returning all groups...",
                           fixed = TRUE)

  testthat::expect_warning(strategyNoReal$getDistribution(num.groups = c(1, 5)),
                           "[TypeBasedStrategy][getDistribution][WARN] Number of clusters incorrect. Returning all groups...",
                           fixed = TRUE)

  strategy <- suppressWarnings(TypeBasedStrategy$new(subset.cluster,
                                                     heuristics,
                                                     configuration))

  testthat::expect_error(strategy$getDistribution(),
                         "[TypeBasedStrategy][getDistribution][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[TypeBasedStrategy][createTrain][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)
  testthat::expect_error(suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                                     "outputs",
                                                                                                     "saveCSV"),
                                                                                    winslash = "/",
                                                                                    mustWork = FALSE))),
                         "[TypeBasedStrategy][saveCSV][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()),
                         4)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.clusters = 2))),
                         2)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.clusters = 1:2))),
                         2)
  testthat::expect_equal(length(suppressWarnings(strategy$getDistribution(num.groups = 1))),
                         3)

  testthat::expect_equal(length(strategy$getDistribution(include.unclustered = TRUE)),
                         4)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[TypeBasedStrategy][createTrain][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_equal(c("gtable", "gTree", "grob", "gDesc"), class(strategy$plot()))

  testthat::expect_message(strategy$plot(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                   "outputs",
                                                                                   "plots"),
                                                                  winslash = "/",
                                                                  mustWork = FALSE),
                                         file.name = "TypeBasedStrategyPlot"),
                           "[TypeBasedStrategy][plot][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[TypeBasedStrategy][saveCSV][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE)),
                           "[TypeBasedStrategy][saveCSV][WARN] File name not defined. Using 'ChiSquareHeuristic-SpearmanHeuristic.csv'",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE)),
                           "[TypeBasedStrategy][saveCSV][WARN] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = 2),
                           "[TypeBasedStrategy][saveCSV][WARN] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_warning(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                                      "outputs",
                                                                                      "saveCSV"),
                                                                     winslash = "/",
                                                                     mustWork = FALSE),
                                            num.clusters = list(list(2:60), list(2:60))),
                           "[TypeBasedStrategy][saveCSV][WARN] Number of clusters incorrect. Must be between 2 and 50. Ignoring clustering for real type features...",
                           fixed = TRUE)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                              "outputs",
                                                                              "saveCSV"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(list(3:6), list(3:6))))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputs",
                                                                             "saveCSV",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         8)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                              "outputs",
                                                                              "saveCSV2"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(NULL, list(3:6))))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputs",
                                                                             "saveCSV2",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         53)

  suppressWarnings(strategy$saveCSV(dir.path = normalizePath(path = file.path(tempdir(),
                                                                              "outputs",
                                                                              "saveCSV3"),
                                                             winslash = "/",
                                                             mustWork = FALSE),
                                    num.clusters = list(list(3:6), NULL)))

  testthat::expect_equal(nrow(read.csv(file = normalizePath(path = file.path(tempdir(),
                                                                             "outputs",
                                                                             "saveCSV3",
                                                                             "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                                            winslash = "/",
                                                            mustWork = FALSE),
                                       header = TRUE,
                                       sep = ";")),
                         53)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "outputs"),
                               winslash = "/",
                               mustWork = FALSE))) {
    unlink(x = normalizePath(path = file.path(tempdir(),
                                              "outputs"),
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

testthat::test_that("TypeBasedStrategy checks incompatible heuristics", {

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

  configuration <- StrategyConfiguration$new()

  strategyIncompatibleBinary <- suppressWarnings(TypeBasedStrategy$new(subset.cluster,
                                                                       list(KendallHeuristic$new(),
                                                                            NULL),
                                                                       configuration))
  strategyIncompatibleReal <- suppressWarnings(TypeBasedStrategy$new(subset.cluster,
                                                                     list(NULL,
                                                                          OddsRatioHeuristic$new()),
                                                                     configuration))

  testthat::expect_warning(strategyIncompatibleBinary$execute(),
                           "[TypeBasedStrategy][execute][WARN] 23 features were incompatible with 'KendallHeuristic' heuristic",
                           fixed = TRUE)

  testthat::expect_warning(strategyIncompatibleReal$execute(),
                           "[TypeBasedStrategy][execute][WARN] 26 features were incompatible with 'OddsRatioHeuristic' heuristic",
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

  configuration <- StrategyConfiguration$new()

  subset.cluster.no.binary <- Subset$new(dataset = data.frame(c(0, 1, 0, 1),
                                                              c(0.2, 1.6, 5.12, 3.1),
                                                              c(0.1, 2.4, 6.89, 10.5)),
                                         class.index = 1,
                                         class.values = as.factor(c(0, 1)),
                                         positive.class = 1)

  strategyNoBinaryFeatures <- suppressWarnings(TypeBasedStrategy$new(subset.cluster.no.binary,
                                                                     list(ChiSquareHeuristic$new(),
                                                                          NULL),
                                                                     configuration))

  testthat::expect_message(suppressWarnings(strategyNoBinaryFeatures$execute()),
                           "[TypeBasedStrategy][execute][INFO] Not binary features for clustering",
                           fixed = TRUE)

  subset.cluster.no.real <- Subset$new(dataset = data.frame(c(0, 1, 0, 1),
                                                            c(0, 1, 0, 1),
                                                            c(0, 1, 1, 1)),
                                       class.index = 1,
                                       class.values = as.factor(c(0, 1)),
                                       positive.class = 1)

  strategyNoRealFeatures <- suppressWarnings(TypeBasedStrategy$new(subset.cluster.no.real,
                                                                   list(NULL,
                                                                        SpearmanHeuristic$new()),
                                                                   configuration))

  testthat::expect_message(suppressWarnings(strategyNoRealFeatures$execute()),
                           "[TypeBasedStrategy][execute][INFO] Not real features for clustering",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
