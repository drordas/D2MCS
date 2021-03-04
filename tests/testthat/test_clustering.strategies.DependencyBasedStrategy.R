testthat::test_that("DependencyBasedStrategy: initialize function works", {

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_is(DependencyBasedStrategy$new(subset = subset.cluster,
                                                  heuristic = heuristics,
                                                  configuration = configuration),
                      "DependencyBasedStrategy")

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][INFO] Heuristic for binary data defined",
                           fixed = TRUE)

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][INFO] Heuristic for real data defined",
                           fixed = TRUE)
})

testthat::test_that("DependencyBasedStrategy: initialize function checks parameter type", {

  subset.cluster <- NULL
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- NULL
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][FATAL] Heuristic parameter is not defined or incorrect. Must contain two elements. Aborting...",
                         fixed = TRUE, )

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- list(1, 1)
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][FATAL] Defined heuristics are not correct. Must be inherit from 'GenericHeuristic' class. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- list(NULL, SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][WARNING] Heuristic for binary data not defined",
                           fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), NULL)
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][WARNING] Heuristic for real data not defined",
                           fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- NULL

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][FATAL] Configuration parameter must be inherit from 'StrategyConfiguration' class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("DependencyBasedStrategy: checks configuration object", {

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
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
                         "[DependencyBasedStrategy][FATAL] Configuration parameter must have 'getBinaryCutoff' method. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
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
                         "[DependencyBasedStrategy][FATAL] Configuration parameter must have 'getRealCutoff' method. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
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
                         "[DependencyBasedStrategy][FATAL] Configuration parameter must have 'tiebreak' method. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
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
                         "[DependencyBasedStrategy][FATAL] Configuration parameter must have 'qualityOfCluster' method. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
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
                         "[DependencyBasedStrategy][FATAL] Configuration parameter must have 'isImprovingClustering' method. Aborting...",
                         fixed = TRUE)
})

testthat::setup({
  if (!dir.exists(file.path("resourceFiles",
                            "outputslfdc"))) {
    dir.create(file.path("resourceFiles",
                         "outputslfdc"))
  }
})

testthat::test_that("DependencyBasedStrategy works with 'lfdc' tiebreak method", {

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new(tiebreakMethod = 'lfdc',
                                                            realCutoff = .9)

  strategy <- DependencyBasedStrategy$new(subset = subset.cluster,
                                          heuristic = heuristics,
                                          configuration = configuration)

  testthat::expect_error(strategy$getDistribution(),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputslfdc", "saveCSV")),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()), 5)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1)), 0)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1:2)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1:3)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.groups = 1)), 4)

  testthat::expect_equal(length(strategy$getDistribution(num.groups = c(1, 1))),
                         2)

  testthat::expect_message(strategy$getDistribution(num.groups = c(5, 1)),
                           "[DependencyBasedStrategy][WARNING] Number of clusters incorrect. Returning all groups...",
                           fixed = TRUE)

  testthat::expect_message(strategy$getDistribution(num.groups = c(1, 5)),
                           "[DependencyBasedStrategy][WARNING] Number of clusters incorrect. Returning all groups...",
                           fixed = TRUE)

  testthat::expect_equal(length(strategy$getDistribution(include.unclustered = TRUE)), 5)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[DependencyBasedStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_equal(c("gtable", "gTree", "grob", "gDesc"), class(strategy$plot()))

  testthat::expect_message(strategy$plot(dir.path = file.path("resourceFiles",
                                                              "outputslfdc",
                                                              "plots"),
                                         file.name = "TypeBasedStrategyPlot"),
                           "[DependencyBasedStrategy][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[DependencyBasedStrategy][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                                 "outputslfdc",
                                                                 "saveCSV")),
                           "[DependencyBasedStrategy][WARNING] File name not defined. Using 'ChiSquareHeuristic-SpearmanHeuristic.csv'",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                                 "outputslfdc",
                                                                 "saveCSV")),
                           "[DependencyBasedStrategy][WARNING] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                                 "outputslfdc",
                                                                 "saveCSV"),
                                            num.clusters = 2),
                           "[DependencyBasedStrategy][WARNING] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                                 "outputslfdc",
                                                                 "saveCSV"),
                                            num.clusters = list(list(2:60), list(2:60))),
                           paste0("[DependencyBasedStrategy][WARNING] Number of clusters incorrect. Must be between 2 and ", max(strategy$.__enclos_env__$private$all.distribution[[2]]$k), ". Ignoring clustering for real type features..."),
                           fixed = TRUE)

  strategy$saveCSV(dir.path = file.path("resourceFiles",
                                        "outputslfdc",
                                        "saveCSV"),
                   num.clusters = list(list(2:3),
                                       list(2:3)))

  testthat::expect_equal(nrow(read.csv(file = file.path("resourceFiles",
                                                        "outputslfdc",
                                                        "saveCSV",
                                                        "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                       header = TRUE,
                                       sep = ";")),
                         4)

  strategy$saveCSV(dir.path = file.path("resourceFiles",
                                        "outputslfdc",
                                        "saveCSV2"),
                   num.clusters = list(NULL,
                                       list(2:3)))

  testthat::expect_equal(nrow(read.csv(file = file.path("resourceFiles",
                                                        "outputslfdc",
                                                        "saveCSV2",
                                                        "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                       header = TRUE,
                                       sep = ";")),
                         4)

  strategy$saveCSV(dir.path = file.path("resourceFiles",
                                        "outputslfdc",
                                        "saveCSV3"),
                   num.clusters = list(list(2:3),
                                       NULL))

  testthat::expect_equal(nrow(read.csv(file = file.path("resourceFiles",
                                                        "outputslfdc",
                                                        "saveCSV3",
                                                        "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                       header = TRUE,
                                       sep = ";")),
                         4)
})

testthat::teardown({
  if (dir.exists(file.path("resourceFiles", "outputslfdc"))) {
    unlink(file.path("resourceFiles", "outputslfdc"), recursive = TRUE, force = TRUE)
  }
})

testthat::setup({
  if (!dir.exists(file.path("resourceFiles",
                            "outputsltdc"))) {
    dir.create(file.path("resourceFiles",
                         "outputsltdc"))
  }
})

testthat::test_that("DependencyBasedStrategy works with 'ltdc' tiebreak method", {

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new(tiebreakMethod = "ltdc")

  strategy <- DependencyBasedStrategy$new(subset.cluster, heuristics, configuration)


  testthat::expect_error(strategy$getDistribution(),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)
  testthat::expect_error(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                               "outputsltdc",
                                                               "saveCSV")),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()), 4)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1)), 0)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1:2)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.groups = 1)), 3)
  testthat::expect_equal(length(strategy$getDistribution(include.unclustered = TRUE)), 4)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[DependencyBasedStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_equal(c("gtable", "gTree", "grob", "gDesc"), class(strategy$plot()))

  testthat::expect_message(strategy$plot(dir.path = file.path("resourceFiles",
                                                              "outputsltdc",
                                                              "plots"),
                                         file.name = "TypeBasedStrategyPlot"),
                           "[DependencyBasedStrategy][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[DependencyBasedStrategy][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                                 "outputsltdc",
                                                                 "saveCSV")),
                           "[DependencyBasedStrategy][WARNING] File name not defined. Using 'ChiSquareHeuristic-SpearmanHeuristic.csv'",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                                 "outputsltdc",
                                                                 "saveCSV")),
                           "[DependencyBasedStrategy][WARNING] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                                 "outputsltdc",
                                                                 "saveCSV"),
                                            num.clusters = 2),
                           "[DependencyBasedStrategy][WARNING] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles",
                                                                 "outputsltdc",
                                                                 "saveCSV"),
                                            num.clusters = list(list(2:60), list(2:60))),
                           paste0("[DependencyBasedStrategy][WARNING] Number of clusters incorrect. Must be between 2 and ", max(strategy$.__enclos_env__$private$all.distribution[[2]]$k), ". Ignoring clustering for real type features..."),
                           fixed = TRUE)

  strategy$saveCSV(dir.path = file.path("resourceFiles",
                                        "outputsltdc",
                                        "saveCSV"),
                   num.clusters = list(list(2:3), list(2:3)))

  testthat::expect_equal(nrow(read.csv(file = file.path("resourceFiles",
                                                        "outputsltdc",
                                                        "saveCSV",
                                                        "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                       header = TRUE,
                                       sep = ";")),
                         4)

  strategy$saveCSV(dir.path = file.path("resourceFiles",
                                        "outputsltdc",
                                        "saveCSV2"),
                   num.clusters = list(NULL,
                                       list(2:3)))

  testthat::expect_equal(nrow(read.csv(file = file.path("resourceFiles",
                                                        "outputsltdc",
                                                        "saveCSV2",
                                                        "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                       header = TRUE,
                                       sep = ";")),
                         4)

  strategy$saveCSV(dir.path = file.path("resourceFiles",
                                        "outputsltdc",
                                        "saveCSV3"),
                   num.clusters = list(list(2:3),
                                       NULL))

  testthat::expect_equal(nrow(read.csv(file = file.path("resourceFiles",
                                                        "outputsltdc",
                                                        "saveCSV3",
                                                        "ChiSquareHeuristic-SpearmanHeuristic.csv"),
                                       header = TRUE,
                                       sep = ";")),
                         4)
})

testthat::teardown({
  if (dir.exists(file.path("resourceFiles",
                           "outputsltdc"))) {
    unlink(x = file.path("resourceFiles",
                     "outputsltdc"),
           recursive = TRUE,
           force = TRUE)
  }

  if (file.exists(file.path("Rplots.pdf"))) {
    file.remove(file.path("Rplots.pdf"))
  }
})

testthat::test_that("DependencyBasedStrategy checks incompatible heuristics", {

  subset.cluster <- readRDS(file.path("resourceFiles",
                                      "data",
                                      "subset.rds"))

  configuration <- DependencyBasedStrategyConfiguration$new()

  strategyIncompatibleBinary <- DependencyBasedStrategy$new(subset = subset.cluster,
                                                            heuristic = list(KendallHeuristic$new(), NULL),
                                                            configuration = configuration)

  strategyIncompatibleReal <- DependencyBasedStrategy$new(subset = subset.cluster,
                                                          heuristic = list(NULL, OddsRatioHeuristic$new()),
                                                          configuration = configuration)

  testthat::expect_message(suppressWarnings(strategyIncompatibleBinary$execute()),
                           "[DependencyBasedStrategy][WARNING] 44 features were incompatible with 'KendallHeuristic' heuristic",
                           fixed = TRUE)

  testthat::expect_message(suppressWarnings(strategyIncompatibleReal$execute()),
                           "[DependencyBasedStrategy][WARNING] 50 features were incompatible with 'OddsRatioHeuristic' heuristic",
                           fixed = TRUE)
})

testthat::test_that("TypeBasedStrategy checks no binary or real features", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  subset.cluster.no.binary <- Subset$new(dataset = data.frame(c(0, 1, 0, 1),
                                                              c(0.2, 1.6, 5.12, 3.1),
                                                              c(0.1, 2.4, 6.89, 10.5)),
                                         class.index = 1,
                                         class.values = as.factor(c(0, 1)),
                                         positive.class = 1)

  strategyNoBinaryFeatures <- DependencyBasedStrategy$new(subset = subset.cluster.no.binary,
                                                          heuristic = list(ChiSquareHeuristic$new(),
                                                                           NULL),
                                                          configuration = configuration)

  testthat::expect_message(suppressWarnings(strategyNoBinaryFeatures$execute()),
                           "[DependencyBasedStrategy][INFO] Not binary features for clustering",
                           fixed = TRUE)

  subset.cluster.no.real <- Subset$new(dataset = data.frame(c(0, 1, 0, 1),
                                                            c(0, 1, 0, 1),
                                                            c(0, 1, 1, 1)),
                                       class.index = 1,
                                       class.values = as.factor(c(0, 1)),
                                       positive.class = 1)

  strategyNoRealFeatures <- DependencyBasedStrategy$new(subset = subset.cluster.no.real,
                                                        heuristic = list(NULL,
                                                                         SpearmanHeuristic$new()),
                                                        configuration = configuration)

  testthat::expect_message(suppressWarnings(strategyNoRealFeatures$execute()),
                           "[DependencyBasedStrategy][INFO] Not real features for clustering",
                           fixed = TRUE)
})
