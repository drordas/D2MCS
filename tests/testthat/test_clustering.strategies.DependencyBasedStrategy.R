test_that("DependencyBasedStrategy: initialize", {

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_is(DependencyBasedStrategy$new(subset = subset.cluster,
                                                  heuristic = heuristics,
                                                  configuration = configuration),
                      "DependencyBasedStrategy")

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][INFO] Heuristic for binary data defined",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][INFO] Heuristic for real data defined",
                           fixed = TRUE,
                           all = FALSE)
})

testthat::test_that("DependencyBasedStrategy: initialize checks parameter type", {

  subset.cluster <- NULL
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- NULL
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][FATAL] Heuristic parameter is not defined or incorrect. Must contain two elements. Aborting...",
                         fixed = TRUE, )

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(1, 1)
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][FATAL] Defined heuristics are not correct. Must be inherit from 'GenericHeuristic' class. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(NULL, SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][WARNING] Heuristic for binary data not defined",
                           fixed = TRUE,
                           all = FALSE)

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), NULL)
  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_message(DependencyBasedStrategy$new(subset = subset.cluster,
                                                       heuristic = heuristics,
                                                       configuration = configuration),
                           "[DependencyBasedStrategy][WARNING] Heuristic for real data not defined",
                           fixed = TRUE,
                           all = FALSE)

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- NULL

  testthat::expect_error(DependencyBasedStrategy$new(subset = subset.cluster,
                                                     heuristic = heuristics,
                                                     configuration = configuration),
                         "[DependencyBasedStrategy][FATAL] Configuration parameter must be inherit from 'StrategyConfiguration' class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("DependencyBasedStrategy: checks configuration object", {
  testthat::skip_if_not_installed("R6")
  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
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

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
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

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
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

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
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

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
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

testthat::test_that("DependencyBasedStrategy works", {

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- DependencyBasedStrategyConfiguration$new()

  strategy <- DependencyBasedStrategy$new(subset.cluster, heuristics, configuration)


  testthat::expect_error(strategy$getDistribution(),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)
  testthat::expect_error(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                         "[DependencyBasedStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  suppressWarnings(strategy$execute(verbose = TRUE))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()), 7)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1)), 0)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1:2)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.groups = 1)), 5)
  testthat::expect_equal(length(strategy$getDistribution(include.unclustered = TRUE)), 8)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[DependencyBasedStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_equal(c("gtable", "gTree", "grob", "gDesc"), class(strategy$plot()))

  testthat::expect_message(strategy$plot(dir.path = file.path("resourceFiles", "outputs", "plots"), file.name = "BinaryRealTypeStrategyPlot"),
                           "[DependencyBasedStrategy][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  unlink(file.path("resourceFiles", "outputs", "plots"), recursive = TRUE, force = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[DependencyBasedStrategy][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                           "[DependencyBasedStrategy][WARNING] File name not defined. Using 'ChiSquareHeuristic-SpearmanHeuristic.csv'",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                           "[DependencyBasedStrategy][WARNING] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV"),
                                            num.clusters = 2),
                           "[DependencyBasedStrategy][WARNING] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV"),
                                            num.clusters = list(2:60, 2:60)),
                           "[DependencyBasedStrategy][WARNING] Number of clusters incorrect. Must be between 2 and 2. Ignoring clustering for real type features...",
                           fixed = TRUE,
                           all = FALSE)

  unlink(file.path("resourceFiles", "outputs", "saveCSV"), recursive = TRUE, force = TRUE)

})
