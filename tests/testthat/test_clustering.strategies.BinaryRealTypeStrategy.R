testthat::test_that("BinaryRealTypeStrategy: initialize", {

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- StrategyConfiguration$new()

  testthat::expect_is(BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration),
                      "BinaryRealTypeStrategy")
})

testthat::test_that("BinaryRealTypeStrategy: initialize checks parameter type", {

  subset.cluster <- NULL
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration),
                         "[BinaryRealTypeStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- NULL
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration),
                         "[BinaryRealTypeStrategy][FATAL] Heuristic parameter is not defined or incorrect. Must contain two elements. Aborting...",
                         fixed = TRUE, )

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(1, 1)
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration),
                         "[BinaryRealTypeStrategy][FATAL] Defined heuristics are not correct. Must be inherit from 'GenericHeuristic' class. Aborting...",
                         fixed = TRUE)

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- StrategyConfiguration$new()

  testthat::expect_message(BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration),
                           "[BinaryRealTypeStrategy][INFO] Heuristic for binary data defined",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration),
                           "[BinaryRealTypeStrategy][INFO] Heuristic for real data defined",
                           fixed = TRUE,
                           all = FALSE)

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(NULL, SpearmanHeuristic$new())
  configuration <- StrategyConfiguration$new()

  testthat::expect_message(BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration),
                           "[BinaryRealTypeStrategy][WARNING] Heuristic for binary data not defined",
                           fixed = TRUE,
                           all = FALSE)

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), NULL)
  configuration <- StrategyConfiguration$new()

  testthat::expect_message(BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration),
                           "[BinaryRealTypeStrategy][WARNING] Heuristic for real data not defined",
                           fixed = TRUE,
                           all = FALSE)
})

testthat::test_that("BinaryRealTypeStrategy works", {

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- StrategyConfiguration$new()

  strategy <- BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration)

  suppressWarnings(strategy$execute(verbose = TRUE))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  heuristics <- list(ChiSquareHeuristic$new(), SpearmanHeuristic$new())
  configuration <- StrategyConfiguration$new()

  strategyNoReal <- BinaryRealTypeStrategy$new(subset.cluster, list(ChiSquareHeuristic$new(), NULL), configuration)
  strategyNoBinary <- BinaryRealTypeStrategy$new(subset.cluster, list(NULL, SpearmanHeuristic$new()), configuration)

  testthat::expect_message(suppressWarnings(strategyNoReal$execute()),
                           "[BinaryRealTypeStrategy][INFO] BinaryRealTypeStrategy has not heuristic to real features. Assuming one cluster by default",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_equal(c("gg", "ggplot"), class(strategyNoReal$plot()))

  testthat::expect_message(suppressWarnings(strategyNoBinary$execute()),
                           "[BinaryRealTypeStrategy][INFO] BinaryRealTypeStrategy has not heuristic to binary features. Assuming one cluster by default",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_equal(c("gg", "ggplot"), class(strategyNoBinary$plot()))

  strategy <- BinaryRealTypeStrategy$new(subset.cluster, heuristics, configuration)

  testthat::expect_error(strategy$getDistribution(),
                         "[BinaryRealTypeStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[BinaryRealTypeStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)
  testthat::expect_error(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                         "[BinaryRealTypeStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  suppressWarnings(strategy$execute(verbose = TRUE))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()), 7)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 2)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1:2)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.groups = 1)), 4)
  testthat::expect_equal(length(strategy$getDistribution(include.unclustered = TRUE)), 8)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[BinaryRealTypeStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_equal(c("gtable", "gTree", "grob", "gDesc"), class(strategy$plot()))

  testthat::expect_message(strategy$plot(dir.path = file.path("resourceFiles", "outputs", "plots"), file.name = "BinaryRealTypeStrategyPlot"),
                           "[BinaryRealTypeStrategy][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  unlink(file.path("resourceFiles", "outputs", "plots"), recursive = TRUE, force = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[BinaryRealTypeStrategy][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                           "[BinaryRealTypeStrategy][WARNING] File name not defined. Using 'ChiSquareHeuristic-SpearmanHeuristic.csv'",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                           "[BinaryRealTypeStrategy][WARNING] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV"),
                                            num.clusters = 2),
                           "[BinaryRealTypeStrategy][WARNING] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV"),
                                            num.clusters = list(2:60, 2:60)),
                           "[BinaryRealTypeStrategy][WARNING] Number of clusters incorrect. Must be between 2 and 50. Ignoring clustering for real type features...",
                           fixed = TRUE,
                           all = FALSE)

  unlink(file.path("resourceFiles", "outputs", "saveCSV"), recursive = TRUE, force = TRUE)
})
