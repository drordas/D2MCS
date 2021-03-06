testthat::test_that("SimpleStrategy: initialize", {

  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristic <- ChiSquareHeuristic$new()
  configuration <- StrategyConfiguration$new()

  testthat::expect_is(SimpleStrategy$new(subset.cluster, heuristic, configuration),
                      "SimpleStrategy")
})

testthat::setup({
  if (!dir.exists(file.path("resourceFiles", "outputs"))) {
    dir.create(file.path("resourceFiles", "outputs"))
  }
})

testthat::test_that("SimpleStrategy works", {
  testthat::skip_if_not_installed("grDevices")
  subset.cluster <- readRDS(file.path("resourceFiles", "data", "subset.rds"))
  heuristic <- ChiSquareHeuristic$new()
  configuration <- StrategyConfiguration$new()

  strategy <- SimpleStrategy$new(subset.cluster, heuristic, configuration)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")

  heuristic <- MCCHeuristic$new()
  configuration <- StrategyConfiguration$new()

  strategy <- SimpleStrategy$new(subset.cluster, heuristic, configuration)

  testthat::expect_error(strategy$getDistribution(),
                         "[SimpleStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(strategy$createTrain(subset = subset.cluster),
                         "[SimpleStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)
  testthat::expect_error(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                         "[SimpleStrategy][FATAL] Clustering not done or errorneous. Aborting...",
                         fixed = TRUE)

  capture.output(suppressWarnings(strategy$execute(verbose = TRUE)))

  testthat::expect_is(strategy$getBestClusterDistribution(), "list")
  testthat::expect_is(strategy$getUnclustered(), "list")

  testthat::expect_equal(length(strategy$getDistribution()), 5)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 1)), 5)
  testthat::expect_equal(length(strategy$getDistribution(num.clusters = 2)), 2)
  testthat::expect_equal(length(strategy$getDistribution(num.groups = 1)), 1)
  testthat::expect_equal(length(strategy$getDistribution(include.unclustered = TRUE)), 6)

  testthat::expect_is(strategy$createTrain(subset = subset.cluster),
                      "Trainset")
  testthat::expect_error(strategy$createTrain(subset = NULL),
                         "[SimpleStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  grDevices::pdf(NULL)

  testthat::expect_equal(c("gg", "ggplot"),
                         class(strategy$plot(dir.path = NULL,
                                             file.name = NULL)))

  testthat::expect_message(strategy$plot(dir.path = file.path("resourceFiles", "outputs", "plots"),
                                         file.name = "SimpleGenericClusteringStrategyPlot"),
                           "[SimpleStrategy][INFO] Plot has been succesfully saved at",
                           fixed = TRUE)

  testthat::expect_error(strategy$saveCSV(dir.path = NULL),
                         "[SimpleStrategy][FATAL] Path not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                           "[SimpleStrategy][WARNING] File name not defined. Using 'MCCHeuristic.csv'",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV")),
                           "[SimpleStrategy][WARNING] Number of clusters not defined. Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV"),
                                            num.clusters = 2),
                           "[SimpleStrategy][WARNING] Type of num.clusters not valid (must be NULL or list type). Saving all cluster configurations",
                           fixed = TRUE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV"),
                                            num.clusters = list(2:60)),
                           "[SimpleStrategy][WARNING] Number of clusters exceeds maximum number of clusters. Saving all cluster configurations",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(strategy$saveCSV(dir.path = file.path("resourceFiles", "outputs", "saveCSV"),
                                            num.clusters = list(0:3)),
                           "[SimpleStrategy][WARNING] Number of clusters exceeds the range of minimum and maximum number of clusters. Saving all cluster configurations",
                           fixed = TRUE,
                           all = FALSE)
})

testthat::teardown({
  if (dir.exists(file.path("resourceFiles", "outputs"))) {
    unlink(file.path("resourceFiles", "outputs"), recursive = TRUE, force = TRUE)
  }
})
