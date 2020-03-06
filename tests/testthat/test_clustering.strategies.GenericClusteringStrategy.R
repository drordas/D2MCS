testthat::test_that("GenericClusteringStrategy: initialize", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_silent(GenericClusteringStrategy$new(subset = subset,
                                                 heuristic = heuristic,
                                                 description = description,
                                                 configuration = configuration))
})

testthat::test_that("GenericClusteringStrategy: initialize checks parameter type", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- NULL
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Strategy description parameter must be defined as 'character' type. Aborting...",
                         fixed = TRUE)

  subset <- NULL
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- NULL
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Heuristics is not correct (must inherit from 'GenericHeuristic' class). Aborting...",
                         fixed = TRUE)

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- list()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Adequate heuristics not found (must inherit from 'GenericHeuristic' class). Aborting...",
                         fixed = TRUE)

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- NULL

  testthat::expect_error(GenericClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[GenericClusteringStrategy][FATAL] Configuration parameter must be inherit from 'StrategyConfiguration' class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: getDescription function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getDescription(), description)
})

testthat::test_that("GenericClusteringStrategy: getHeuristic function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getHeuristic(), list(heuristic))
})

testthat::test_that("GenericClusteringStrategy: getConfiguration function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getConfiguration(), configuration)
})

testthat::test_that("GenericClusteringStrategy: getBestClusterDistribution function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_null(strategy$getBestClusterDistribution())
})

testthat::test_that("GenericClusteringStrategy: getUnclustered function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_null(strategy$getUnclustered())
})

testthat::test_that("GenericClusteringStrategy: execute function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$execute(verbose = TRUE),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: getDistribution function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$getDistribution(),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: createTrain function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$createTrain(subset = subset),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: plot function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$plot(),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericClusteringStrategy: saveCSV function works", {

  subset <- readRDS(file.path("resourceFiles", "data", "subset.rds"))

  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- GenericClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$saveCSV(dir.path = "example", "example"),
                         "[GenericClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})
