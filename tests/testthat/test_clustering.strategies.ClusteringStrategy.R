testthat::test_that("ClusteringStrategy: initialize", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_silent(ClusteringStrategy$new(subset = subset,
                                                 heuristic = heuristic,
                                                 description = description,
                                                 configuration = configuration))
})

testthat::test_that("ClusteringStrategy: initialize checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- NULL
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(ClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[ClusteringStrategy][FATAL] Strategy description parameter must be defined as 'character' type. Aborting...",
                         fixed = TRUE)

  subset <- NULL
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(ClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[ClusteringStrategy][FATAL] Subset parameter must be defined as 'Subset' type. Aborting...",
                         fixed = TRUE)

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- NULL
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(ClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[ClusteringStrategy][FATAL] Heuristics is not correct (must inherit from 'GenericHeuristic' class). Aborting...",
                         fixed = TRUE)

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- list()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  testthat::expect_error(ClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[ClusteringStrategy][FATAL] Adequate heuristics not found (must inherit from 'GenericHeuristic' class). Aborting...",
                         fixed = TRUE)

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- NULL

  testthat::expect_error(ClusteringStrategy$new(subset = subset,
                                                heuristic = heuristic,
                                                description = description,
                                                configuration = configuration),
                         "[ClusteringStrategy][FATAL] Configuration parameter must be inherit from 'StrategyConfiguration' class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClusteringStrategy: getDescription function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getDescription(), description)
})

testthat::test_that("ClusteringStrategy: getHeuristic function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getHeuristic(), list(heuristic))
})

testthat::test_that("ClusteringStrategy: getConfiguration function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_equal(strategy$getConfiguration(), configuration)
})

testthat::test_that("ClusteringStrategy: getBestClusterDistribution function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_null(strategy$getBestClusterDistribution())
})

testthat::test_that("ClusteringStrategy: getUnclustered function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_null(strategy$getUnclustered())
})

testthat::test_that("ClusteringStrategy: execute function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$execute(verbose = TRUE),
                         "[ClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClusteringStrategy: getDistribution function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$getDistribution(),
                         "[ClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClusteringStrategy: createTrain function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$createTrain(subset = subset),
                         "[ClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClusteringStrategy: plot function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$plot(),
                         "[ClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClusteringStrategy: saveCSV function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  subset <- Subset$new(dataset = corpus,
                       class.index = 50,
                       class.values = c(1, 0),
                       positive.class = 1)
  heuristic <- MCCHeuristic$new()
  description <- "example"
  configuration <- StrategyConfiguration$new()

  strategy <- ClusteringStrategy$new(subset = subset,
                                     heuristic = heuristic,
                                     description = description,
                                     configuration = configuration)

  testthat::expect_error(strategy$saveCSV(dir.path = "example", "example"),
                         "[ClusteringStrategy][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})
