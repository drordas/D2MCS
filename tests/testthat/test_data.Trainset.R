testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Trainset: initialize function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  testthat::expect_is(Trainset$new(cluster.dist = list(corpus[1:49]),
                                   class.name = "Class",
                                   class.values = factor(corpus[[50]]),
                                   positive.class = 1),
                      "Trainset")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Trainset: initialize function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  testthat::expect_error(Trainset$new(cluster.dist = NULL,
                                      class.name = "Class",
                                      class.values = factor(corpus[[50]]),
                                      positive.class = 1),
                         "[Trainset][initialize][FATAL] Clusters empty or incorrect (must be a list). Aborting...",
                         fixed = TRUE)

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  testthat::expect_error(Trainset$new(cluster.dist = list(corpus[1:49]),
                                      class.name = "Class",
                                      class.values = corpus[[50]],
                                      positive.class = 2),
                         "[Trainset][initialize][FATAL] Class.values parameter must be defined as 'factor' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Trainset$new(cluster.dist = list(corpus[1:49]),
                                      class.name = "Class",
                                      class.values = factor(corpus[[50]]),
                                      positive.class = 2),
                         "[Trainset][initialize][FATAL] Positive Class parameter is incorrect. Must be '0' '1'. Aborting...",
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

testthat::test_that("Trainset: getPositiveClass function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                         class.name = "Class",
                         class.values = factor(corpus[[50]]),
                         positive.class = 1)

  testthat::expect_equal(trainset$getPositiveClass(), 1)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Trainset: getClassName function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  testthat::expect_equal(trainset$getClassName(), "Class")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Trainset: getClassValues function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  testthat::expect_equal(trainset$getClassValues(), factor(corpus[[50]]))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Trainset: getColumnNames function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  testthat::expect_equal(trainset$getColumnNames(1), names(corpus)[1:49])
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Trainset: getColumnNames function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  testthat::expect_error(trainset$getColumnNames(-50),
                         "[Trainset][getColumnNames][FATAL] Position not defined or incorrect. Must be included between 1 and 1. Aborting...",
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

testthat::test_that("Trainset: getFeatureValues function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  testthat::expect_equal(trainset$getFeatureValues(1), corpus[1:49])
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Trainset: getFeatureValues function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  testthat::expect_error(trainset$getFeatureValues(-50),
                         "[Trainset][getFeatureValues][FATAL] Position not defined or incorrect. Must be included between 1 and 1. Aborting...",
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

testthat::test_that("Trainset: getInstances function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  corpus[[50]] <- factor(corpus[[50]])

  testthat::expect_equal(trainset$getInstances(1), corpus)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("Trainset: getInstances function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  testthat::expect_error(trainset$getInstances(-50),
                         "[Trainset][getInstances][FATAL] Position not defined or incorrect. Must be included between 1 and 1. Aborting...",
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

testthat::test_that("Trainset: getNumClusters function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE, )

  names(corpus) <-  unlist(strsplit(scan(file = file.path, nlines = 1,
                                         what = "character", quiet = TRUE),
                                    split = ","))

  trainset <- Trainset$new(cluster.dist = list(corpus[1:49]),
                           class.name = "Class",
                           class.values = factor(corpus[[50]]),
                           positive.class = 1)

  testthat::expect_equal(trainset$getNumClusters(), 1)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
