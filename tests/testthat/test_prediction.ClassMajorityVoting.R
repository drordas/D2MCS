testthat::test_that("ClassMajorityVoting: initialize", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_is(ClassMajorityVoting$new(cutoff = cutoff,
                                              class.tie = class.tie,
                                              majority.class = majority.class),
                      "ClassMajorityVoting")
})

testthat::test_that("ClassMajorityVoting: initialize checks parameter type", {

  cutoff <- 0.5
  class.tie <- 1
  majority.class <- "Positive"

  testthat::expect_error(ClassMajorityVoting$new(cutoff = cutoff,
                                                 class.tie = class.tie,
                                                 majority.class = majority.class),
                         "[ClassMajorityVoting][FATAL] Invalid class tie value. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("ClassMajorityVoting: getMajorityClass", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_equal(ClassMajorityVoting$new(cutoff = cutoff,
                                                 class.tie = class.tie,
                                                 majority.class = majority.class)$getMajorityClass(),
                         "Positive")
})

testthat::test_that("ClassMajorityVoting: getClassTie", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  testthat::expect_equal(ClassMajorityVoting$new(cutoff = cutoff,
                                                 class.tie = class.tie,
                                                 majority.class = majority.class)$getClassTie(),
                         "Positive")
})

testthat::test_that("ClassMajorityVoting: execute function checks parameter type", {

  cutoff <- 0.5
  class.tie <- "Positive"
  majority.class <- "Positive"

  voting <- ClassMajorityVoting$new(cutoff = cutoff,
                                    class.tie = class.tie,
                                    majority.class = majority.class)

  testthat::expect_error(voting$execute(predictions = NULL,
                                        verbose = FALSE),
                         "[ClassMajorityVoting][FATAL] Predictions parameter must be defined as 'ClusterPrediction' type. Aborting...",
                         fixed = TRUE)

  predictions  <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                         positive.class = 1)
  testthat::expect_error(voting$execute(predictions = predictions,
                                        verbose = FALSE),
                         "[ClassMajorityVoting][FATAL] Cluster predictions were not computed. Aborting...",
                         fixed = TRUE)
})
