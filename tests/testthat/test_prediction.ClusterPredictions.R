test_that("ClusterPredictions: initialize", {

  testthat::expect_is(ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                             positive.class = 1),
                      "ClusterPredictions")
})

test_that("ClusterPredictions: initialize checks parameter type", {

  testthat::expect_error(ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                                positive.class = 2),
                         "[ClusterPredictions][FATAL] Positive class not found. Should be 1 or 0. Aborting...",
                         fixed = TRUE)
})

# test_that("ClusterPredictions: add function works",{
#
#
#   clusterPrediction <- ClusterPredictions$new(class.values = c(1,0,1,1),
#                                               positive.class = 1)
#
#   prediction <- prediction_object
#
#   testthat::expect_invisible(clusterPrediction$add(prediction = prediction))
# })

test_that("ClusterPredictions: add function checks parameter type", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  prediction <- NULL

  testthat::expect_error(clusterPrediction$add(prediction = prediction),
                         "[ClusterPredictions][FATAL] Prediction parameter must be defined as 'Prediction' object. Aborting...",
                         fixed = TRUE)
})

# test_that("ClusterPredictions: get function works",{
#
#
#   clusterPrediction <- ClusterPredictions$new(class.values = c(1,0,1,1),
#                                               positive.class = 1)
#
#   position <- 1
#
#   testthat::expect_invisible(clusterPrediction$add(prediction = prediction))
# })

test_that("ClusterPredictions: get function checks parameter type", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  position <- NULL

  testthat::expect_error(clusterPrediction$get(position = 1),
                         "[ClusterPredictions][FATAL] Position exceeds list size. Aborting...",
                         fixed = TRUE)
})

test_that("ClusterPredictions: getAll function works", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  testthat::expect_is(clusterPrediction$getAll(), "list")
})

test_that("ClusterPredictions: size function works", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  testthat::expect_equal(clusterPrediction$size(), 0)
})

test_that("ClusterPredictions: getPositiveClass function works", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  testthat::expect_equal(clusterPrediction$getPositiveClass(), 1)
})

test_that("ClusterPredictions: getClassValues function works", {

  clusterPrediction <- ClusterPredictions$new(class.values = c(1, 0, 1, 1),
                                              positive.class = 1)

  testthat::expect_equal(clusterPrediction$getClassValues(), c(1, 0, 1, 1))
})
