testthat::test_that("DependencyBasedStrategyConfiguration: minNumClusters function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_type(configuration$minNumClusters(), "double")
})

testthat::test_that("DependencyBasedStrategyConfiguration: maxNumClusters function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_equal(configuration$maxNumClusters(features = list(list(1, 2), list(1))), 2)
  testthat::expect_equivalent(suppressWarnings(configuration$maxNumClusters()), 3)

})

testthat::test_that("DependencyBasedStrategyConfiguration: getBinaryCutoff function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_type(configuration$getBinaryCutoff(), "double")
})

testthat::test_that("DependencyBasedStrategyConfiguration: getRealCutoff function works", {

  configuration <- DependencyBasedStrategyConfiguration$new()

  testthat::expect_type(configuration$getRealCutoff(), "double")
})
