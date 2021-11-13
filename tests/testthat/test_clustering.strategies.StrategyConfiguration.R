testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("StrategyConfiguration: minNumClusters function works", {

  configuration <- StrategyConfiguration$new()

  testthat::expect_equal(configuration$minNumClusters(), 2)
  testthat::expect_message(configuration$minNumClusters(),
                           "[StrategyConfiguration][minNumClusters][INFO] Using default minCluster configuration: 2 clusters minimun",
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

testthat::test_that("StrategyConfiguration: maxNumClusters function works", {

  configuration <- StrategyConfiguration$new()

  testthat::expect_equal(configuration$maxNumClusters(), 50)
  testthat::expect_message(configuration$maxNumClusters(),
                           "[StrategyConfiguration][maxNumClusters][INFO] Using default maxCluster configuration: 50 clusters maximun",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
