testthat::test_that("UseProbability: initialize", {

  testthat::expect_is(UseProbability$new(),
                      "UseProbability")
})
